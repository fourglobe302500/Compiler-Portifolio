module Compiler.Lexer

open System
open Compiler

type internal StateType =
  | Blank
  | Int
  | Float
  | String of bool
  | Char of bool
  | Identifier
  | WhiteSpace
  | MultOperator of Punctuation

type internal State =
    { State: StateType
      Text: string
      Position: int
      Diagnostics: string list
      Tokens: Token list }

let lex input =
  let toState (s, t, p, d, tokens) = { State = s; Text = t; Position = p; Diagnostics = d; Tokens = tokens }
  let getPos off (text: string) = off - text.Length
  let stoken token pos text = { Token = token; Position = getPos pos text; Text = Value text }
  let dispatch (nState: StateType) (nText: string) oState oText position diagnostics tokens : State =
    match oState with
    | WhiteSpace ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken (SyntaxToken.WhiteSpace oText) position oText])
    | Int ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken ((SyntaxToken.IntegerLiteral << Literal << Value) <| int oText) position oText])
    | Float ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken ((SyntaxToken.FloatLiteral << Literal << Value) <| float oText) position oText])
    | String _ ->
        toState (nState, nText, position + 1, diagnostics, tokens@[{Token = (SyntaxToken.StringLiteral << Literal << Value) oText; Position = getPos position oText - 1; Text = Value <| sprintf "\"%s\"" oText}])
    | Char _ ->
      if oText.Length <> 1 then
        toState (nState, nText, position + 1, diagnostics@[sprintf "Invallid char literal at %i to %i" (position - oText.Length - 1) position], tokens)
      else
        toState (nState, nText, position + 1, diagnostics, tokens@[{Token = (SyntaxToken.CharLiteral << Literal << Value) <| char oText; Position = getPos position oText - 1; Text = Value <| sprintf "'%s'" oText}])
    | Identifier ->
      match oText with
      | Keyword.Keyword k ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken (SyntaxToken.Keyword k) position oText])
      | "true" ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken ((SyntaxToken.BooleanLiteral << Literal << Value) true) position oText])
      | "false" ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken ((SyntaxToken.BooleanLiteral << Literal << Value) false) position oText])
      | _ ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken (((SyntaxToken.Identifier << Value) oText)) position oText])
    | MultOperator pun ->
        toState (nState, nText, position + 1, diagnostics, tokens@[stoken (((SyntaxToken.Operator << Value) pun)) position oText])
    | _ -> toState (nState, nText, position + 1, diagnostics, tokens)

  let dispatchop (oState) ofun f : State =
    match oState with
    | MultOperator op -> ofun op
    | _ -> f ()

  let inner (state) char: State =
    let dispatchop ofun f = dispatchop state.State ofun f
    let dispatch ns nt = dispatch ns nt state.State state.Text state.Position state.Diagnostics state.Tokens
    let dispatchmo nop = dispatch (MultOperator nop) (Punctuation.getText nop)
    let convertTo ns: State = toState (MultOperator ns, Punctuation.getText ns, state.Position + 1, state.Diagnostics, state.Tokens)
    let escape is ch =
      match is, ch with
      | true, 'n' -> "\n"
      | true, 't' -> "\t"
      | true, 'r' -> "\r"
      | _, c -> string c
    match char with
    | c when Char.IsLetter c ->
      match state.State with
      | Identifier -> toState (Identifier, state.Text + string c, state.Position + 1, state.Diagnostics, state.Tokens)
      | String is -> toState (String false, state.Text + escape is c, state.Position + 1, state.Diagnostics, state.Tokens)
      | Char is -> toState (Char false, state.Text + escape is c, state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch Identifier (string c)
    | n when Char.IsDigit n ->
      match state.State with
      | Int -> toState (Int, state.Text + string n, state.Position + 1, state.Diagnostics, state.Tokens)
      | Float -> toState (Float, state.Text + string n, state.Position + 1, state.Diagnostics, state.Tokens)
      | String is -> toState (String false, state.Text + escape is n, state.Position + 1, state.Diagnostics, state.Tokens)
      | Char is -> toState (Char false, state.Text + escape is n, state.Position + 1, state.Diagnostics, state.Tokens)
      | MultOperator op ->
        match op with
        | Dot -> toState (Float, state.Text + string n, state.Position + 1, state.Diagnostics, state.Tokens)
        | _ -> dispatch Int (string n)
      | Identifier -> toState (Identifier, state.Text + string n, state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch Int (string n)
    | w when Char.IsWhiteSpace w ->
      match state.State with
      | WhiteSpace -> toState (WhiteSpace, state.Text + string w, state.Position + 1, state.Diagnostics, state.Tokens)
      | String is -> toState (String false, state.Text + escape is w, state.Position + 1, state.Diagnostics, state.Tokens)
      | Char is -> toState (Char false, state.Text + escape is w, state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch WhiteSpace (string w)
    | '_' ->
      match state.State with
      | Identifier -> toState (Identifier, state.Text + "_", state.Position + 1, state.Diagnostics, state.Tokens)
      | _ ->
        toState (Blank, "", state.Position + 1, state.Diagnostics, state.Tokens@[stoken ((SyntaxToken.InvallidCharacter << Value) '_') state.Position state.Text])
    | '+' ->
      let f () = dispatchmo Plus
      dispatchop (function | Plus -> convertTo PlusPlus | _ -> f()) f
    | '-' ->
      let f () = dispatchmo Minus
      dispatchop (function | Minus -> convertTo MinusMinus | _ -> f()) f
    | '*' -> dispatchmo Star
    | '/' -> dispatchmo Slash
    | '%' -> dispatchmo Modulos
    | ',' -> dispatchmo Comma
    | '|' ->
      let f () = dispatchmo Pipe
      dispatchop (function | Pipe -> convertTo PipePipe | OpenParen -> convertTo OpenParenPipe | _ -> f()) f
    | '&' ->
      let f () = dispatchmo Ampersant
      dispatchop (function | Ampersant -> convertTo AmpersantAmpersant | _ -> f()) f
    | '!' -> dispatchmo Bang
    | '^' -> dispatchmo Hat
    | '~' -> dispatchmo Tilde
    | '<' -> dispatchmo Lesser
    | '>' -> dispatchmo Greater
    | '?' -> dispatchmo QuestionMark
    | ':' -> dispatchmo Colon
    | '"' ->
      match state.State with
      | String false -> dispatch Blank ""
      | String true -> toState (String false, state.Text + "\"", state.Position + 1, state.Diagnostics, state.Tokens)
      | Char _ -> toState (Char false, state.Text + "\"", state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch (String false) ""
    | '\\' ->
      match state.State with
      | String false -> toState (String true, state.Text, state.Position + 1, state.Diagnostics, state.Tokens)
      | String true -> toState (String false, state.Text + "\\", state.Position + 1, state.Diagnostics, state.Tokens)
      | Char false -> toState (Char true, state.Text, state.Position + 1, state.Diagnostics, state.Tokens)
      | Char true -> toState (Char false, state.Text + "\\", state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch (String false) ""
    | ''' ->
      match state.State with
      | Char false -> dispatch Blank ""
      | Char true -> toState (Char false, state.Text + "'", state.Position + 1, state.Diagnostics, state.Tokens)
      | String _ -> toState (String false, state.Text + "'", state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatch (Char false) ""
    | '(' -> dispatchmo OpenParen
    | ')' ->
      let f () = dispatchmo CloseParen
      dispatchop (function | Pipe -> convertTo PipeCloseParen | _ -> f()) f
    | '{' -> dispatchmo OpenBrace
    | '}' -> dispatchmo CloseBrace
    | '[' -> dispatchmo OpenSquareBrace
    | ']' -> dispatchmo CloseSquareBrace
    | '.' ->
      match state.State with
      | Int -> toState (Float, state.Text + ".", state.Position + 1, state.Diagnostics, state.Tokens)
      | _ -> dispatchmo Dot
    | '=' ->
      let f () = dispatchmo Equals
      dispatchop
        (function
          | Equals -> convertTo EqualsEquals
          | Plus -> convertTo PlusEquals
          | Minus -> convertTo MinusEquals
          | Star -> convertTo StarEquals
          | Slash -> convertTo SlashEquals
          | Pipe -> convertTo PipeEquals
          | Ampersant -> convertTo AmpersantEquals
          | Lesser -> convertTo LesserEquals
          | Greater -> convertTo GreaterEquals
          | _ -> f())
        f
    | _ ->
      let ns = dispatch Blank ""
      toState (Blank, "", ns.Position + 1, ns.Diagnostics@[sprintf "Invallid character '%c' at %i" char ns.Position], ns.Tokens@[stoken ((InvallidCharacter << Value) char) ns.Position (string char)])

  input
  |> Seq.toList
  |> List.fold inner { State = Blank; Text = ""; Position = 0; Diagnostics = []; Tokens = [] }
  |> fun res ->
    match res.State with
    | String _ ->
      let ns = dispatch Blank "" res.State res.Text res.Position res.Diagnostics res.Tokens
      Failure (ns.Diagnostics@[sprintf "Not finished string literal at %i to %i" (res.Position - 1 - res.Text.Length) (res.Position - 1)])
    | Char _ ->
      let ns = dispatch Blank "" res.State res.Text res.Position res.Diagnostics res.Tokens
      Failure (ns.Diagnostics@[sprintf "Not finished char literal at %i to %i" (res.Position - 1 - res.Text.Length) (res.Position - 1)])
    | _ ->
      let ns = dispatch Blank "" res.State res.Text res.Position res.Diagnostics res.Tokens
      match ns.Diagnostics with
      | [] -> Success (ns.Tokens@[stoken EndOfFile ns.Position "\000"])
      | diag -> Failure diag
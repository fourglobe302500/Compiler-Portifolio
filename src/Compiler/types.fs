namespace Compiler

type Result<'S,'F> =
| Success of 'S
| Failure of 'F

[<AutoOpen>]
module Result =
  let bind f x =
    match x with
    | Success s -> f s
    | Failure x -> Failure x

  let (|>>) x f = bind f x

type Punctuation =
  // Matematical Operators
| Plus                // Unary (pre) identity | Binary adition
| Minus               // Unary (pre) negation | Binary subtraction
| Star                // Binary multiplication
| Slash               // Binary division
| Modulos             // Binary module
| Comma               // Binary ignore | Simple separator
| QuestionMark        // Binary is true

// in between
| OpenParenPipe       // Absolute
| PipeCloseParen      // Absolute
| OpenParen           // Preference
| CloseParen          // Preference
| OpenBrace           // Scope
| CloseBrace          // Scope
| OpenSquareBrace     // Access index
| CloseSquareBrace    // Access index
| Colon               // Possibilty separetor

// in Middle
| Dot                 // Access propertie

  // Boolean Operators
| PipePipe            // Binary or
| AmpersantAmpersant  // Binary and
| Bang                // Unary (pre) not

  // Bitwise Operators
| Pipe                // Binary or
| Ampersant           // Binary and
| Hat                 // Binary xor
| Tilde               // Unary (pre) not

  // Equality Operators
| EqualsEquals        // Binary equals
| BangEquals          // Binary notEquals
| Lesser              // Binary lesserThan
| Greater             // Binary greaterThan
| LesserEquals        // Binary notGreaterThan
| GreaterEquals       // Binary notLesserThan

// Assigments Operators
| PlusPlus            // Unary (pre|pos) plus one assigment
| MinusMinus          // Unary (pre|pos) minus one assigment
| Equals              // Binary simple assigment
| PlusEquals          // Binary add assigment
| MinusEquals         // Binary subtract assigment
| StarEquals          // Binary multiply assigment
| SlashEquals         // Binary divide assigment
| ModulosEquals       // Binary module assigment
| PipeEquals          // Binary or assigment
| AmpersantEquals     // Binary and assigment

type Value<'t> = Value of 't

type Literal<'t> = Literal of 't

type Keyword =
| Not

type LiteralValue =
| Identifier of string
| Boolean of bool
| Integer of int
| Float of float
| Char of char
| String of string

type SyntaxToken =
| Identifier          of Value: Value<string>
| Keyword             of Keyword
| IntegerLiteral      of Value: Literal<Value<int>>
| FloatLiteral        of Value: Literal<Value<float>>
| BooleanLiteral      of Value: Literal<Value<bool>>
| StringLiteral       of Value: Literal<Value<string>>
| CharLiteral         of Value: Literal<Value<char>>
| Operator            of Value: Value<Punctuation>
| InvallidCharacter   of Character: Value<char>
| WhiteSpace          of Spaces: string
| EndOfFile

type Token =
  { Token: SyntaxToken
    Position: int
    Text: Value<string> }

type Expression =
  private | LiteralExpression       of Literal: Value<Token>
          | IdentifierExpression    of Identifier: Value<Token>
          | PrefixUnaryExpression   of Operator: Value<Token> * Expression
          | PosfixUnaryExpression   of Expression * Operator: Value<Token>
          | BinaryExpression        of Left: Expression * Operator: Value<Token> * Right: Expression
          | InBetweenExpression     of OpenOperator: Value<Token> * Expression * CloseOperator: Value<Token>
          | TernaryExpression       of Condition: Expression * ConditionalOperator: Value<Token> * IsTrue: Expression * SeparationOperator: Value<Token> * IsFalse: Expression

[<RequireQualifiedAccess>]
module Punctuation =
  let getText = function
    | Plus                -> "+"
    | Minus               -> "-"
    | Star                -> "*"
    | Slash               -> "/"
    | Modulos             -> "%"
    | Comma               -> ","
    | QuestionMark        -> "?"
    | OpenParenPipe       -> "(|"
    | PipeCloseParen      -> "|)"
    | OpenParen           -> "("
    | CloseParen          -> ")"
    | OpenBrace           -> "["
    | CloseBrace          -> "]"
    | OpenSquareBrace     -> "{"
    | CloseSquareBrace    -> "}"
    | Colon               -> ":"
    | Dot                 -> "."
    | PipePipe            -> "||"
    | AmpersantAmpersant  -> "&&"
    | Bang                -> "!"
    | Pipe                -> "|"
    | Ampersant           -> "&"
    | Hat                 -> "^"
    | Tilde               -> "~"
    | EqualsEquals        -> "=="
    | BangEquals          -> "!="
    | Lesser              -> "<"
    | Greater             -> ">"
    | LesserEquals        -> "<="
    | GreaterEquals       -> ">="
    | PlusPlus            -> "++"
    | MinusMinus          -> "--"
    | Equals              -> "="
    | PlusEquals          -> "+="
    | MinusEquals         -> "-="
    | StarEquals          -> "*="
    | SlashEquals         -> "/="
    | ModulosEquals       -> "%="
    | PipeEquals          -> "|="
    | AmpersantEquals     -> "&="

  let (|IsOpener|IsCloser|IsBinary|IsNothing|) op =
    match op with
    | OpenParenPipe       -> IsOpener PipeCloseParen
    | OpenParen           -> IsOpener CloseParen
    | OpenBrace           -> IsOpener CloseBrace
    | OpenSquareBrace     -> IsOpener CloseSquareBrace
    | PipeCloseParen      -> IsCloser PipeCloseParen
    | CloseParen          -> IsCloser CloseParen
    | CloseBrace          -> IsCloser CloseBrace
    | CloseSquareBrace    -> IsCloser CloseSquareBrace
    | QuestionMark        -> IsNothing
    | Colon               -> IsNothing
    | Bang                -> IsNothing
    | Tilde               -> IsNothing
    | PlusPlus            -> IsNothing
    | MinusMinus          -> IsNothing
    | Dot                 -> IsNothing
    | Plus                -> IsBinary 6
    | Minus               -> IsBinary 6
    | Star                -> IsBinary 7
    | Slash               -> IsBinary 7
    | Modulos             -> IsBinary 7
    | Comma               -> IsBinary 1
    | AmpersantAmpersant  -> IsBinary 5
    | PipePipe            -> IsBinary 4
    | Pipe                -> IsBinary 4
    | Ampersant           -> IsBinary 4
    | Hat                 -> IsBinary 4
    | EqualsEquals        -> IsBinary 3
    | BangEquals          -> IsBinary 3
    | Lesser              -> IsBinary 3
    | Greater             -> IsBinary 3
    | LesserEquals        -> IsBinary 3
    | GreaterEquals       -> IsBinary 3
    | Equals              -> IsBinary 2
    | PlusEquals          -> IsBinary 2
    | MinusEquals         -> IsBinary 2
    | StarEquals          -> IsBinary 2
    | SlashEquals         -> IsBinary 2
    | ModulosEquals       -> IsBinary 2
    | PipeEquals          -> IsBinary 2
    | AmpersantEquals     -> IsBinary 2

  let (|IsUnary|_|) = function
    | Bang                -> Some false
    | Tilde               -> Some false
    | PlusPlus            -> Some true
    | MinusMinus          -> Some true
    | Plus                -> Some false
    | Minus               -> Some false
    | _ -> None

  let getPrecedence = function
  | IsBinary p -> p
  | _ -> 0

[<RequireQualifiedAccess>]
module Value =
  let get (Value v) = v

[<RequireQualifiedAccess>]
module Keyword =
  let (|Keyword|_|) text = None

[<RequireQualifiedAccess>]
module SyntaxToken =
  let getText = function
  | SyntaxToken.Identifier (Value i)      -> sprintf "Identifier '%s'"            i
  | Keyword k                             -> sprintf "Keyword: '%O'"              k
  | IntegerLiteral (Literal (Value i))    -> sprintf "Integer Literal: %i"        i
  | FloatLiteral (Literal (Value f))      -> sprintf "Float Literal: %4f"         f
  | BooleanLiteral (Literal (Value b))    -> sprintf "Boolean Literal: %b"        b
  | StringLiteral (Literal (Value s))     -> sprintf "String Literal: \"%s\""     (s.Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r"))
  | CharLiteral (Literal (Value c))       -> sprintf "Char Literal: '%c'"         c
  | InvallidCharacter (Value i)           -> sprintf "Invallid Character: '%c'"   i
  | WhiteSpace w                          -> sprintf "White Space: '%s'"          (w.Replace("\n", "\\n").Replace("\t", "\\t").Replace("\r", "\\r"))
  | Operator (Value op)                   -> sprintf "Operator: '%s'"             (Punctuation.getText op)
  | EndOfFile                             -> sprintf "End Of File"

  let (|IsValue|IsWhitespace|IsEndOfFile|IsInvallid|IsOperator|IsKeyword|) = function
  | SyntaxToken.Identifier (Value i)      -> IsValue (LiteralValue.Identifier i)
  | Keyword k                             -> IsKeyword k
  | IntegerLiteral (Literal (Value i))    -> IsValue (Integer i)
  | FloatLiteral (Literal (Value f))      -> IsValue (Float f)
  | BooleanLiteral (Literal (Value b))    -> IsValue (Boolean b)
  | StringLiteral (Literal (Value s))     -> IsValue (String s)
  | CharLiteral (Literal (Value c))       -> IsValue (Char c)
  | InvallidCharacter (Value i)           -> IsInvallid i
  | WhiteSpace w                          -> IsWhitespace w
  | Operator (Value op)                   -> IsOperator op
  | EndOfFile                             -> IsEndOfFile

open System.CodeDom.Compiler

[<RequireQualifiedAccess>]
module Token =
  let show (writer: IndentedTextWriter) token =
    writer.WriteLine("{0} (raw: '{1}', start at: {2}, length: {3}, end at: {4})",
      (SyntaxToken.getText token.Token),
      (Value.get token.Text),
      token.Position,
      ((Value.get token.Text).Length),
      (token.Position + (Value.get token.Text).Length))

[<RequireQualifiedAccess>]
module Expression =
  let createLiteral token =
    match token.Token with
    | IntegerLiteral _ | FloatLiteral _ | StringLiteral _
    | CharLiteral _ | BooleanLiteral _ -> Some <| LiteralExpression (Value token)
    | _ -> None

  let createIdentifier token =
    match token.Token with
    | Identifier _ -> Some <| IdentifierExpression (Value token)
    | _ -> None

  let createPrefixUnary token expr =
    match token.Token with
    | Operator (Value op) ->
      match op with
      | Plus | Minus | PlusPlus | MinusMinus | Bang | Tilde -> Some <| PrefixUnaryExpression (Value token, expr)
      | _ -> None
    | _ -> None

  let createPosfixUnary token expr =
    match token.Token with
    | Operator (Value op) ->
      match op with
      | PlusPlus | MinusMinus -> Some <| PosfixUnaryExpression (expr, Value token)
      | _ -> None
    | _ -> None

  let createBinary token left right =
    match token.Token with
    | Operator (Value op) ->
      match op with
      | Plus | Minus | Star | Slash | Modulos | Comma
      | PipePipe | AmpersantAmpersant | Pipe | Equals
      | Ampersant | Hat | EqualsEquals | PlusEquals
      | BangEquals | Lesser | Greater | PipeEquals
      | LesserEquals | GreaterEquals | MinusEquals
      | StarEquals | SlashEquals| ModulosEquals
      | AmpersantEquals -> Some <| BinaryExpression (left, Value token, right)
      | _ -> None
    | _ -> None

  let createTernary conditionalToken separatorToken cond isTrue isFalse =
    match conditionalToken.Token, separatorToken.Token with
    | (Operator (Value cOp)), (Operator (Value sOp)) ->
      match cOp, sOp with
      | QuestionMark, Colon -> Some <| TernaryExpression (cond, Value conditionalToken, isTrue, Value separatorToken, isFalse)
      | _ -> None
    | _ -> None

  let createInBetween openToken expr closeToken =
    match openToken.Token, closeToken.Token with
    | (Operator (Value oOp)), (Operator (Value cOp)) ->
      match oOp, cOp with
      | OpenParen, CloseParen | OpenParenPipe, PipeCloseParen | OpenBrace, CloseBrace | OpenSquareBrace, CloseSquareBrace
        -> Some <| InBetweenExpression (Value openToken, expr, Value closeToken)
      | _ -> None
    | _ -> None

  let rec show (writer: IndentedTextWriter) = function
  | LiteralExpression (Value v) ->
    writer.WriteLine "Literal: "
    writer.Indent <- writer.Indent + 1
    Token.show writer v
    writer.Indent <- writer.Indent - 1
  | IdentifierExpression (Value i) ->
    writer.WriteLine "Identifier: "
    writer.Indent <- writer.Indent + 1
    Token.show writer i
    writer.Indent <- writer.Indent - 1
  | PrefixUnaryExpression (Value op, expr) ->
    writer.WriteLine "Unary pre-fix: "
    writer.Indent <- writer.Indent + 1
    writer.WriteLine "Operator: "
    writer.Indent <- writer.Indent + 1
    Token.show writer op
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Expression: "
    writer.Indent <- writer.Indent + 1
    show writer expr
    writer.Indent <- writer.Indent - 2
  | PosfixUnaryExpression (expr, Value op) ->
    writer.WriteLine "Unary post-fix: "
    writer.Indent <- writer.Indent + 1
    writer.WriteLine "Expression: "
    writer.Indent <- writer.Indent + 1
    show writer expr
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Operator: "
    writer.Indent <- writer.Indent + 1
    Token.show writer op
    writer.Indent <- writer.Indent - 2
  | BinaryExpression (left, Value op, right) ->
    writer.WriteLine "Binary Expression: "
    writer.Indent <- writer.Indent + 1
    writer.WriteLine "Left: "
    writer.Indent <- writer.Indent + 1
    show writer left
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Operator: "
    writer.Indent <- writer.Indent + 1
    Token.show writer op
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Right: "
    writer.Indent <- writer.Indent + 1
    show writer right
    writer.Indent <- writer.Indent - 2
  | InBetweenExpression (Value open', expr, Value close) ->
    writer.WriteLine "In Between Expression: "
    writer.Indent <- writer.Indent + 1
    writer.WriteLine "Open Token: "
    writer.Indent <- writer.Indent + 1
    Token.show writer open'
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Expression: "
    writer.Indent <- writer.Indent + 1
    show writer expr
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Closer Token: "
    writer.Indent <- writer.Indent + 1
    Token.show writer close
    writer.Indent <- writer.Indent - 2
  | TernaryExpression (cond, Value condOp, isTrue, Value sepOp, isFalse) ->
    writer.WriteLine "Ternary Expression: "
    writer.Indent <- writer.Indent + 1
    writer.WriteLine "Condition: "
    writer.Indent <- writer.Indent + 1
    show writer cond
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Condition Token: "
    writer.Indent <- writer.Indent + 1
    Token.show writer condOp
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Is True Expression: "
    writer.Indent <- writer.Indent + 1
    show writer isTrue
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Seperator Token: "
    writer.Indent <- writer.Indent + 1
    Token.show writer sepOp
    writer.Indent <- writer.Indent - 1
    writer.WriteLine "Is False Expression: "
    writer.Indent <- writer.Indent + 1
    show writer isFalse
    writer.Indent <- writer.Indent - 2
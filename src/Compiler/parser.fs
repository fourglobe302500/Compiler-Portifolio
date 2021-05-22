module Compiler.Parser

open System
open System.CodeDom.Compiler

let parse input =
  let errors = ResizeArray ()

  let rec parseExpr stack =
    let rec parseBase = function
      | ({ Token = SyntaxToken.Identifier _ } as i)::({Token = SyntaxToken.IsOperator (Punctuation.IsUnary true) } as op)::tail ->
        match Expression.createIdentifier i with
        | Some e ->
          Expression.createPosfixUnary op e, tail
        | None ->
          errors.Add (sprintf "Internal error processing token '%O' at %i" i.Token i.Position)
          None, tail
      | ({ Token = SyntaxToken.Identifier _ } as i)::tail ->
        Expression.createIdentifier i, tail
      | ({ Token = SyntaxToken.IsValue _ } as t)::tail ->
        Expression.createLiteral t, tail
      | ({ Token = SyntaxToken.IsOperator (Punctuation.IsOpener closer) } as opener)::tail ->
        let (e, r) = parseExpr tail
        match r with
        | ({ Token = SyntaxToken.IsOperator (Punctuation.IsCloser cl) } as clT)::tail when cl = closer ->
          match e with
          | Some expr ->
            Expression.createInBetween opener expr clT
          | None -> None
          , tail
        | cl::tail ->
          errors.Add (sprintf "Unexpected token '%O' at %i" cl.Token cl.Position)
          None, tail
        | [] ->
          errors.Add "Expected closer token at"
          None, []
      | t::tail ->
        errors.Add (sprintf "Unexpected token '%O' at %i" t.Token t.Position)
        None, tail
      | [] ->
        errors.Add "Expected token but found none"
        None, []

    let rec parseUnary = function
      | ({ Token = SyntaxToken.IsOperator (Punctuation.IsUnary _) } as t)::tail ->
        let (e, r) = (parseUnary tail)
        match e with
        | Some expr ->
          Expression.createPrefixUnary t expr
        | None -> None
        , r
      | stack -> parseBase stack

    let rec parseBinary precedence stack =
      let (left, r) = parseUnary stack
      let rec inner left stack =
        match stack with
        | ({ Token = SyntaxToken.IsOperator (Punctuation.IsBinary p) } as op)::tail ->
          if p <= precedence then left, stack
          else
            let _tail = tail
            let (right, r) = parseBinary p tail
            let n =
              match left, right with
              | Some left, Some right -> Expression.createBinary op left right
              | _ -> None
            inner n r
        | _ -> left, stack
      inner left r

    let (e, r) = parseBinary 0 stack
    match r with
    | ({ Token = SyntaxToken.IsOperator (Punctuation.QuestionMark) } as qm)::tail ->
      let (iT, r) = parseExpr tail
      match r with
      | ({ Token = SyntaxToken.IsOperator (Punctuation.Colon) } as co)::tail ->
        let (iF, r) = parseExpr tail
        match e, iT, iF with
        | Some expr, Some isTrue, Some isFalse -> Expression.createTernary qm co expr isTrue isFalse
        | _ -> None
        , r
      | _::_ ->
        errors.Add (sprintf "Missing colon at %i" r.[0].Position)
        let (_, r) = parseExpr r
        None, r
      | [] ->
        errors.Add "Missing colon & expression"
        None, r
    | _ -> e, r

  input
  |> List.filter (fun t -> match t.Token with | WhiteSpace _ | EndOfFile -> false | _ -> true)
  |> parseExpr
  |> fun (expr, r) ->
    match r, expr with
    | [], Some expression -> Success expression
    | _::_, _ -> Failure ((errors |> Seq.toList)@["Invallid input. Cannot have hanging values."])
    | _, None -> Failure ((errors |> Seq.toList)@["Invallid expression."])
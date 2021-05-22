open System
open Compiler
open System.CodeDom.Compiler

[<EntryPoint>]
let main argv =
  if argv.Length > 0 then
    printfn "REPL doesn't support arguments."
    1
  else
    while true do
      printf "> "
      Console.ReadLine()
      |> fun l -> if String.IsNullOrEmpty l then exit 0 else l
      |> Lexer.lex
      |>> Parser.parse
      |> function
        | Success expr ->
          Expression.show (new IndentedTextWriter(Console.Out)) expr
        | Failure diagnostics ->
          Console.ForegroundColor <- ConsoleColor.Red
          diagnostics
          |> List.iter (printfn "%s")
          Console.ResetColor()
    0
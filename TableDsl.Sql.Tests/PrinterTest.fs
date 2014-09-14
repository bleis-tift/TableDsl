namespace TableDsl.Sql.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module PrinterTest =
  open TableDsl
  open TableDsl.Sql
  open Basis.Core

  [<Test>]
  let ``print empty list`` () =
    []
    |> Printer.print
    |> should equal ""

  let trimAndCountIndent (str: string) =
    let indent = str |> Seq.takeWhile ((=)' ') |> Seq.length
    (str |> Str.subFrom indent, indent)

  let adjust str =
    let lines =
      str |> Str.replace "\r\n" "\n" |> Str.splitBy "\n" |> Array.toList
    let adjusted =
      match lines with
      | [] -> []
      | [line] -> [line]
      | ""::first::rest ->
          let first, indent = trimAndCountIndent first
          first::(rest |> List.map (Str.subFrom indent))
      | _ ->
          failwithf "oops! %A" lines
    adjusted |> Str.join "\n"

  let source =
    [
      """
      table Users = {
        Id: int
        Name: nvarchar(16)
      }""", """
      CREATE TABLE [Users] (
          [Id] int NOT NULL
        , [Name] nvarchar(16) NOT NULL
      );"""
    ]
    |> List.map (fun (a, b) -> (adjust a, adjust b))

  [<TestCaseSource("source")>]
  let tests (args: System.Tuple<string, string>) =
    let input = args.Item1
    let expected = args.Item2
    input
    |> Parser.parse
    |> Printer.print
    |> should equal expected

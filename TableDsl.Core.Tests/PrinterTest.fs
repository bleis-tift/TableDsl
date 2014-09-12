namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module PrinterTest =
  open TableDsl
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
    [ "coltype Created = datetime2"
      """
      coltype Platform =
        | iOS = 1
        | Android = 2
      based int"""
      "coltype uniqueidentifier = { uniqueidentifier with default = NEWID() }"
      """
      coltype Platform =
        | iOS = 1
        | Android = 2
      based { int with default = 1 }"""
      "coltype T(@n) = decimal(@n, 4)"
      "coltype T = nullable(nvarchar(256))"
      "coltype nvarchar(@n) = { nvarchar(@n) with collate = Japanese_BIN }"
      "coltype T(@a, @b, @c) = { int with default = 1@a@b@c }"
      """
      coltype FK(@table, @col) = { uniqueidentifier with FK = @table.@col }
      coltype FKID(@table) = FK(@table, Id)"""
      """
      /// 名前を表します。
      coltype Name[名前] = { nvarchar with collate = Japanese_BIN }"""
    ]
    |> List.map adjust

  [<TestCaseSource("source")>]
  let tests str =
    str
    |> Parser.parse
    |> Printer.print
    |> should equal str
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

      """
      table Users = {
        Id: int
        Name: nvarchar(16)
      }"""
      """
      table Users = {
        Id: int
        Name: nvarchar(16)
      }
      table DeletedUsers = {
        Id: int
        UserId: int
      }"""
      """
      table Users = {
        Id: { uniqueidentifier with PK }
      }"""
      """
      table Users = {
        Id: { uniqueidentifier with index = unclustered.IX1.1 }
      }"""
      """
      table Users = {
        Id: { uniqueidentifier with PK = unclustered; index = clustered.IX1.1; unique }
      }"""
      """
      table Users = {
        _: datetime2
      }"""
      """
      table Users = {
        Id: int
        Name: nullable(nvarchar(16))
        Age: nullable(int)
      }"""
      """
      /// ユーザテーブル
      /// ユーザを表す。
      table Users[ユーザテーブル] = {
        /// ID
        Id[ID]: int
        /// ユーザ名
        Name[名前]: nvarchar(16)
      }"""
    ]
    |> List.map adjust

  [<TestCaseSource("source")>]
  let tests str =
    let printed = str |> Parser.parse |> Printer.print

    printed |> Parser.parse
    |> should equal (str |> Parser.parse)
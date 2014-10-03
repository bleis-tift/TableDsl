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

  type Source = {
    Input: string
    Expected: string
  }

  let source =
    [
      { Input = """
                table Users = {
                  Id: int
                  Name: nvarchar(16)
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                   );""" }
      { Input = """
                table Users = {
                  Id: int
                  Name: nvarchar(16)
                }
                table DeletedUsers = {
                  Id: int
                  UserId: int
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                   );
                   CREATE TABLE [DeletedUsers] (
                       [Id] int NOT NULL
                     , [UserId] int NOT NULL
                   );""" }
      { Input = """
                table Users = {
                  Id: { uniqueidentifier with PK }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] uniqueidentifier NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [PK_Users] PRIMARY KEY CLUSTERED (
                       [Id]
                   );""" }
      { Input = """
                table Users = {
                  Name: { nvarchar(128) with PK = PK1 }
                  Age: { int with PK = PK1 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(128) NOT NULL
                     , [Age] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [PK1_Users] PRIMARY KEY CLUSTERED (
                       [Name]
                     , [Age]
                   );""" }
      { Input = """
                table Users = {
                  Name: { nvarchar(128) with PK = PK1.2 }
                  Age: { int with PK = PK1.1 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(128) NOT NULL
                     , [Age] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [PK1_Users] PRIMARY KEY CLUSTERED (
                       [Age]
                     , [Name]
                   );""" }
    ]
    |> List.map (fun { Input = a; Expected = b} -> { Input = adjust a; Expected = adjust b })

  [<TestCaseSource("source")>]
  let tests { Input = input; Expected = expected } =
    input
    |> Parser.parse
    |> Printer.print
    |> should equal expected

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
      { Input = """
                table Users = {
                  Id: { uniqueidentifier with PK }
                  Name: nvarchar(128)
                }
                table DeletedUsers = {
                  Id: { uniqueidentifier with PK }
                  UserId: { uniqueidentifier with FK = Users.Id }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] uniqueidentifier NOT NULL
                     , [Name] nvarchar(128) NOT NULL
                   );
                   CREATE TABLE [DeletedUsers] (
                       [Id] uniqueidentifier NOT NULL
                     , [UserId] uniqueidentifier NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [PK_Users] PRIMARY KEY CLUSTERED (
                       [Id]
                   );
                   ALTER TABLE [DeletedUsers] ADD CONSTRAINT [PK_DeletedUsers] PRIMARY KEY CLUSTERED (
                       [Id]
                   );
                   ALTER TABLE [DeletedUsers] ADD CONSTRAINT [FK_DeletedUsers_Users] FOREIGN KEY (
                       [UserId]
                   ) REFERENCES [Users] (
                       [Id]
                   ) ON UPDATE NO ACTION
                     ON DELETE NO ACTION;""" }
      { Input = """
                table Users = {
                  Name: { nvarchar(128) with PK = PK1 }
                  Age: { int with PK = PK1 }
                }
                table DeletedUsers = {
                  Name: { nvarchar(128) with FK = FK1.1.Users.Name }
                  Age: { int with FK = FK1.2.Users.Age }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(128) NOT NULL
                     , [Age] int NOT NULL
                   );
                   CREATE TABLE [DeletedUsers] (
                       [Name] nvarchar(128) NOT NULL
                     , [Age] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [PK1_Users] PRIMARY KEY CLUSTERED (
                       [Name]
                     , [Age]
                   );
                   ALTER TABLE [DeletedUsers] ADD CONSTRAINT [FK1_DeletedUsers_Users] FOREIGN KEY (
                       [Name]
                     , [Age]
                   ) REFERENCES [Users] (
                       [Name]
                     , [Age]
                   ) ON UPDATE NO ACTION
                     ON DELETE NO ACTION;""" }
    ]
    |> List.map (fun { Input = a; Expected = b} -> { Input = adjust a; Expected = adjust b })

  [<TestCaseSource("source")>]
  let tests { Input = input; Expected = expected } =
    input
    |> Parser.parse
    |> Printer.print
    |> should equal expected

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
      // 単純な例
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
      // ワイルドカード
      { Input = """
                coltype Created = datetime2
                table Users = {
                  Id: int
                  Name: nvarchar(16)
                  _: Created
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                     , [Created] datetime2 NOT NULL
                   );""" }
      // 2つのテーブル
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
      // NULL
      { Input = """
                table Users = {
                  Id: int
                  Name: nullable(nvarchar(16))
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NULL
                   );""" }
      // 単純なPK
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
      // 複数列でPK
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
      // 複数列でPK(順番指定)
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
      // FK
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
      // 複数列でFK
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
      // INDEX
      { Input = """
                table Users = {
                  Id: int
                  Name: { nvarchar(16) with index = IX1 }
                  Age: { int with index = IX1 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                     , [Age] int NOT NULL
                   );
                   CREATE NONCLUSTERED INDEX [IX1_Users] ON [Users] (
                       [Name]
                     , [Age]
                   );""" }
      // UNIQUE制約
      { Input = """
                table Users = {
                  Id: int
                  Name: { nvarchar(16) with unique }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ_Users] UNIQUE NONCLUSTERED (
                       [Name]
                   );""" }
      // 複数列でUNIQUE制約
      { Input = """
                table Users = {
                  Name: { nvarchar(16) with unique = UQ1 }
                  Age: { int with unique = UQ1; unique = UQ2 }
                  Hoge: { int with unique = UQ2 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(16) NOT NULL
                     , [Age] int NOT NULL
                     , [Hoge] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ1_Users] UNIQUE NONCLUSTERED (
                       [Name]
                     , [Age]
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ2_Users] UNIQUE NONCLUSTERED (
                       [Age]
                     , [Hoge]
                   );""" }
      // 複数列でUNIQUE制約(順番指定)
      { Input = """
                table Users = {
                  Name: { nvarchar(16) with unique = UQ1.2 }
                  Age: { int with unique = UQ1.1; unique = UQ2 }
                  Hoge: { int with unique = UQ2 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(16) NOT NULL
                     , [Age] int NOT NULL
                     , [Hoge] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ1_Users] UNIQUE NONCLUSTERED (
                       [Age]
                     , [Name]
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ2_Users] UNIQUE NONCLUSTERED (
                       [Age]
                     , [Hoge]
                   );""" }
      // UNIQUE制約(クラスタ化)
      { Input = """
                table Users = {
                  Id: int
                  Name: { nvarchar(16) with unique = clustered }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Id] int NOT NULL
                     , [Name] nvarchar(16) NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [UQ_Users] UNIQUE CLUSTERED (
                       [Name]
                   );""" }
      // DEFAULT制約
      { Input = """
                table Users = {
                  Name: nvarchar(128)
                  Age: { int with default = 42 }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(128) NOT NULL
                     , [Age] int NOT NULL
                   );
                   ALTER TABLE [Users] ADD CONSTRAINT [DF_Users_Age] DEFAULT (42) FOR [Age];""" }
      // COLLATE
      { Input = """
                table Users = {
                  Name: { nvarchar(128) with collate = Japanese_XJIS_100_CI_AS_SC }
                }"""
        Expected = """
                   CREATE TABLE [Users] (
                       [Name] nvarchar(128) COLLATE Japanese_XJIS_100_CI_AS_SC NOT NULL
                   );""" }
    ]
    |> List.map (fun { Input = a; Expected = b} -> { Input = adjust a; Expected = adjust b })

  [<TestCaseSource("source")>]
  let tests { Input = input; Expected = expected } =
    input
    |> Parser.parse
    |> Printer.print
    |> should equal expected

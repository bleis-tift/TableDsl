namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module ParserTest =
  open Basis.Core
  open TableDsl
  open TableDsl.Parser

  let builtin0 name =
    { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [] }
      ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None }

  let builtinTypeDef0 name =
    { TypeName = name; TypeParameters = [] }

  let boundNullable typ =
    { ColumnTypeDef = BuiltinType { TypeName = "nullable"; TypeParameters = [BoundType typ] }
      ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

  [<Test>]
  let ``empty string`` () =
    ""
    |> parse
    |> should equal []

  module ColTypeDef =
    let builtin1 name _1 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [TypeVariable _1] }
        ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

    let builtin2 name _1 _2 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [TypeVariable _1; TypeVariable _2] }
        ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

    let parse input =
      Parser.parse input
      |> List.choose (function ColTypeDef def -> Some def | _ -> None)

    let tryParse input =
      Parser.tryParse input
      |> Result.map (fst >> List.choose (function ColTypeDef def -> Some def | _ -> None))

    [<Test>]
    let ``one simple alias def`` () =
      "coltype Created = datetime2"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "Created"; TypeParameters = [] }, builtin0 "datetime2")
                          ColumnAttributes = []
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one simple enum type`` () =
      """
      coltype Platform =
        | iOS = 1
        | Android = 2
      based int"""
      |> parse
      |> should equal [ { ColumnTypeDef = EnumTypeDef { EnumTypeName = "Platform"
                                                        BaseType = builtinTypeDef0 "int"
                                                        Cases = [("iOS", 1); ("Android", 2)] }
                          ColumnAttributes = []
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one alias def with attribute`` () =
      "coltype uniqueidentifier = { uniqueidentifier with default = NEWID() }"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "uniqueidentifier"; TypeParameters = [] }, builtin0 "uniqueidentifier")
                          ColumnAttributes = [ ComplexColAttr ("default", [ Lit "NEWID()" ]) ]
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one simple enum type with attribute`` () =
      """
      coltype Platform =
        | iOS = 1
        | Android = 2
      based { int with default = 1 }"""
      |> parse
      |> should equal [ { ColumnTypeDef = EnumTypeDef { EnumTypeName = "Platform"
                                                        BaseType = builtinTypeDef0 "int"
                                                        Cases = [("iOS", 1); ("Android", 2)] }
                          ColumnAttributes = [ ComplexColAttr ("default", [ Lit "1" ]) ]
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one generic alias def - partial bound`` () =
      let originalType =
        { ColumnTypeDef = BuiltinType { TypeName = "decimal"; TypeParameters = [TypeVariable "@n"; BoundValue "4"] }
          ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

      "coltype T(@n) = decimal(@n, 4)"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "T"; TypeParameters = [TypeVariable "@n"] }, originalType)
                          ColumnAttributes = []
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one alias def with nested type`` () =
      let nvarchar256 = { ColumnTypeDef = BuiltinType { TypeName = "nvarchar"; TypeParameters = [BoundValue "256"] }
                          ColumnAttributes = []
                          ColumnSummary = None
                          ColumnJpName = None }
      "coltype T = nullable(nvarchar(256))"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "T"; TypeParameters = [] }, boundNullable nvarchar256)
                          ColumnAttributes = []
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``one generic alias def with attribute`` () =
      "coltype nvarchar(@n) = { nvarchar(@n) with collate = Japanese_BIN }"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "nvarchar"; TypeParameters = [TypeVariable "@n"] }, builtin1 "nvarchar" "@n")
                          ColumnAttributes = [ ComplexColAttr ("collate", [ Lit "Japanese_BIN" ]) ]
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``generic alias def with parameterized attribute value`` () =
      "coltype T(@a, @b, @c) = { int with default = 1@a@b@c }"
      |> parse
      |> should equal [ { ColumnTypeDef =
                            AliasDef ({ TypeName = "T"; TypeParameters = [TypeVariable "@a"; TypeVariable "@b"; TypeVariable "@c"] }, builtin0 "int")
                          ColumnAttributes = [ ComplexColAttr ("default", [ Lit "1"; Var "@a"; Var "@b"; Var "@c" ]) ]
                          ColumnSummary = None
                          ColumnJpName = None } ]

    [<Test>]
    let ``two alias defs`` () =
      """
      coltype FK(@table, @col) = { uniqueidentifier with FK = @table.@col }
      coltype FKID(@table) = FK(@table, Id)
      """
      |> parse
      |> should equal
           [ let fkType sndParam sndAttr =
               { ColumnTypeDef = AliasDef ({ TypeName = "FK"; TypeParameters = [TypeVariable "@table"; sndParam] }, builtin0 "uniqueidentifier")
                 ColumnAttributes = [ ComplexColAttr ("FK", [ Var "@table"; Lit "."; sndAttr ]) ]
                 ColumnSummary = None
                 ColumnJpName = None }
             yield fkType (TypeVariable "@col") (Var "@col")
             yield { ColumnTypeDef = AliasDef ({ TypeName = "FKID"; TypeParameters = [TypeVariable "@table"] }, fkType (BoundValue "Id") (Lit "Id"))
                     ColumnAttributes = []
                     ColumnSummary = None
                     ColumnJpName = None }
           ]

    [<Test>]
    let ``summary and jp name`` () =
      """
      /// 名前を表します。
      coltype Name[名前] = { nvarchar with collate = Japanese_BIN }
      """
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "Name"; TypeParameters = [] }, builtin0 "nvarchar")
                          ColumnAttributes = [ ComplexColAttr ("collate", [ Lit "Japanese_BIN" ]) ]
                          ColumnSummary = Some "名前を表します。"
                          ColumnJpName = Some "名前" } ]

    [<Test>]
    let ``dup type variable`` () =
      "coltype nvarchar(@n, @n) = { nvarchar(@n) with collate = Japanese_BIN }"
      |> tryParse
      |> function
         | Success res -> failwithf "oops! Success! : %A" res
         | Failure (FParsecDefaultMessage err) -> failwithf "oops! : %s" err
         | Failure (UserFriendlyMessages errs) -> errs |> List.map (fun (_, _, msg) -> msg) |> should equal ["型nvarcharの定義で型変数@nが重複しています。"]

    [<Test>]
    let ``can't inherit enum type`` () =
      """
      coltype Platform =
        | iOS = 1
        | Android = 2
      based { int with default = 1 }
      
      coltype Platform2 =
        | WP = 3
      based Platform"""
      |> tryParse
      |> function
         | Success res -> failwithf "oops! Success! : %A" res
         | Failure (FParsecDefaultMessage err) -> failwithf "oops! : %s" err
         | Failure (UserFriendlyMessages errs) -> errs |> List.map (fun (_, _, msg) -> msg) |> should equal ["列挙型の定義(Platform2)の基底型に他の列挙型(Platform)を指定することはできません。"]

  module TableDef =
    let nullable typ =
      { ColumnTypeDef = BuiltinType { TypeName = "nullable"; TypeParameters = [BoundType typ] }
        ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None }
    let builtin1 name _1 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [BoundValue _1] }
        ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

    let builtin2 name _1 _2 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [BoundValue _1; BoundValue _2] }
        ColumnAttributes = []; ColumnSummary = None; ColumnJpName = None}

    let parse input =
      Parser.parse input
      |> List.choose (function TableDef def -> Some def | _ -> None)

    let tryParse input =
      Parser.tryParse input
      |> Result.map (fst >> List.choose (function TableDef def -> Some def | _ -> None))

    [<Test>]
    let ``one simple table`` () =
      """
      table Users = {
        Id: int
        Name: nvarchar(16)
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = (builtin1 "nvarchar" "16", []) } ]
             }
           ]

    [<Test>]
    let ``two simple tables`` () =
      """
      table Users = {
        Id: int
        Name: nvarchar(16)
      }
      table DeletedUsers = {
        Id: int
        UserId: int
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = (builtin1 "nvarchar" "16", []) } ]
             }
             { TableSummary = None
               TableAttributes = []
               TableName = "DeletedUsers"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("UserId", None)
                     ColumnType = (builtin0 "int", []) } ] }
           ]

    [<Test>]
    let ``simple attr`` () =
      """
      table Users = {
        Id: { uniqueidentifier with PK }
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "uniqueidentifier", [SimpleColAttr "PK"]) } ]
             }
           ]

    [<Test>]
    let ``complex attr`` () =
      """
      table Users = {
        Id: { uniqueidentifier with index = unclustered.IX1.1 }
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "uniqueidentifier", [ComplexColAttr ("index", [ Lit "unclustered.IX1.1" ])]) } ]
             }
           ]

    [<Test>]
    let ``attributes`` () =
      """
      table Users = {
        Id: { uniqueidentifier with PK = unclustered; index = clustered.IX1.1; unique }
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "uniqueidentifier",
                                   [ ComplexColAttr ("PK", [ Lit "unclustered" ])
                                     ComplexColAttr ("index", [ Lit "clustered.IX1.1" ])
                                     SimpleColAttr "unique" ]) } ]
             }
           ]

    [<Test>]
    let ``wildcard column`` () =
      """
      table Users = {
        _ : datetime2
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = Wildcard
                     ColumnType = (builtin0 "datetime2", []) } ] } ]

    [<Test>]
    let ``nullable type`` () =
      """
      table Users = {
        Id: int
        Name: nullable(nvarchar(16))
        Age: nullable(int)
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = []
               TableName = "Users"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = (nullable (builtin1 "nvarchar" "16"), []) }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Age", None)
                     ColumnType = (nullable (builtin0 "int"), []) } ]
             }
           ]

    [<Test>]
    let ``summary and jp name`` () =
      """
      /// ユーザテーブル
      /// ユーザを表す。
      table Users[ユーザテーブル] = {
        /// ID
        Id[ID]: int
        /// ユーザ名
        Name[名前]: nvarchar(16)
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = Some "ユーザテーブル\nユーザを表す。"
               TableAttributes = []
               TableName = "Users"
               TableJpName = Some "ユーザテーブル"
               ColumnDefs =
                 [ { ColumnSummary = Some "ID"
                     ColumnName = ColumnName ("Id", Some "ID")
                     ColumnType = (builtin0 "int", []) }
                   { ColumnSummary = Some "ユーザ名"
                     ColumnName = ColumnName ("Name", Some "名前")
                     ColumnType = (builtin1 "nvarchar" "16", []) } ]
             }
           ]

    let errMsg (lines: string list) = System.String.Join("\r\n", lines)

    [<Test>]
    let ``type not found`` () =
      """
      table Users = {
        Name: string
      }
      """
      |> tryParse
      |> function
         | Success res -> failwithf "oops! Success! : %A" res
         | Failure (FParsecDefaultMessage err) -> failwithf "oops! : %s" err
         | Failure (UserFriendlyMessages errs) -> errs |> List.map (fun (_, _, msg) -> msg) |> should equal ["stringという型が見つかりませんでした。"]

    [<Test>]
    let ``simple table attr`` () =
      """
      [<Master>]
      table Apps = {
        Id: int
      }"""
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = [SimpleTableAttr "Master"]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }]
             }
           ]

    [<Test>]
    let ``complex table attr`` () =
      """
      [<Foo(0, abcd)>]
      table Apps = {
        Id: int
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = [ComplexTableAttr ("Foo", ["0"; "abcd"])]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }]
             }
           ]

    [<Test>]
    let ``table attr combination`` () =
      """
      [<Master>]
      [<Foo(0, abcd)>]
      table Apps = {
        Id: int
      }
      """
      |> parse
      |> should equal
           [ { TableSummary = None
               TableAttributes = [SimpleTableAttr "Master"; ComplexTableAttr ("Foo", ["0"; "abcd"])]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = (builtin0 "int", []) }]
             }
           ]
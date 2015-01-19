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
      ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None }

  let builtinTypeDef0 name =
    { TypeName = name; TypeParameters = [] }

  [<Test>]
  let ``empty string`` () =
    ""
    |> parse
    |> should equal []

  module ColTypeDef =
    let builtin1 name _1 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [TypeVariable _1] }
        ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None}

    let builtin2 name _1 _2 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [TypeVariable _1; TypeVariable _2] }
        ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None}

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
                          ColumnTypeDefAttributes = []
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

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
                                                        Cases = [("iOS", None, 1); ("Android", None, 2)] }
                          ColumnTypeDefAttributes = []
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``one alias def with attribute`` () =
      "coltype uniqueidentifier = { uniqueidentifier with default = NEWID() }"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "uniqueidentifier"; TypeParameters = [] }, builtin0 "uniqueidentifier")
                          ColumnTypeDefAttributes = [ ComplexColAttr ("default", [ Lit "NEWID()" ]) ]
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

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
                                                        Cases = [("iOS", None, 1); ("Android", None, 2)] }
                          ColumnTypeDefAttributes = [ ComplexColAttr ("default", [ Lit "1" ]) ]
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``one simple enum type with jpName`` () =
      """
      coltype Platform =
        | iOS[アイオーエス] = 1
        | Android[アンヨヨイヨ] = 2
      based int"""
      |> parse
      |> should equal [ { ColumnTypeDef = EnumTypeDef { EnumTypeName = "Platform"
                                                        BaseType = builtinTypeDef0 "int"
                                                        Cases = [("iOS", Some "アイオーエス", 1); ("Android", Some "アンヨヨイヨ", 2)] }
                          ColumnTypeDefAttributes = []
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``one generic alias def - partial bound`` () =
      let originalType =
        { ColumnTypeDef = BuiltinType { TypeName = "decimal"; TypeParameters = [TypeVariable "@n"; BoundValue "4"] }
          ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None}

      "coltype T(@n) = decimal(@n, 4)"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "T"; TypeParameters = [TypeVariable "@n"] }, originalType)
                          ColumnTypeDefAttributes = []
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``one generic alias def with attribute`` () =
      "coltype nvarchar(@n) = { nvarchar(@n) with collate = Japanese_BIN }"
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "nvarchar"; TypeParameters = [TypeVariable "@n"] }, builtin1 "nvarchar" "@n")
                          ColumnTypeDefAttributes = [ ComplexColAttr ("collate", [ Lit "Japanese_BIN" ]) ]
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``generic alias def with parameterized attribute value`` () =
      "coltype T(@a, @b, @c) = { int with default = 1@a@b@c }"
      |> parse
      |> should equal [ { ColumnTypeDef =
                            AliasDef ({ TypeName = "T"; TypeParameters = [TypeVariable "@a"; TypeVariable "@b"; TypeVariable "@c"] }, builtin0 "int")
                          ColumnTypeDefAttributes = [ ComplexColAttr ("default", [ Lit "1"; Var "@a"; Var "@b"; Var "@c" ]) ]
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

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
                 ColumnTypeDefAttributes = [ ComplexColAttr ("FK", [ Var "@table"; Lit "."; sndAttr ]) ]
                 ColumnTypeDefSummary = None
                 ColumnTypeDefJpName = None }
             yield fkType (TypeVariable "@col") (Var "@col")
             yield { ColumnTypeDef = AliasDef ({ TypeName = "FKID"; TypeParameters = [TypeVariable "@table"] }, fkType (BoundValue "Id") (Lit "Id"))
                     ColumnTypeDefAttributes = []
                     ColumnTypeDefSummary = None
                     ColumnTypeDefJpName = None }
           ]

    [<Test>]
    let ``summary and jp name`` () =
      """
      /// 名前を表します。
      coltype Name[名前] = { nvarchar with collate = Japanese_BIN }
      """
      |> parse
      |> should equal [ { ColumnTypeDef = AliasDef ({ TypeName = "Name"; TypeParameters = [] }, builtin0 "nvarchar")
                          ColumnTypeDefAttributes = [ ComplexColAttr ("collate", [ Lit "Japanese_BIN" ]) ]
                          ColumnTypeDefSummary = Some "名前を表します。"
                          ColumnTypeDefJpName = Some "名前" } ]

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
         | Failure (UserFriendlyMessages errs) -> 
            errs 
            |> List.map (fun (_, _, msg) -> msg) 
            |> should equal ["列挙型の定義(Platform2)の基底型には組み込み型しか指定できませんが、他の列挙型(Platform)が指定されました。"]

  module TableDef =
    let builtin1 name _1 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [BoundValue _1] }
        ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None}

    let builtin2 name _1 _2 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [BoundValue _1; BoundValue _2] }
        ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None}

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
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = { Type = NonEnum { RootType = "nvarchar(16)"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "nvarchar"
                                    ColumnTypeRefParams = ["16"]
                                    ColumnTypeRefAttributes = [] } } ]
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
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = { Type = NonEnum { RootType = "nvarchar(16)"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "nvarchar"
                                    ColumnTypeRefParams = ["16"]
                                    ColumnTypeRefAttributes = [] } } ]
             }
             { TableSummary = None
               TableAttributes = []
               TableName = "DeletedUsers"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("UserId", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ] }
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
                     ColumnType = { Type = NonEnum { RootType = "uniqueidentifier"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "uniqueidentifier"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [SimpleAttr "PK"] } } ]
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
                     ColumnType = { Type = NonEnum { RootType = "uniqueidentifier"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "uniqueidentifier"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [ComplexAttr ("index", ["unclustered.IX1.1"])] } } ]
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
                     ColumnType = { Type = NonEnum { RootType = "uniqueidentifier"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "uniqueidentifier"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes =
                                      [ComplexAttr ("PK", ["unclustered"])
                                       ComplexAttr ("index", ["clustered.IX1.1"])
                                       SimpleAttr "unique"] } } ]
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
                     ColumnType = { Type = NonEnum { RootType = "datetime2"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "datetime2"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ] } ]

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
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Name", None)
                     ColumnType = { Type = NonEnum { RootType = "nvarchar(16)"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = true
                                    ColumnTypeRefName = "nvarchar"
                                    ColumnTypeRefParams = ["16"]
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = None
                     ColumnName = ColumnName ("Age", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = true
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ]
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
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } }
                   { ColumnSummary = Some "ユーザ名"
                     ColumnName = ColumnName ("Name", Some "名前")
                     ColumnType = { Type = NonEnum { RootType = "nvarchar(16)"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "nvarchar"
                                    ColumnTypeRefParams = ["16"]
                                    ColumnTypeRefAttributes = [] } } ]
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
               TableAttributes = [SimpleAttr "Master"]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ]
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
               TableAttributes = [ComplexAttr ("Foo", ["0"; "abcd"])]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ]
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
               TableAttributes = [SimpleAttr "Master"; ComplexAttr ("Foo", ["0"; "abcd"])]
               TableName = "Apps"
               TableJpName = None
               ColumnDefs =
                 [ { ColumnSummary = None
                     ColumnName = ColumnName ("Id", None)
                     ColumnType = { Type = NonEnum { RootType = "int"
                                                     ColumnTypeSummary = None
                                                     ColumnTypeJpName = None
                                                     ColumnTypeAttributes = [] }
                                    IsNullable = false
                                    ColumnTypeRefName = "int"
                                    ColumnTypeRefParams = []
                                    ColumnTypeRefAttributes = [] } } ]
             }
           ]
    
  module ComplexDef =

    let builtin1 name _1 =
      { ColumnTypeDef = BuiltinType { TypeName = name; TypeParameters = [TypeVariable _1] }
        ColumnTypeDefAttributes = []; ColumnTypeDefSummary = None; ColumnTypeDefJpName = None }

    [<Test>]
    let ``type def is expanded inline in table def`` () =
      let colDef p = 
        { ColumnTypeDef = AliasDef ({ TypeName = "Name"; TypeParameters = p }, builtin1 "nvarchar" "@n")
                          ColumnTypeDefAttributes = [ ComplexColAttr ("collate", [ Lit "Japanese_BIN" ]); ComplexColAttr ("default", [ Lit "42" ]) ]
                          ColumnTypeDefSummary = Some "名前を表します。"
                          ColumnTypeDefJpName = Some "名前" }
      """
      /// 名前を表します。
      coltype Name(@n)[名前] = { nvarchar(@n) with collate = Japanese_BIN; default = 42 }

      /// ユーザテーブル
      /// ユーザを表す。
      table Users[ユーザテーブル] = {
        /// ID
        Id[ID]: int
        /// ユーザ名
        _: Name(256)
      }"""
      |> parse
      |> should equal
            [ ColTypeDef (colDef [TypeVariable "@n"])
              TableDef
                { TableSummary = Some "ユーザテーブル\nユーザを表す。"
                  TableAttributes = []
                  TableName = "Users"
                  TableJpName = Some "ユーザテーブル"
                  ColumnDefs = 
                    [
                      { ColumnSummary = Some "ID" 
                        ColumnName = ColumnName ("Id", Some "ID")
                        ColumnType = { Type = NonEnum { RootType = "int"
                                                        ColumnTypeSummary = None
                                                        ColumnTypeJpName = None
                                                        ColumnTypeAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeRefName = "int"
                                       ColumnTypeRefParams = []
                                       ColumnTypeRefAttributes = [] } }
                      { ColumnSummary = Some "ユーザ名" 
                        ColumnName = Wildcard
                        ColumnType = { Type = NonEnum { RootType = "nvarchar(256)"
                                                        ColumnTypeSummary = Some "名前を表します。"
                                                        ColumnTypeJpName = Some "名前"
                                                        ColumnTypeAttributes =
                                                          [ComplexAttr ("collate", ["Japanese_BIN"])
                                                           ComplexAttr ("default", ["42"])] }
                                       IsNullable = false
                                       ColumnTypeRefName = "Name"
                                       ColumnTypeRefParams = ["256"]
                                       ColumnTypeRefAttributes = [] } }
                    ]
                }
            ]

    [<Test>]
    let ``alias def in the enum based def`` () =
      """
      coltype foo = int
      coltype Platform =
        | iOS = 1
        | Android = 2
      based { foo with default = 1 }
      """
      |> Parser.tryParse
      |> Result.map (fst >> List.choose (function TableDef def -> Some def | _ -> None))
      |> function
         | Success res -> failwithf "oops... expected is failure but success : %A" res
         | Failure (FParsecDefaultMessage err) -> failwithf "oops... : %s" err
         | Failure (UserFriendlyMessages errs) -> 
            errs 
            |> List.map (fun (_, _, msg) -> msg) 
            |> should equal ["列挙型の定義(Platform)の基底型には組み込み型しか指定できませんが、列定義(foo)が指定されました。"] 
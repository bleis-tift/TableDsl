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

  let builtinTypeDef1 name param =
    { TypeName = name; TypeParameters = [param] }

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
                                                        Cases = [("iOS", None, "1"); ("Android", None, "2")] }
                          ColumnTypeDefAttributes = []
                          ColumnTypeDefSummary = None
                          ColumnTypeDefJpName = None } ]

    [<Test>]
    let ``one simple enum char type`` () =
      """
      coltype Platform =
        | iOS = '1'
        | Android = '2'
      based char(1)"""
      |> parse
      |> should equal [ { ColumnTypeDef = EnumTypeDef { EnumTypeName = "Platform"
                                                        BaseType = builtinTypeDef1 "char" (BoundValue "1")
                                                        Cases = [("iOS", None, "'1'"); ("Android", None, "'2'")] }
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
                                                        Cases = [("iOS", None, "1"); ("Android", None, "2")] }
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
                                                        Cases = [("iOS", Some "アイオーエス", "1"); ("Android", Some "アンヨヨイヨ", "2")] }
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Name", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "nvarchar(16)"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "nvarchar"
                                       ColumnTypeParams = ["16"]
                                       ColumnTypeAttributes = [] } } ]
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Name", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "nvarchar(16)"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "nvarchar"
                                       ColumnTypeParams = ["16"]
                                       ColumnTypeAttributes = [] } } ]
             }
             { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "DeletedUsers"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("UserId", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ] }
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "uniqueidentifier"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "uniqueidentifier"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [SimpleAttr "PK"] } } ]
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "uniqueidentifier"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "uniqueidentifier"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [ComplexAttr ("index", ["unclustered.IX1.1"])] } } ]
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "uniqueidentifier"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "uniqueidentifier"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes =
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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = Wildcard
                     ColumnDefType = { Ref = NonEnum { RootType = "datetime2"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "datetime2"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ] } ]

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
           [ { TableDefSummary = None
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Name", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "nvarchar(16)"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = true
                                       ColumnTypeName = "nvarchar"
                                       ColumnTypeParams = ["16"]
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Age", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = true
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ]
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
           [ { TableDefSummary = Some "ユーザテーブル\nユーザを表す。"
               TableDefAttributes = []
               TableDefName = "Users"
               TableDefJpName = Some "ユーザテーブル"
               ColumnDefs =
                 [ { ColumnDefSummary = Some "ID"
                     ColumnDefName = ColumnName ("Id", Some "ID")
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } }
                   { ColumnDefSummary = Some "ユーザ名"
                     ColumnDefName = ColumnName ("Name", Some "名前")
                     ColumnDefType = { Ref = NonEnum { RootType = "nvarchar(16)"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "nvarchar"
                                       ColumnTypeParams = ["16"]
                                       ColumnTypeAttributes = [] } } ]
             }
           ]

    [<Test>]
    let ``simple table attr`` () =
      """
      [<Master>]
      table Apps = {
        Id: int
      }"""
      |> parse
      |> should equal
           [ { TableDefSummary = None
               TableDefAttributes = [SimpleAttr "Master"]
               TableDefName = "Apps"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ]
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
           [ { TableDefSummary = None
               TableDefAttributes = [ComplexAttr ("Foo", ["0"; "abcd"])]
               TableDefName = "Apps"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ]
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
           [ { TableDefSummary = None
               TableDefAttributes = [SimpleAttr "Master"; ComplexAttr ("Foo", ["0"; "abcd"])]
               TableDefName = "Apps"
               TableDefJpName = None
               ColumnDefs =
                 [ { ColumnDefSummary = None
                     ColumnDefName = ColumnName ("Id", None)
                     ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                       ColumnTypeRefSummary = None
                                                       ColumnTypeRefJpName = None
                                                       ColumnTypeRefAttributes = [] }
                                       IsNullable = false
                                       ColumnTypeName = "int"
                                       ColumnTypeParams = []
                                       ColumnTypeAttributes = [] } } ]
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
                { TableDefSummary = Some "ユーザテーブル\nユーザを表す。"
                  TableDefAttributes = []
                  TableDefName = "Users"
                  TableDefJpName = Some "ユーザテーブル"
                  ColumnDefs = 
                    [
                      { ColumnDefSummary = Some "ID" 
                        ColumnDefName = ColumnName ("Id", Some "ID")
                        ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                          ColumnTypeRefSummary = None
                                                          ColumnTypeRefJpName = None
                                                          ColumnTypeRefAttributes = [] }
                                          IsNullable = false
                                          ColumnTypeName = "int"
                                          ColumnTypeParams = []
                                          ColumnTypeAttributes = [] } }
                      { ColumnDefSummary = Some "ユーザ名" 
                        ColumnDefName = Wildcard
                        ColumnDefType = { Ref = NonEnum { RootType = "nvarchar(256)"
                                                          ColumnTypeRefSummary = Some "名前を表します。"
                                                          ColumnTypeRefJpName = Some "名前"
                                                          ColumnTypeRefAttributes =
                                                             [ ComplexAttr ("collate", ["Japanese_BIN"])
                                                               ComplexAttr ("default", ["42"]) ] }
                                          IsNullable = false
                                          ColumnTypeName = "Name"
                                          ColumnTypeParams = ["256"]
                                          ColumnTypeAttributes = [] } }
                    ]
                }
            ]

    [<Test>]
    let ``nested enum`` () =
      """
      /// プラットフォーム
      coltype Platform[プラットフォーム] =
        | iOS = 1
        | Android = 2
        | Etc = 99
      based { int with default = 99 }

      coltype Platform2 = { Platform with default = 1 }

      /// 端末テーブル
      table Devices[端末] = {
        /// ID
        Id[ID]: int
        /// 種別
        _: Platform2
      }"""
      |> parse
      |> List.filter (function TableDef _ -> true | _ -> false)
      |> should equal
            [ TableDef
                { TableDefSummary = Some "端末テーブル"
                  TableDefAttributes = []
                  TableDefName = "Devices"
                  TableDefJpName = Some "端末"
                  ColumnDefs = 
                    [
                      { ColumnDefSummary = Some "ID" 
                        ColumnDefName = ColumnName ("Id", Some "ID")
                        ColumnDefType = { Ref = NonEnum { RootType = "int"
                                                          ColumnTypeRefSummary = None
                                                          ColumnTypeRefJpName = None
                                                          ColumnTypeRefAttributes = [] }
                                          IsNullable = false
                                          ColumnTypeName = "int"
                                          ColumnTypeParams = []
                                          ColumnTypeAttributes = [] } }
                      { ColumnDefSummary = Some "種別" 
                        ColumnDefName = Wildcard
                        ColumnDefType = { Ref = Enum ({ RootType = "int"
                                                        ColumnTypeRefSummary = Some "プラットフォーム"
                                                        ColumnTypeRefJpName = Some "プラットフォーム"
                                                        ColumnTypeRefAttributes =
                                                           [ ComplexAttr ("default", ["1"]) ] },
                                                      [("iOS", None, "1"); ("Android", None, "2"); ("Etc", None, "99")])
                                          IsNullable = false
                                          ColumnTypeName = "Platform2"
                                          ColumnTypeParams = []
                                          ColumnTypeAttributes = [] } }
                    ]
                }
            ]

namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module ParserTest =
  open Basis.Core
  open TableDsl
  open TableDsl.Parser

  [<Test>]
  let ``empty string`` () =
    ""
    |> Parser.parse
    |> should equal []

  let builtin0 name =
    { ColumnTypeDef = BuiltinType { ColumnTypeName = name; TypeParameters = [] }; ColumnAttributes = [] }

  let builtin1 name _1 =
    { ColumnTypeDef = BuiltinType { ColumnTypeName = name; TypeParameters = [TypeVariable _1] }; ColumnAttributes = [] }

  [<Test>]
  let ``one simple table`` () =
    """
    table Users = {
      Id: int
      Name: nvarchar(16)
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) }
                 { ColumnSummary = None
                   ColumnName = ColumnName ("Name", None)
                   ColumnType = ({ Type = builtin1 "nvarchar" "@1"; TypeParameters = [ "16" ] }, []) } ]
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
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) }
                 { ColumnSummary = None
                   ColumnName = ColumnName ("Name", None)
                   ColumnType = ({ Type = builtin1 "nvarchar" "@1"; TypeParameters = [ "16" ] }, []) } ]
           }
           { TableSummary = None
             TableName = "DeletedUsers"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) }
                 { ColumnSummary = None
                   ColumnName = ColumnName ("UserId", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) } ] }
         ]

  [<Test>]
  let ``simple attr`` () =
    """
    table Users = {
      Id: { uniqueidentifier with PK }
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "uniqueidentifier"; TypeParameters = [] }, [SimpleAttr "PK"]) } ]
           }
         ]

  [<Test>]
  let ``complex attr`` () =
    """
    table Users = {
      Id: { uniqueidentifier with index = unclustered.IX1.1 }
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "uniqueidentifier"; TypeParameters = [] }, [ComplexAttr ("index", { Value = "unclustered.IX1.1" })]) } ]
           }
         ]

  [<Test>]
  let ``attributes`` () =
    """
    table Users = {
      Id: { uniqueidentifier with PK = unclustered; index = clustered.IX1.1; unique }
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "uniqueidentifier"; TypeParameters = [] },
                                 [ ComplexAttr ("PK", { Value = "unclustered" })
                                   ComplexAttr ("index", { Value = "clustered.IX1.1" })
                                   SimpleAttr "unique" ]) } ]
           }
         ]

  [<Test>]
  let ``wildcard column`` () =
    """
    table Users = {
      _ : datetime2
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = Wildcard
                   ColumnType = ({ Type = builtin0 "datetime2"; TypeParameters = [] }, []) } ] } ]

  [<Test>]
  let ``nullable`` () =
    """
    table Users = {
      Id: int
      Name: nullable(nvarchar(16))
      Age: nullable(int)
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = None
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = None
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) }
                 { ColumnSummary = None
                   ColumnName = ColumnName ("Name", None)
                   ColumnType = ({ Type = builtin1 "nullable" "@type"
                                   TypeParameters = [ "nvarchar(16)" ] }, []) }
                 { ColumnSummary = None
                   ColumnName = ColumnName ("Age", None)
                   ColumnType = ({ Type = builtin1 "nullable" "@type"
                                   TypeParameters = [ "int" ] }, []) } ]
           }
         ]

  [<Test>]
  let ``summary`` () =
    """
    /// ユーザテーブル
    /// ユーザを表す。
    table Users = {
      /// ID
      Id: int
      /// ユーザ名
      Name: nvarchar(16)
    }
    """
    |> Parser.parse
    |> should equal
         [ { TableSummary = Some "ユーザテーブル\nユーザを表す。"
             TableName = "Users"
             TableJpName = None
             ColumnDefs =
               [ { ColumnSummary = Some "ID"
                   ColumnName = ColumnName ("Id", None)
                   ColumnType = ({ Type = builtin0 "int"; TypeParameters = [] }, []) }
                 { ColumnSummary = Some "ユーザ名"
                   ColumnName = ColumnName ("Name", None)
                   ColumnType = ({ Type = builtin1 "nvarchar" "@1"; TypeParameters = [ "16" ] }, []) } ]
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
    |> Parser.tryParse
    |> function
       | Success res -> failwithf "oops! Success! : %A" res
       | Failure (FParsecDefaultMessage err) -> failwithf "oops! : %s" err
       | Failure (UserFriendlyMessages errs) -> errs |> List.map (fun (_, _, msg) -> msg) |> should equal ["stringという型が見つかりませんでした。"]

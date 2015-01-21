namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module KeyTest =
  open TableDsl

  let colDef name attrs = {
    ColumnDefSummary = None
    ColumnDefName = ColumnName (name, None)
    ColumnDefType =
    {
      Ref = NonEnum {
        RootType = "int"
        ColumnTypeRefSummary = None
        ColumnTypeRefJpName = None
        ColumnTypeRefAttributes = []
      }
      IsNullable = false
      ColumnTypeName = "int"
      ColumnTypeParams = []
      ColumnTypeAttributes = attrs
    }
  }

  let testInput cols = {
    TableDefSummary = None
    TableDefAttributes = []
    TableDefName = "Table"
    TableDefJpName = None
    ColumnDefs = cols
  }

  [<Test>]
  let empty () =
    testInput []
    |> Key.collectTableKeys
    |> should equal []

  [<Test>]
  let ``simple index`` () =
    testInput [colDef "Col1" [SimpleAttr "index"]]
    |> Key.collectTableKeys
    |> should equal [Index { Name = "IX_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``simple two indexies`` () =
    testInput [ colDef "Col1" [SimpleAttr "index"]
                colDef "Col2" [SimpleAttr "index"] ]
    |> Key.collectTableKeys
    |> should equal [ Index { Name = "IX_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }
                      Index { Name = "IX_Table_2"; ClusteredType = NonClustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc } ] } ]

  [<Test>]
  let ``simple PK`` () =
    testInput [colDef "Col1" [SimpleAttr "PK"]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``simple UQ`` () =
    testInput [colDef "Col1" [SimpleAttr "unique"]]
    |> Key.collectTableKeys
    |> should equal [UniqueKey { Name = "UQ_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["clustered"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``FK`` () =
    testInput [colDef "Col1" [ComplexAttr ("FK", ["Other.ColA"])]]
    |> Key.collectTableKeys
    |> should equal [ForeignKey { KeyName = "FK_Table_Other"
                                  RelationshipTableName = "Other"
                                  ColumnPairs = [ { OwnColumnName = "Col1"; ParentColumnName = "ColA" } ] }]

  [<Test>]
  let ``two FKs`` () =
    testInput [ colDef "Col1" [ComplexAttr ("FK", ["Other.ColA"])]
                colDef "Col2" [ComplexAttr ("FK", ["Other.ColB"])] ]
    |> Key.collectTableKeys
    |> should equal [ ForeignKey { KeyName = "FK_Table_Other"
                                   RelationshipTableName = "Other"
                                   ColumnPairs = [ { OwnColumnName = "Col1"; ParentColumnName = "ColA" } ] }
                      ForeignKey { KeyName = "FK_Table_Other_2"
                                   RelationshipTableName = "Other"
                                   ColumnPairs = [ { OwnColumnName = "Col2"; ParentColumnName = "ColB" } ] } ]

  [<Test>]
  let ``two FKs 2`` () =
    testInput [ colDef "Col1" [ComplexAttr ("FK", ["Other.ColA"])]
                colDef "Col2" [ComplexAttr ("FK", ["Another.ColB"])] ]
    |> Key.collectTableKeys
    |> should equal [ ForeignKey { KeyName = "FK_Table_Other"
                                   RelationshipTableName = "Other"
                                   ColumnPairs = [ { OwnColumnName = "Col1"; ParentColumnName = "ColA" } ] }
                      ForeignKey { KeyName = "FK_Table_Another"
                                   RelationshipTableName = "Another"
                                   ColumnPairs = [ { OwnColumnName = "Col2"; ParentColumnName = "ColB" } ] } ]

  [<Test>]
  let ``FK (prefix)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("FK", ["FK1.Other.ColA"])]
                colDef "Col2" [ComplexAttr ("FK", ["FK1.Other.ColB"])] ]
    |> Key.collectTableKeys
    |> should equal [ForeignKey { KeyName = "FK1_Table_Other"
                                  RelationshipTableName = "Other"
                                  ColumnPairs = [ { OwnColumnName = "Col1"; ParentColumnName = "ColA" }
                                                  { OwnColumnName = "Col2"; ParentColumnName = "ColB" } ] }]

  [<Test>]
  let ``FK (prefix order)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("FK", ["FK1.2.Other.ColA"])]
                colDef "Col2" [ComplexAttr ("FK", ["FK1.1.Other.ColB"])] ]
    |> Key.collectTableKeys
    |> should equal [ForeignKey { KeyName = "FK1_Table_Other"
                                  RelationshipTableName = "Other"
                                  ColumnPairs = [ { OwnColumnName = "Col2"; ParentColumnName = "ColB" }
                                                  { OwnColumnName = "Col1"; ParentColumnName = "ColA" } ] }]

  [<Test>]
  let ``complex PK (nonclustered)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["nonclustered"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered asc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["clustered.asc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered desc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["clustered.desc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                             { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix index)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1.2"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1.1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix index) 2`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1.1"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1.2"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                             { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix index asc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1.1.asc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix index desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1.2.desc"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                             { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (clustered prefix index asc/desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["clustered.PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["clustered.PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered asc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["nonclustered.asc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered desc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["nonclustered.desc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                                { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix index)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1.2"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1.1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                                { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix index) 2`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1.1"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1.2"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                                { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix index asc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1.1.asc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                                { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix index desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1.2.desc"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                                { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (nonclustered prefix index asc/desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["nonclustered.PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["nonclustered.PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = NonClustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                                { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (asc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["asc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (desc)`` () =
    testInput [colDef "Col1" [ComplexAttr ("PK", ["desc"])]]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (prefix)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                             { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (prefix asc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.asc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                             { Name = "Col2"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (prefix desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.desc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Desc }
                                                                                             { Name = "Col2"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (prefix asc/desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col1"; LayoutOrder = Asc }
                                                                                             { Name = "Col2"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (prefix index)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.2"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.1"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (prefix index asc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.1.asc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Asc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]

  [<Test>]
  let ``complex PK (prefix index desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.2.desc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                             { Name = "Col1"; LayoutOrder = Desc } ] }]

  [<Test>]
  let ``complex PK (prefix index asc/desc)`` () =
    testInput [ colDef "Col1" [ComplexAttr ("PK", ["PK1.2.asc"])]
                colDef "Col2" [ComplexAttr ("PK", ["PK1.1.desc"])] ]
    |> Key.collectTableKeys
    |> should equal [PrimaryKey { Name = "PK1_Table"; ClusteredType = Clustered; Columns = [ { Name = "Col2"; LayoutOrder = Desc }
                                                                                             { Name = "Col1"; LayoutOrder = Asc } ] }]
namespace TableDsl.Sql

open Basis.Core
open TableDsl

[<PrinterPlugin("sql")>]
module Printer =
  open System.IO
  open System.Text

  type ClusteredType with
    member this.Text =
      match this with
      | Clustered -> "CLUSTERED"
      | NonClustered -> "NONCLUSTERED"

  let printColumnTypeName (colDef: ColumnDef) =
    let rootType = ColumnDef.rootTypeName colDef
    let attrs = ColumnDef.attributes colDef
    let identity =
      match attrs |> Seq.tryFind (Attribute.key >> (=)"identity") with Some _ -> " IDENTITY(1, 1)" | _ -> ""
    let collate =
      match attrs |> Seq.tryFind (Attribute.key >> (=)"collate") with Some (ComplexAttr (_, [c])) -> " COLLATE " + c | _ -> ""
    rootType + identity + collate + (if ColumnDef.isNullable colDef then " NULL" else " NOT NULL")

  let printColumnDef col =
    let name = ColumnDef.actualName col
    "[" + name + "] " + (printColumnTypeName col)

  let printCreateTable table =
    "CREATE TABLE [" + table.TableDefName + "] (\n"
      + "    " + (table.ColumnDefs |> List.map printColumnDef |> Str.join "\n  , ")
      + "\n);"

  let attrs colDef =
    let colName = ColumnDef.actualName colDef
    colDef |> ColumnDef.attributes |> Seq.map (fun a -> (colName, a))

  let printCols cols =
    "    " + (cols |> List.map (fun col -> "[" + col + "]") |> Str.join "\n  , ")

  let printKeyCols (cols: SingleTableKeyColumn list) =
    "    " + (cols |> List.map (fun col -> "[" + col.Name + "]") |> Str.join "\n  , ")

  let printFKOwnCols (cols: ColumnPair list) =
    "    " + (cols |> List.map (fun col -> "[" + col.OwnColumnName + "]") |> Str.join "\n  , ")

  let printFKParentCols (cols: ColumnPair list) =
    "    " + (cols |> List.map (fun col -> "[" + col.ParentColumnName + "]") |> Str.join "\n  , ")

  let printKey tableName = function
  | PrimaryKey key ->
      "ALTER TABLE [" + tableName + "] ADD CONSTRAINT [" + key.Name + "] PRIMARY KEY " + key.ClusteredType.Text + " (\n" + (printKeyCols key.Columns) + "\n);"
  | UniqueKey key ->
      "ALTER TABLE [" + tableName + "] ADD CONSTRAINT [" + key.Name + "] UNIQUE " + key.ClusteredType.Text + " (\n" + (printKeyCols key.Columns) + "\n);"
  | Index key ->
      "CREATE " + key.ClusteredType.Text + " INDEX [" + key.Name + "] ON [" + tableName + "] (\n" + (printKeyCols key.Columns) + "\n);"
  | ForeignKey key ->
      "ALTER TABLE [" + tableName + "] ADD CONSTRAINT [" + key.KeyName + "] FOREIGN KEY (\n" +
        (printFKOwnCols key.ColumnPairs) + "\n) REFERENCES [" + key.RelationshipTableName + "] (\n" +
        (printFKParentCols key.ColumnPairs) + "\n) ON UPDATE NO ACTION\n  ON DELETE NO ACTION;"
  | DefaultConstraint d ->
      "ALTER TABLE [" + tableName + "] ADD CONSTRAINT [" + d.Name + "] DEFAULT (" + d.Value + ") FOR [" + d.ColumnName + "];"

  let printAlterTable tableDef =
    let keys = Key.collectTableKeys tableDef
    keys
    |> List.map (printKey tableDef.TableDefName)

  let printSummaryAndJpName _tableDef =
    None

  let printSql elems =
    let targets =
      elems |> List.choose (function TableDef t -> Some t | _ -> None)

    let createTable =
      targets
      |> List.map printCreateTable
      |> Str.join "\n"
    let alterTable =
      targets
      |> List.collect printAlterTable
      |> Str.join "\n"
    let summaryAndJpName =
      targets
      |> List.choose printSummaryAndJpName
      |> Str.join "\n"

    createTable
      + (if alterTable <> "" then "\n" + alterTable else "")
      + (if summaryAndJpName <> "" then "\n" + summaryAndJpName else "")

  let print (output: string option, _options: Map<string, string>, elems) =
    let printed = printSql elems
    match output with
    | Some output ->
        File.WriteAllText(output, printed, Encoding.UTF8)
    | None ->
        printfn "%s" printed
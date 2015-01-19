namespace TableDsl.Sql

open Basis.Core
open TableDsl

type AList<'TKey, 'TValue> = ('TKey * 'TValue) list

module AList =
  let add key value (kvs: AList<_, _>) : AList<_, _> =
    let rec add' acc = function
    | (k, _)::xs when k = key -> (key, value)::xs
    | x::xs -> x::(add' acc xs)
    | [] -> (key, value)::acc
    add' [] kvs

  let add2 key value (kvs: AList<_, _>) : AList<_, _> =
    let rec add' acc = function
    | (k, v)::xs when k = key -> (key, value::v)::xs
    | x::xs -> x::(add' acc xs)
    | [] -> (key, [value])::acc
    add' [] kvs

type ClusteredType = NonClustered | Clustered 
with
  override this.ToString() =
    match this with
    | NonClustered -> "NONCLUSTERED"
    | Clustered -> "CLUSTERED"

type LayoutOrder = Asc | Desc
with
  override this.ToString() =
    match this with
    | Asc -> "ASC"
    | Desc -> "Desc"

type IndexInfo = {
  ClusteredType: ClusteredType
  KeyNamePrefix: string
  ColumnIndex: int
  LayoutOrder: LayoutOrder
}
with
  override this.ToString() =
    (string this.ClusteredType) + "." + this.KeyNamePrefix + "." + (string this.ColumnIndex) + "." + (string this.LayoutOrder)
    
module PrimaryKey =
  let defaultIndexInfo =
    { ClusteredType = Clustered; KeyNamePrefix = "PK"; ColumnIndex = 0; LayoutOrder = Asc }

module UniqueKey =
  let defaultIndexInfo =
    { ClusteredType = NonClustered; KeyNamePrefix = "UQ"; ColumnIndex = 0; LayoutOrder = Asc }

module Index =
  let defaultIndexInfo =
    { ClusteredType = NonClustered; KeyNamePrefix = "IX"; ColumnIndex = 0; LayoutOrder = Asc }

type AlterTableKey =
  | PrimaryKey of ClusteredType * string
  | ForeignKey of string * string
  | UniqueKey of ClusteredType * string
  | Index of ClusteredType * string
  | Default of string

type AlterTableCol =
  | PrimaryKeyCol of int * string
  | ForeignKeyCol of int * string * string
  | UniqueKeyCol of int * string
  | IndexCol of int * string
  | DefaultCol of string * string

[<PrinterPlugin("sql")>]
module Printer =
  open System.IO
  open System.Text

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

  let indexInfo defaultInfo (value: string) =
    let int str =
      try int str with
      | e -> raise (System.FormatException(str + " can't convert to int.", e))
    match value.Split([|'.'|]) with
    | [| "clustered" |] -> { defaultInfo with ClusteredType = Clustered }
    | [| "clustered"; "asc" |] -> { defaultInfo with ClusteredType = Clustered; LayoutOrder = Asc }
    | [| "clustered"; "desc" |] -> { defaultInfo with ClusteredType = Clustered; LayoutOrder = Desc }
    | [| "clustered"; keyNamePrefix |] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = keyNamePrefix }
    | [| "clustered"; keyNamePrefix; index |] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index }
    | [| "clustered"; keyNamePrefix; index; "asc" |] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Asc }
    | [| "clustered"; keyNamePrefix; index; "desc" |] -> { defaultInfo with ClusteredType = Clustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Desc }
    | [| "nonclustered" |] -> { defaultInfo with ClusteredType = NonClustered }
    | [| "nonclustered"; "asc" |] -> { defaultInfo with ClusteredType = NonClustered; LayoutOrder = Asc }
    | [| "nonclustered"; "desc" |] -> { defaultInfo with ClusteredType = NonClustered; LayoutOrder = Desc }
    | [| "nonclustered"; keyNamePrefix |] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = keyNamePrefix }
    | [| "nonclustered"; keyNamePrefix; index |] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index }
    | [| "nonclustered"; keyNamePrefix; index; "asc" |] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Asc }
    | [| "nonclustered"; keyNamePrefix; index; "desc" |] -> { defaultInfo with ClusteredType = NonClustered; KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Desc }
    | [| "asc" |] -> { defaultInfo with LayoutOrder = Asc }
    | [| "desc" |] -> { defaultInfo with LayoutOrder = Desc }
    | [| keyNamePrefix |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix }
    | [| keyNamePrefix; "asc" |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix; LayoutOrder = Asc }
    | [| keyNamePrefix; "desc" |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix; LayoutOrder = Desc }
    | [| keyNamePrefix; index |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix; ColumnIndex = int index }
    | [| keyNamePrefix; index; "asc" |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Asc }
    | [| keyNamePrefix; index; "desc" |] -> { defaultInfo with KeyNamePrefix = keyNamePrefix; ColumnIndex = int index; LayoutOrder = Desc }
    | _ -> assert false; defaultInfo

  let addAlter tableName acc = function
  | col, SimpleAttr "PK" -> acc |> AList.add (PrimaryKey (Clustered, "PK_" + tableName)) [PrimaryKeyCol (0, col)]
  | col, ComplexAttr ("PK", [value]) ->
      let indexInfo = indexInfo PrimaryKey.defaultIndexInfo value
      acc |> AList.add2 (PrimaryKey (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (PrimaryKeyCol (indexInfo.ColumnIndex, col))
  | col, SimpleAttr "unique" -> acc |> AList.add (UniqueKey (NonClustered, "UQ_" + tableName)) [UniqueKeyCol (0, col)]
  | col, ComplexAttr ("unique", [value]) ->
      let indexInfo = indexInfo UniqueKey.defaultIndexInfo value
      acc |> AList.add2 (UniqueKey (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (UniqueKeyCol (indexInfo.ColumnIndex, col))
  | col, ComplexAttr ("FK", [value]) ->
      match value.Split([|'.'|]) with
      | [| parentTable; parentCol |] ->
          acc |> AList.add2 (ForeignKey (("FK_" + tableName + "_" + parentTable), parentTable)) (ForeignKeyCol (0, col, parentCol))
      | [| keyNamePrefix; parentTable; parentCol |] ->
          acc |> AList.add2 (ForeignKey ((keyNamePrefix + "_" + tableName + "_" + parentTable), parentTable)) (ForeignKeyCol (0, col, parentCol))
      | [| keyNamePrefix; order; parentTable; parentCol |] ->
          acc |> AList.add2 (ForeignKey ((keyNamePrefix + "_" + tableName + "_" + parentTable), parentTable)) (ForeignKeyCol (int order, col, parentCol))
      | _ -> assert false; failwith "oops!"
  | col, SimpleAttr "index" ->
      acc |> AList.add2 (Index (NonClustered, "IX_" + tableName)) (IndexCol (0, col))
  | col, ComplexAttr ("index", [value]) ->
      let indexInfo = indexInfo Index.defaultIndexInfo value
      acc |> AList.add2 (Index (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (IndexCol (indexInfo.ColumnIndex, col))
  | col, ComplexAttr ("default", [value]) ->
      acc |> AList.add (Default ("DF_" + tableName + "_" + col)) [DefaultCol (col, value)]
  | _col, SimpleAttr "identity"
  | _col, ComplexAttr ("identity", _)
  | _col, ComplexAttr ("collate", _)
  | _col, ComplexAttr ("average", _)
  | _col, ComplexAttr ("max", _) -> acc // do nothing here
  | _col, SimpleAttr _ -> failwith "not implemented"
  | _col, ComplexAttr _ -> failwith "not implemented"

  let printCols cols =
    "    " + (cols |> List.map (fun col -> "[" + col + "]") |> Str.join "\n  , ")

  let printPKCols cols =
    cols |> List.rev |> List.sortBy (fun (PrimaryKeyCol (order, _)) -> order) |> List.map (fun (PrimaryKeyCol (_, col)) -> col) |> printCols

  let printFKOwnCols cols =
    cols |> List.rev |> List.map (fun (ForeignKeyCol (_, col, _)) -> col) |> printCols

  let printFKParentCols cols =
    cols |> List.rev |> List.map (fun (ForeignKeyCol (_, _, col)) -> col) |> printCols

  let printUQCols cols =
    cols |> List.rev |> List.sortBy (fun (UniqueKeyCol (order, _)) -> order) |> List.map (fun (UniqueKeyCol (_, col)) -> col) |> printCols

  let printIXCols cols =
    cols |> List.rev |> List.sortBy (fun (IndexCol (order, _)) -> order) |> List.map (fun (IndexCol (_, col)) -> col) |> printCols

  let printAlterTable tableDef =
    let alters =
      tableDef.ColumnDefs
      |> Seq.collect attrs
      |> Seq.fold (addAlter tableDef.TableDefName) []

    alters
    |> List.map (fun (key, cols) ->
        match key with
        | PrimaryKey (clusteredType, name) ->
            "ALTER TABLE [" + tableDef.TableDefName + "] ADD CONSTRAINT [" + name + "] PRIMARY KEY " + (string clusteredType) + " (\n" + (printPKCols cols) + "\n);"
        | ForeignKey (name, parentTable) ->
            "ALTER TABLE [" + tableDef.TableDefName + "] ADD CONSTRAINT [" + name + "] FOREIGN KEY (\n" +
              (printFKOwnCols cols) + "\n) REFERENCES [" + parentTable + "] (\n" +
              (printFKParentCols cols) + "\n) ON UPDATE NO ACTION\n  ON DELETE NO ACTION;"
        | UniqueKey (clusteredType, name) ->
            "ALTER TABLE [" + tableDef.TableDefName + "] ADD CONSTRAINT [" + name + "] UNIQUE " + (string clusteredType) + " (\n" + (printUQCols cols) + "\n);"
        | Index (clusteredType, name) ->
            "CREATE " + (string clusteredType) + " INDEX [" + name + "] ON [" + tableDef.TableDefName + "] (\n" + (printIXCols cols) + "\n);"
        | Default name ->
            let col, value = cols |> List.map (fun (DefaultCol (c, v)) -> (c, v)) |> List.head
            "ALTER TABLE [" + tableDef.TableDefName + "] ADD CONSTRAINT [" + name + "] DEFAULT (" + value + ") FOR [" + col + "];"
       )

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
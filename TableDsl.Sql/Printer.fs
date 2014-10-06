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

module Printer =
  let printAttributeValue attrValueElems =
    let printAttrValueElem = function
    | Lit l -> l
    | Var v -> "oops!"

    attrValueElems |> List.map printAttrValueElem |> Str.concat

  let rec printNonEnumType attrs = function
  | { TypeName = "nullable"; TypeParameters = typeParams } ->
      let innerTypeName = typeParams |> List.map (function BoundValue v -> v | BoundType t -> printColumnTypeName [] t |> fst) |> Str.concat
      (innerTypeName, attrs, " NULL")
  | typ ->
      let typeParams =
        match typ.TypeParameters with
        | [] -> ""
        | notEmpty -> "(" + (notEmpty |> List.map (function (BoundValue v) -> v | _ -> "oops!") |> Str.join ", ") + ")"
      (typ.TypeName + typeParams, attrs, " NOT NULL")

  and columnTypeName attrs colTyp =
    match colTyp with
    | BuiltinType typ -> printNonEnumType attrs typ
    | AliasDef (_typ, orgType) -> columnTypeName (attrs @ orgType.ColumnAttributes) orgType.ColumnTypeDef
    | EnumTypeDef typ -> printNonEnumType attrs typ.BaseType

  and printCollate attrs =
    let collate = attrs |> List.tryPick (function ComplexAttr ("collate", v) -> Some v | _ -> None)
    match collate with
    | Some collate -> " COLLATE " + (printAttributeValue collate)
    | None -> ""

  and printColumnTypeName attrs (typ: ColumnTypeDef) =
    let attrs = attrs @ typ.ColumnAttributes
    let typeName, attrs, nullable = columnTypeName attrs typ.ColumnTypeDef
    (typeName + (printCollate attrs), nullable)

  let printColumnDef col =
    let typ, attrs = col.ColumnType
    let name =
      match col.ColumnName with
      | ColumnName (name, _) -> name
      | Wildcard ->
          match typ.ColumnTypeDef with
          | BuiltinType { TypeName = name }
          | AliasDef ({ TypeName = name }, _)
          | EnumTypeDef { EnumTypeName = name } -> name
    let typeName, nullable = printColumnTypeName attrs typ
    "[" + name + "] " + typeName + nullable

  let printCreateTable table =
    "CREATE TABLE [" + table.TableName + "] (\n"
      + "    " + (table.ColumnDefs |> List.map printColumnDef |> Str.join "\n  , ")
      + "\n);"

  let rec attrs'' colName = function
  | BuiltinType _ -> Seq.empty
  | AliasDef (_, originalType) -> attrs' colName originalType
  | EnumTypeDef _ -> Seq.empty

  and attrs' colName colTypeDef =
    let typeDef = colTypeDef.ColumnTypeDef
    let attrs = colTypeDef.ColumnAttributes
    seq { yield! attrs'' colName typeDef; for attr in attrs -> (colName, attr) }

  let attrs colDef =
    let typ, attrs = colDef.ColumnType
    let colName, attrs =
      match colDef.ColumnName with
      | Wildcard -> let name, attrs, _ = columnTypeName attrs typ.ColumnTypeDef in name, attrs
      | ColumnName (name, _) -> name, attrs
    seq { yield! attrs' colName typ; for attr in attrs -> (colName, attr) }

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
  | col, ComplexAttr ("PK", value) ->
      let value = printAttributeValue value
      let indexInfo = indexInfo PrimaryKey.defaultIndexInfo value
      acc |> AList.add2 (PrimaryKey (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (PrimaryKeyCol (indexInfo.ColumnIndex, col))
  | col, SimpleAttr "unique" -> acc |> AList.add (UniqueKey (NonClustered, "UQ_" + tableName)) [UniqueKeyCol (0, col)]
  | col, ComplexAttr ("unique", value) ->
      let value = printAttributeValue value
      let indexInfo = indexInfo UniqueKey.defaultIndexInfo value
      acc |> AList.add2 (UniqueKey (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (UniqueKeyCol (indexInfo.ColumnIndex, col))
  | col, ComplexAttr ("FK", value) ->
      let value = printAttributeValue value
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
  | col, ComplexAttr ("index", value) ->
      let value = printAttributeValue value
      let indexInfo = indexInfo Index.defaultIndexInfo value
      acc |> AList.add2 (Index (indexInfo.ClusteredType, indexInfo.KeyNamePrefix + "_" + tableName)) (IndexCol (indexInfo.ColumnIndex, col))
  | col, ComplexAttr ("default", value) ->
      let value = printAttributeValue value
      acc |> AList.add (Default ("DF_" + tableName + "_" + col)) [DefaultCol (col, value)]
  | _col, ComplexAttr ("collate", _value) -> acc // do nothing
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
      |> Seq.fold (addAlter tableDef.TableName) []

    alters
    |> List.map (fun (key, cols) ->
        match key with
        | PrimaryKey (clusteredType, name) ->
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] PRIMARY KEY " + (string clusteredType) + " (\n" + (printPKCols cols) + "\n);"
        | ForeignKey (name, parentTable) ->
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] FOREIGN KEY (\n" +
              (printFKOwnCols cols) + "\n) REFERENCES [" + parentTable + "] (\n" +
              (printFKParentCols cols) + "\n) ON UPDATE NO ACTION\n  ON DELETE NO ACTION;"
        | UniqueKey (clusteredType, name) ->
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] UNIQUE " + (string clusteredType) + " (\n" + (printUQCols cols) + "\n);"
        | Index (clusteredType, name) ->
            "CREATE " + (string clusteredType) + " INDEX [" + name + "] ON [" + tableDef.TableName + "] (\n" + (printIXCols cols) + "\n);"
        | Default name ->
            let col, value = cols |> List.map (fun (DefaultCol (c, v)) -> (c, v)) |> List.head
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] DEFAULT (" + value + ") FOR [" + col + "];"
       )

  let printSummaryAndJpName tableDef =
    None

  let print elems =
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
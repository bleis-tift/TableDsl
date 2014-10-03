﻿namespace TableDsl.Sql

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

type AlterTableKey =
  | PrimaryKey of string
  | ForeignKey of string * string

type AlterTableCol =
  | PrimaryKeyCol of int * string
  | ForeignKeyCol of int * string * string

module Printer =
  let printAttributeValue attrValueElems =
    let printAttrValueElem = function
    | Lit l -> l
    | Var v -> v

    attrValueElems |> List.map printAttrValueElem |> Str.concat

  let columnTypeName colTyp =
    match colTyp with
    | BuiltinType typ
    | AliasDef (typ, _) ->
        let typeParams =
          match typ.TypeParameters with
          | [] -> ""
          | notEmpty -> "(" + (notEmpty |> List.map (fun (BoundValue v) -> v) |> Str.join ", ") + ")"
        typ.TypeName + typeParams
    | EnumTypeDef typ -> typ.EnumTypeName

  let printColumnTypeName (typ: ColumnTypeDef) =
    let typeName = columnTypeName typ.ColumnTypeDef
    typeName + " NOT NULL"

  let printColumnDef col =
    let typ, _ = col.ColumnType
    let name =
      match col.ColumnName with
      | ColumnName (name, _) -> name
      | Wildcard -> columnTypeName typ.ColumnTypeDef
    "[" + name + "] " + (printColumnTypeName typ)

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
    let colName =
      match colDef.ColumnName with
      | Wildcard -> failwith "not implemented"
      | ColumnName (name, _) -> name
    seq { yield! attrs' colName typ; for attr in attrs -> (colName, attr) }

  let addAlter tableName acc = function
  | col, SimpleAttr "PK" -> acc |> AList.add (PrimaryKey ("PK_" + tableName)) [PrimaryKeyCol (0, col)]
  | _col, SimpleAttr _ -> failwith "not implemented"
  | col, ComplexAttr ("PK", value) ->
      let value = printAttributeValue value
      match value.Split([|'.'|], 2) with
      | [| keyNamePrefix; order |] -> acc |> AList.add2 (PrimaryKey (keyNamePrefix + "_" + tableName)) (PrimaryKeyCol (int order, col))
      | [| keyNamePrefix |] -> acc |> AList.add2 (PrimaryKey (keyNamePrefix + "_" + tableName)) (PrimaryKeyCol (0, col))
      | _ -> assert false; failwith "oops!"
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
  | _col, ComplexAttr _ -> failwith "not implemented"

  let printCols cols =
    "    " + (cols |> List.map (fun col -> "[" + col + "]") |> Str.join "\n  , ")

  let printPKCols cols =
    cols |> List.rev |> List.sortBy (fun (PrimaryKeyCol (order, _)) -> order) |> List.map (fun (PrimaryKeyCol (_, col)) -> col) |> printCols

  let printFKOwnCols cols =
    cols |> List.rev |> List.map (fun (ForeignKeyCol (_, col, _)) -> col) |> printCols

  let printFKParentCols cols =
    cols |> List.rev |> List.map (fun (ForeignKeyCol (_, _, col)) -> col) |> printCols

  let printAlterTable tableDef =
    let alters =
      tableDef.ColumnDefs
      |> Seq.collect attrs
      |> Seq.fold (addAlter tableDef.TableName) []

    alters
    |> List.map (fun (key, cols) ->
        match key with
        | PrimaryKey name ->
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] PRIMARY KEY CLUSTERED (\n" + (printPKCols cols) + "\n);"
        | ForeignKey (name, parentTable) ->
            "ALTER TABLE [" + tableDef.TableName + "] ADD CONSTRAINT [" + name + "] FOREIGN KEY (\n" +
              (printFKOwnCols cols) + "\n) REFERENCES [" + parentTable + "] (\n" +
              (printFKParentCols cols) + "\n) ON UPDATE NO ACTION\n  ON DELETE NO ACTION;")

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
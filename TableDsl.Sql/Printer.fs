namespace TableDsl.Sql

open Basis.Core
open TableDsl

module Printer =
  let columnTypeName colTyp =
    match colTyp with
    | BuiltinType typ
    | AliasDef (typ, _) -> typ.TypeName
    | EnumTypeDef typ -> typ.EnumTypeName

  let printColumnTypeName (typ: ClosedTypeRef) =
    let typeName = columnTypeName typ.Type.ColumnTypeDef
    let typeParams =
      match typ.TypeParameters with
      | [] -> ""
      | notEmpty -> "(" + (notEmpty |> Str.join ", ") + ")"
    typeName + typeParams + " NOT NULL"

  let printColumnDef col =
    let typ, _ = col.ColumnType
    let name =
      match col.ColumnName with
      | ColumnName (name, _) -> name
      | Wildcard -> columnTypeName typ.Type.ColumnTypeDef
    "[" + name + "] " + (printColumnTypeName typ)

  let printCreateTable table =
    "CREATE TABLE [" + table.TableName + "] (\n"
      + "    " + (table.ColumnDefs |> List.map printColumnDef |> Str.join "\n  , ")
      + "\n);"

  let printAlterTable table =
    ""

  let printSummaryAndJpName table =
    ""

  let print elems =
    let targets =
      elems |> List.choose (function TableDef t -> Some t | _ -> None)

    let createTable =
      targets
      |> List.map printCreateTable
      |> Str.join "\n"
    let alterTable =
      targets
      |> List.map printAlterTable
      |> Str.join "\n"
    let summaryAndJpName =
      targets
      |> List.map printSummaryAndJpName
      |> Str.join "\n"

    createTable
      + (if alterTable <> "" then "\n" + alterTable else "")
      + (if summaryAndJpName <> "" then "\n" + summaryAndJpName else "")
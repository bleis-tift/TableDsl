namespace TableDsl.Sql

open Basis.Core
open TableDsl

module Printer =
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

  let printAlterTable table =
    None

  let printSummaryAndJpName table =
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
      |> List.choose printAlterTable
      |> Str.join "\n"
    let summaryAndJpName =
      targets
      |> List.choose printSummaryAndJpName
      |> Str.join "\n"

    createTable
      + (if alterTable <> "" then "\n" + alterTable else "")
      + (if summaryAndJpName <> "" then "\n" + summaryAndJpName else "")
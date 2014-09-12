namespace TableDsl

open Basis.Core

module Printer =
  let printSummary indent = function
  | Some summary ->
      summary
      |> Str.splitBy "\n"
      |> Array.map (fun line -> (String.replicate indent " ") + "/// " + line)
      |> Str.join "\n"
  | None -> ""

  let printJpName = function
  | Some jpName -> "[" + jpName + "]"
  | None -> ""

  let printOpenTypeParam = function
  | TypeVariable v -> v
  | BoundValue v -> v

  let printOpenTypeParams = function
  | [] -> ""
  | typeVars -> "(" + (typeVars |> List.map printOpenTypeParam |> Str.join ", ") + ")"

  let printColumnTypeDef col =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> typ.TypeName
      | AliasDef (typ, originalType) -> failwith "Not implemented yet"
      | EnumTypeDef typ -> failwith "Not implemented yet"

  let printTypeDef = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (typ.TypeName + (printOpenTypeParams typ.TypeParameters), printColumnTypeDef originalType)
  | EnumTypeDef typ -> failwith "Not implemented yet"

  let printTableDef table = ""

  let printColTypeDef (col: ColumnTypeDef) =
    let summary = printSummary 0 col.ColumnSummary
    let jpName = printJpName col.ColumnJpName
    let name, body = printTypeDef col.ColumnTypeDef
    sprintf "%scoltype %s%s =%s" summary name jpName body

  let printElem = function
  | TableDef table -> printTableDef table
  | ColTypeDef col -> printColTypeDef col

  let print (elements: Element list) =
    elements |> List.map printElem |> Str.join "\n"
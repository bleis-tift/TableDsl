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

  let printAttributeValue attrValueElems =
    let printAttrValueElem = function
    | Lit l -> l
    | Var v -> v

    attrValueElems |> List.map printAttrValueElem |> Str.concat

  let printAttribute = function
  | SimpleAttr name -> name
  | ComplexAttr (name, value) -> name + " = " + (printAttributeValue value)

  let printAttributes typ = function
  | [] -> typ
  | attrs -> "{ " + typ + " with " + (attrs |> List.map printAttribute |> Str.join "; ") + " }"

  let printNonEnumTypeName nonEnumType =
    nonEnumType.TypeName + (printOpenTypeParams nonEnumType.TypeParameters)

  let printColumnTypeDef col typeParams attrs =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ) attrs
      | AliasDef (typ, originalType) -> failwith "Not implemented yet"
      | EnumTypeDef typ -> failwith "Not implemented yet"

  let printEnumCases cases =
    cases |> List.map (fun (name, value) -> "  | " + name + " = " + string value) |> Str.join "\n"

  let printEnumTypeBody enumType attrs =
    let baseType = printNonEnumTypeName enumType.BaseType
    "\n" + (printEnumCases enumType.Cases) + "\nbased " + (printAttributes baseType attrs)

  let printTypeDef attrs = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (printNonEnumTypeName typ, printColumnTypeDef originalType typ.TypeParameters attrs)
  | EnumTypeDef typ -> (typ.EnumTypeName, printEnumTypeBody typ attrs)

  let printTableDef table = ""

  let printColTypeDef (col: ColumnTypeDef) =
    let summary = printSummary 0 col.ColumnSummary
    let jpName = printJpName col.ColumnJpName
    let name, body = printTypeDef col.ColumnAttributes col.ColumnTypeDef
    sprintf "%scoltype %s%s =%s" summary name jpName body

  let printElem = function
  | TableDef table -> printTableDef table
  | ColTypeDef col -> printColTypeDef col

  let print (elements: Element list) =
    elements |> List.map printElem |> Str.join "\n"
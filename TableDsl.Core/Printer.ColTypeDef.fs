namespace TableDsl.Printer

open Basis.Core
open TableDsl
open TableDsl.Printer.Primitives

module internal ColTypeDef =
  let printNonEnumTypeName (nonEnumType: NonEnumType) =
    nonEnumType.TypeName + (printOpenTypeParams nonEnumType.TypeParameters)

  let printColumnTypeDef attrs col =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ) attrs
      | AliasDef (typ, _originalType) -> printAttributes (printNonEnumTypeName typ) attrs
      | EnumTypeDef _typ -> failwith "Not implemented yet"

  let printEnumCases cases =
    let printJpName = function Some v -> "[" + v + "]" | None -> ""
    cases |> List.map (fun (name, jpName, value) -> "  | " + name + printJpName jpName + " = " + string value) |> Str.join "\n"

  let printEnumTypeBody enumType attrs =
    let baseType = printNonEnumTypeName enumType.BaseType
    "\n" + (printEnumCases enumType.Cases) + "\nbased " + (printAttributes baseType attrs)

  let printTypeDef attrs = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (printNonEnumTypeName typ, printColumnTypeDef attrs originalType)
  | EnumTypeDef typ -> (typ.EnumTypeName, printEnumTypeBody typ attrs)

  let print (col: ColumnTypeDef) =
    let summary = printSummary 0 col.ColumnTypeDefSummary
    let jpName = printJpName col.ColumnTypeDefJpName
    let name, body = printTypeDef col.ColumnTypeDefAttributes col.ColumnTypeDef
    sprintf "%scoltype %s%s =%s" summary name jpName body
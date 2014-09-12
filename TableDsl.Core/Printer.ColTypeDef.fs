namespace TableDsl.Printer

open Basis.Core
open TableDsl
open TableDsl.Printer.Primitives

module internal ColTypeDef =
  let printNonEnumTypeName (nonEnumType: NonEnumType) =
    nonEnumType.TypeName + (printOpenTypeParams nonEnumType.TypeParameters)

  let printColumnTypeDef col attrs =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ) attrs
      | AliasDef (typ, originalType) -> printAttributes (printNonEnumTypeName typ) attrs
      | EnumTypeDef typ -> failwith "Not implemented yet"

  let printEnumCases cases =
    cases |> List.map (fun (name, value) -> "  | " + name + " = " + string value) |> Str.join "\n"

  let printEnumTypeBody enumType attrs =
    let baseType = printNonEnumTypeName enumType.BaseType
    "\n" + (printEnumCases enumType.Cases) + "\nbased " + (printAttributes baseType attrs)

  let printTypeDef attrs = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (printNonEnumTypeName typ, printColumnTypeDef originalType attrs)
  | EnumTypeDef typ -> (typ.EnumTypeName, printEnumTypeBody typ attrs)

  let print (col: ColumnTypeDef) =
    let summary = printSummary 0 col.ColumnSummary
    let jpName = printJpName col.ColumnJpName
    let name, body = printTypeDef col.ColumnAttributes col.ColumnTypeDef
    sprintf "%scoltype %s%s =%s" summary name jpName body
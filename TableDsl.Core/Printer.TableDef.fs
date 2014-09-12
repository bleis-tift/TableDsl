namespace TableDsl.Printer

open Basis.Core
open TableDsl
open TableDsl.Printer.Primitives

module TableDef =
  let printNonEnumTypeName (nonEnumType: NonEnumType) typeParams =
    match nonEnumType.TypeParameters with
    | [] -> nonEnumType.TypeName
    | other -> nonEnumType.TypeName + (printOpenTypeParams typeParams)

  let printColumnTypeDef col typeParams attrs =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ typeParams) attrs
      | AliasDef (typ, originalType) -> printAttributes (printNonEnumTypeName typ typeParams) attrs
      | EnumTypeDef typ -> failwith "Not implemented yet"

  let printEnumCases cases =
    cases |> List.map (fun (name, value) -> "  | " + name + " = " + string value) |> Str.join "\n"

  let printEnumTypeBody enumType attrs =
    let baseType = printNonEnumTypeName enumType.BaseType []
    "\n" + (printEnumCases enumType.Cases) + "\nbased " + (printAttributes baseType attrs)

  let printTypeDef attrs = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (printNonEnumTypeName typ typ.TypeParameters, printColumnTypeDef originalType typ.TypeParameters attrs)
  | EnumTypeDef typ -> (typ.EnumTypeName, printEnumTypeBody typ attrs)

  let printColumnName = function
  | Wildcard -> "_"
  | ColumnName (name, jpName) -> name + (printJpName jpName)

  let printColumnType (typ, attrs) =
    printColumnTypeDef typ.Type (typ.TypeParameters |> List.map BoundValue) attrs

  let printColumnDef colDef =
    let summary = printSummary 2 colDef.ColumnSummary
    let name = printColumnName colDef.ColumnName
    let typ = printColumnType colDef.ColumnType
    sprintf "%s  %s:%s" summary name typ

  let print table =
    let summary = printSummary 0 table.TableSummary
    let name = table.TableName
    let jpName = printJpName table.TableJpName
    let body = table.ColumnDefs |> List.map printColumnDef |> Str.join "\n"
    sprintf "%stable %s%s = {\n%s\n}" summary name jpName body

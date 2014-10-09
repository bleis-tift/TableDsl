namespace TableDsl.Printer

open Basis.Core
open TableDsl
open TableDsl.Printer.Primitives

module TableDef =
  let rec printNonEnumTypeName (nonEnumType: NonEnumType) =
    match nonEnumType.TypeParameters with
    | [] -> nonEnumType.TypeName
    | other -> nonEnumType.TypeName + (printOpenTypeParams (printColumnTypeDef []) nonEnumType.TypeParameters)

  and printEnumCases cases =
    cases |> List.map (fun (name, value) -> "  | " + name + " = " + string value) |> Str.join "\n"

  and printEnumTypeBody enumType attrs =
    let baseType = printNonEnumTypeName enumType.BaseType
    "\n" + (printEnumCases enumType.Cases) + "\nbased " + (printAttributes baseType attrs)

  and printColumnTypeDef attrs col =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ) attrs
      | AliasDef (typ, originalType) -> printAttributes (printNonEnumTypeName typ) attrs
      | EnumTypeDef typ -> printAttributes typ.EnumTypeName attrs

  let printTypeDef attrs = function
  | BuiltinType _ -> failwith "組み込み型をcoltypeとして出力することはできません。"
  | AliasDef (typ, originalType) -> (printNonEnumTypeName typ, printColumnTypeDef attrs originalType)
  | EnumTypeDef typ -> (typ.EnumTypeName, printEnumTypeBody typ attrs)

  let printColumnName = function
  | Wildcard -> "_"
  | ColumnName (name, jpName) -> name + (printJpName jpName)

  let printColumnType (typ, attrs) =
    printColumnTypeDef attrs typ

  let printColumnDef colDef =
    let summary = printSummary 2 colDef.ColumnSummary
    let name = printColumnName colDef.ColumnName
    let typ = printColumnType colDef.ColumnType
    sprintf "%s  %s:%s" summary name typ

  let printTableAttributes attrs =
    attrs
    |> List.map (function
                 | SimpleTableAttr name -> "[<" + name + ">]"
                 | ComplexTableAttr (name, values) -> "[<" + name + "(" + (values |> Str.join ", ") + ")>]")
    |> Str.join "\n"

  let print table =
    let summary = printSummary 0 table.TableSummary
    let attrs = printTableAttributes table.TableAttributes
    let name = table.TableName
    let jpName = printJpName table.TableJpName
    let body = table.ColumnDefs |> List.map printColumnDef |> Str.join "\n"
    sprintf "%s%stable %s%s = {\n%s\n}" summary attrs name jpName body

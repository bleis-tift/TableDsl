namespace TableDsl.Printer

open Basis.Core
open TableDsl
open TableDsl.Printer.Primitives

module TableDef =
  let printColumnAttribute = function
  | SimpleAttr name -> name
  | ComplexAttr (name, value) -> name + " = " + value.[0]

  let printColumnAttributes typ = function
  | [] -> typ
  | attrs -> "{ " + typ + " with " + (attrs |> List.map printColumnAttribute |> Str.join "; ") + " }"

  let printNullable typ = "nullable(" + typ + ")"

  let rec printNonEnumTypeName (nonEnumType: NonEnumType) =
    match nonEnumType.TypeParameters with
    | [] -> nonEnumType.TypeName
    | _other -> nonEnumType.TypeName + (printOpenTypeParams (printColumnTypeDef []) nonEnumType.TypeParameters)

  and printColumnTypeDef attrs col =
    " " +
      match col.ColumnTypeDef with
      | BuiltinType typ -> printAttributes (printNonEnumTypeName typ) attrs
      | AliasDef (typ, _originalType) -> printAttributes (printNonEnumTypeName typ) attrs
      | EnumTypeDef typ -> printAttributes typ.EnumTypeName attrs

  let printColumnTypeRef (col: ColumnTypeRef) =
    let typ =
      col.ColumnTypeRefName +
      if col.ColumnTypeRefParams.IsEmpty then "" else ("(" + (col.ColumnTypeRefParams |> String.concat ", ") + ")")
    " " +
      printColumnAttributes (if col.IsNullable then printNullable typ else typ) col.ColumnTypeRefAttributes

  let printColumnName = function
  | Wildcard -> "_"
  | ColumnName (name, jpName) -> name + (printJpName jpName)

  let printColumnDef colDef =
    let summary = printSummary 2 colDef.ColumnSummary
    let name = printColumnName colDef.ColumnName
    let typ = printColumnTypeRef colDef.ColumnType
    sprintf "%s  %s:%s" summary name typ

  let printTableAttributes attrs =
    attrs
    |> List.map (function
                 | SimpleAttr name -> "[<" + name + ">]"
                 | ComplexAttr (name, values) -> "[<" + name + "(" + (values |> Str.join ", ") + ")>]")
    |> Str.join "\n"

  let print table =
    let summary = printSummary 0 table.TableSummary
    let attrs = printTableAttributes table.TableAttributes
    let name = table.TableName
    let jpName = printJpName table.TableJpName
    let body = table.ColumnDefs |> List.map printColumnDef |> Str.join "\n"
    sprintf "%s%stable %s%s = {\n%s\n}" summary attrs name jpName body

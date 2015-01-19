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

  let printNonEnumTypeName (nonEnumType: NonEnumType) =
    match nonEnumType.TypeParameters with
    | [] -> nonEnumType.TypeName
    | _other -> nonEnumType.TypeName + (printOpenTypeParams nonEnumType.TypeParameters)

  let printColumnTypeRef (col: ColumnType) =
    let typ =
      col.ColumnTypeName +
      if col.ColumnTypeParams.IsEmpty then "" else ("(" + (col.ColumnTypeParams |> String.concat ", ") + ")")
    " " +
      printColumnAttributes (if col.IsNullable then printNullable typ else typ) col.ColumnTypeAttributes

  let printColumnName = function
  | Wildcard -> "_"
  | ColumnName (name, jpName) -> name + (printJpName jpName)

  let printColumnDef colDef =
    let summary = printSummary 2 colDef.ColumnDefSummary
    let name = printColumnName colDef.ColumnDefName
    let typ = printColumnTypeRef colDef.ColumnDefType
    sprintf "%s  %s:%s" summary name typ

  let printTableAttributes attrs =
    attrs
    |> List.map (function
                 | SimpleAttr name -> "[<" + name + ">]"
                 | ComplexAttr (name, values) -> "[<" + name + "(" + (values |> Str.join ", ") + ")>]")
    |> Str.join "\n"

  let print table =
    let summary = printSummary 0 table.TableDefSummary
    let attrs = printTableAttributes table.TableDefAttributes
    let name = table.TableDefName
    let jpName = printJpName table.TableDefJpName
    let body = table.ColumnDefs |> List.map printColumnDef |> Str.join "\n"
    sprintf "%s%stable %s%s = {\n%s\n}" summary attrs name jpName body

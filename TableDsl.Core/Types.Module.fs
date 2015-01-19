namespace TableDsl

open TableDsl.Extensions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Attribute =
  let key = function SimpleAttr k | ComplexAttr (k, _) -> k

  let toTuple = function SimpleAttr k -> (k, []) | ComplexAttr (k, v) -> (k, v)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColumnDef =
  let isNullable colDef = colDef.ColumnDefType.IsNullable

  let summary colDef =
    match colDef.ColumnDefSummary with
    | Some summary -> Some summary
    | None -> colDef.ColumnDefType.Summary

  let actualName colDef =
    match colDef.ColumnDefName with
    | Wildcard -> colDef.ColumnDefType.ColumnTypeName
    | ColumnName (name, _) -> name

  let jpName colDef =
    match colDef.ColumnDefName with
    | Wildcard | ColumnName (_, None) -> colDef.ColumnDefType.JpName
    | ColumnName (_, jpName) -> jpName

  let rootTypeName colDef = colDef.ColumnDefType.RootTypeName

  let typeRefName colDef = colDef.ColumnDefType.TypeRefName

  let attributes colDef = colDef.ColumnDefType.AllAttrbutes

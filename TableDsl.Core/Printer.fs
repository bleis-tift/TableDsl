namespace TableDsl

open Basis.Core
open TableDsl.Printer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printer =
  let printElem = function
  | TableDef table -> TableDef.print table
  | ColTypeDef col -> ColTypeDef.print col

  let print (elements: Element list) =
    elements |> List.map printElem |> Str.join "\n"
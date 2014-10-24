namespace TableDsl

open Basis.Core
open TableDsl.Printer

type PrinterPluginAttribute (name: string) =
  inherit System.Attribute()

  member __.Name = name

[<PrinterPlugin("tabledsl")>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printer =
  let private printElem = function
  | TableDef table -> TableDef.print table
  | ColTypeDef col -> ColTypeDef.print col

  let print (elements: Element list) =
    elements |> List.map printElem |> Str.join "\n"
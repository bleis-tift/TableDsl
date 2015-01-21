namespace TableDsl

open System.IO
open System.Text
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

  let printElems (elements: Element list) =
    elements |> List.map printElem |> Str.join "\n"

  let print (output: string option, _options: Map<string, string>, elems) =
    let printed = printElems elems
    match output with
    | Some output ->
        File.WriteAllText(output, printed, Encoding.UTF8)
    | None ->
        printfn "%s" printed
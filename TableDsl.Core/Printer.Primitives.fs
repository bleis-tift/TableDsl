namespace TableDsl.Printer

open Basis.Core
open TableDsl

module internal Primitives =
  let printSummary indent = function
  | Some summary ->
      let indent = String.replicate indent " "
      let summary =
        summary
        |> Str.splitBy "\n"
        |> Array.map (fun line -> indent + "/// " + line)
        |> Str.join "\n"
      summary + "\n"
  | None -> ""

  let printJpName = function
  | Some jpName -> "[" + jpName + "]"
  | None -> ""

  let printOpenTypeParam = function
  | TypeVariable v
  | BoundValue v -> v

  let printOpenTypeParams = function
  | [] -> ""
  | typeVars -> "(" + (typeVars |> List.map printOpenTypeParam |> Str.join ", ") + ")"

  let printAttributeValue attrValueElems =
    let printAttrValueElem = function
    | Lit l -> l
    | Var v -> v

    attrValueElems |> List.map printAttrValueElem |> Str.concat

  let printAttribute = function
  | SimpleColAttr name -> name
  | ComplexColAttr (name, value) -> name + " = " + (printAttributeValue value)

  let printAttributes typ = function
  | [] -> typ
  | attrs -> "{ " + typ + " with " + (attrs |> List.map printAttribute |> Str.join "; ") + " }"

namespace TableDsl.RuleCheckerPlugins

open TableDsl
open System.Text.RegularExpressions
open Basis.Core

[<RuleCheckerPlugin("table-name-s", RuleLevel.Warning)>]
module TableNameS =
  let specialRules name =
    let name = name |> Str.toLower
    if Regex.IsMatch(name, @"[ml]ice$") then true
    elif Regex.IsMatch(name, @"[ti]a$") then true
    elif Regex.IsMatch(name, @"[nrlm]ese$") then true
    elif name |> Str.endsWith "people" then true
    elif name |> Str.endsWith "men" then true
    elif name |> Str.endsWith "children" then true
    elif name |> Str.endsWith "deer" then true
    elif name |> Str.endsWith "fish" then true
    elif name |> Str.endsWith "pox" then true
    elif name |> Str.endsWith "sheep" then true
    elif name |> Str.endsWith "genera" then true
    elif name |> Str.endsWith "graffiti" then true
    elif name |> Str.endsWith "mythoi" then true
    elif name |> Str.endsWith "numina" then true
    elif name |> Str.endsWith "teeth" then true
    elif name |> Str.endsWith "geese" then true
    elif name |> Str.endsWith "feet" then true
    elif name |> Str.endsWith "oxen" then true
    else false

  let singularName level excludes name =
    if name |> Str.endsWith "s" then None
    elif specialRules name then None
    elif excludes |> Array.exists ((=)name) then None
    else Some { Level = level; Message = sprintf "テーブル名に単数形が使われています: %s" name }

  let check (level: RuleLevel) (arg: string) (elems: Element list) : CheckResult list =
    let tableNames =
      elems |> Seq.choose (function TableDef t -> Some t.TableName | _ -> None)
    tableNames
    |> Seq.choose (singularName level (arg.Split(',') |> Array.map Str.trim))
    |> Seq.toList
namespace TableDsl.RuleCheckerPlugins

open TableDsl
open System.Text.RegularExpressions
open Basis.Core

[<RuleCheckerPlugin("table-name-s", RuleLevel.Warning, "")>]
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

  let check (level: RuleLevel) (arg: string) (elems: Element list) : DetectedItem list =
    let tableNames =
      elems |> Seq.choose (function TableDef t -> Some t.TableName | _ -> None)
    tableNames
    |> Seq.choose (singularName level (arg.Split(',') |> Array.map Str.trim))
    |> Seq.toList

[<RuleCheckerPlugin("snake-case", RuleLevel.Warning, "")>]
module SnakeCase =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let names =
      elems
      |> Seq.collect (function
                      | TableDef t -> seq { yield ("テーブル名", t.TableName)
                                            yield! t.ColumnDefs
                                                   |> Seq.map (fun cd -> match cd.ColumnName with
                                                                         | Wildcard -> ("列名", cd.ColumnType.ColumnTypeRefName)
                                                                         | ColumnName (n, _) -> ("列名", n)) }
                      | _ -> Seq.empty)
    names
    |> Seq.filter (snd >> Str.contains "_")
    |> Seq.map (fun (kind, n) -> { Level = level; Message = sprintf "%sにスネークケースが使われています: %s" kind n })
    |> Seq.toList

[<RuleCheckerPlugin("upper-case", RuleLevel.Warning, "")>]
module UpperCase =
  open System

  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let names =
      elems
      |> Seq.collect (function
                      | TableDef t -> seq { yield ("テーブル名", t.TableName)
                                            yield! t.ColumnDefs
                                                   |> Seq.map (fun cd -> match cd.ColumnName with
                                                                         | Wildcard -> ("列名", cd.ColumnType.ColumnTypeRefName)
                                                                         | ColumnName (n, _) -> ("列名", n)) }
                      | _ -> Seq.empty)
    names
    |> Seq.filter (snd >> Str.forall Char.IsUpper)
    |> Seq.map (fun (kind, n) -> { Level = level; Message = sprintf "%sにスネークケースが使われています: %s" kind n })
    |> Seq.toList
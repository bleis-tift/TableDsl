namespace TableDsl.RuleCheckerPlugins

open TableDsl
open System
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
    if name |> Str.toLower |> Str.endsWith "s" then None
    elif specialRules name then None
    elif excludes |> Array.exists ((=)name) then None
    else Some { Level = level; Message = sprintf "テーブル名に単数形が使われています: %s" name }

  let check (level: RuleLevel) (arg: string) (elems: Element list) : DetectedItem list =
    let tableNames =
      elems |> Seq.choose (function TableDef t -> Some t.TableName | _ -> None)
    tableNames
    |> Seq.choose (singularName level (arg.Split(',') |> Array.map Str.trim))
    |> Seq.toList

module private Util =
  let names (elems: Element list) =
    elems
    |> Seq.collect (function
                    | TableDef t -> seq { yield ("テーブル名", t.TableName)
                                          yield! t.ColumnDefs
                                                 |> Seq.map (fun cd -> match cd.ColumnName with
                                                                       | Wildcard -> ("列名", cd.ColumnType.ColumnTypeRefName)
                                                                       | ColumnName (n, _) -> ("列名", n)) }
                    | _ -> Seq.empty)

  let check level _arg format pred elems  =
    let names = names elems
    names
    |> Seq.filter (snd >> pred)
    |> Seq.map (fun (kind, n) -> { Level = level; Message = sprintf format kind n })
    |> Seq.toList

[<RuleCheckerPlugin("snake-case", RuleLevel.Warning, "")>]
module SnakeCase =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    Util.check level _arg "%sにスネークケースが使われています: %s" (Str.contains "_") elems

[<RuleCheckerPlugin("upper-case", RuleLevel.Warning, "")>]
module UpperCase =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    Util.check level _arg "大文字のみの%sがあります: %s" (Str.forall Char.IsUpper) elems

[<RuleCheckerPlugin("lower-case", RuleLevel.Warning, "")>]
module LowerCase =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    Util.check level _arg "小文字のみの%sがあります: %s" (Str.forall Char.IsLower) elems

[<RuleCheckerPlugin("jp-name", RuleLevel.Warning, "")>]
module JpName =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let jpNames =
      elems
      |> Seq.collect (function
                      | TableDef t -> seq { yield ("テーブル", t.TableName, t.TableJpName)
                                            yield! t.ColumnDefs
                                                   |> Seq.map (fun cd -> match cd.ColumnName with
                                                                         | Wildcard -> ("列", cd.ColumnType.ColumnTypeRefName, cd.ColumnType.Type.ColumnType.ColumnTypeJpName)
                                                                         | ColumnName (n, jpName) -> ("列", n, jpName)) }
                      | _ -> Seq.empty)
    jpNames
    |> Seq.choose (function (x, name, None) -> Some { Level = level; Message = sprintf "日本語名を持たない%sがあります: %s" x name } | _ -> None)
    |> Seq.toList
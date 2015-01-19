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
      elems |> Seq.choose (function TableDef t -> Some t.TableDefName | _ -> None)
    tableNames
    |> Seq.choose (singularName level (arg.Split(',') |> Array.map Str.trim))
    |> Seq.toList

module private Util =
  let names (elems: Element list) =
    elems
    |> Seq.collect (function
                    | TableDef t -> seq { yield ("テーブル名", t.TableDefName)
                                          yield! t.ColumnDefs
                                                 |> Seq.map (fun cd -> ("列名", ColumnDef.actualName cd)) }
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
                      | TableDef t -> seq { yield ("テーブル", t.TableDefName, t.TableDefJpName)
                                            yield! t.ColumnDefs
                                                   |> Seq.map (fun cd -> ("列", ColumnDef.actualName cd, ColumnDef.jpName cd)) }
                      | _ -> Seq.empty)
    jpNames
    |> Seq.choose (function (x, name, None) -> Some { Level = level; Message = sprintf "日本語名を持たない%sがあります: %s" x name } | _ -> None)
    |> Seq.toList

[<RuleCheckerPlugin("table-summary", RuleLevel.Warning, "")>]
module TableSummary =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let summaries =
      elems
      |> Seq.collect (function TableDef t -> seq { yield (t.TableDefName, t.TableDefSummary) } | _ -> Seq.empty)
    summaries
    |> Seq.choose (function (name, None) -> Some { Level = level; Message = sprintf "サマリを持たないテーブルがあります: %s" name } | _ -> None)
    |> Seq.toList

[<RuleCheckerPlugin("invalid-prefix", RuleLevel.Fatal, "")>]
module InvalidPrefix =
  let prefix value =
    match value |> Str.splitBy "." with
    | [| "clustered"; "asc" |] | [| "clustered"; "desc" |] -> None
    | [| "clustered"; keyNamePrefix |]
    | [| "clustered"; keyNamePrefix; _ |]
    | [| "clustered"; keyNamePrefix; _; "asc" |]
    | [| "clustered"; keyNamePrefix; _; "desc" |] -> Some keyNamePrefix
    | [| "nonclustered"; "asc" |] | [| "nonclustered"; "desc" |] -> None
    | [| "nonclustered"; keyNamePrefix |]
    | [| "nonclustered"; keyNamePrefix; _ |]
    | [| "nonclustered"; keyNamePrefix; _; "asc" |]
    | [| "nonclustered"; keyNamePrefix; _; "desc" |] -> Some keyNamePrefix
    | [| "asc" |] | [| "desc" |] -> None
    | [| keyNamePrefix |]
    | [| keyNamePrefix; _ |]
    | [| keyNamePrefix; _; "asc" |]
    | [| keyNamePrefix; _; "desc" |] -> Some keyNamePrefix
    | _ -> None

  let prefixForFK value =
    match value |> Str.splitBy "." with
    | [| keyNamePrefix; _; _ |]
    | [| keyNamePrefix; _; _; _ |] -> Some keyNamePrefix
    | _ -> None

  let invalidPrimaryKeyPrefix value = prefix value |> Option.bind (fun prefix -> if prefix |> Str.startsWith "PK" then None else Some prefix)
  let invalidForeignKeyPrefix value = prefixForFK value |> Option.bind (fun prefix -> if prefix |> Str.startsWith "FK" then None else Some prefix)
  let invalidUniqueKeyPrefix value = prefix value |> Option.bind (fun prefix -> if prefix |> Str.startsWith "UQ" then None else Some prefix)
  let invalidIndexPrefix value = prefix value |> Option.bind (fun prefix -> if prefix |> Str.startsWith "IX" then None else Some prefix)

  let check (_level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let indexInfos =
      seq {
        for table in elems |> List.choose (function TableDef t -> Some t | _ -> None) do
          for col in table.ColumnDefs do
            for attr in col |> ColumnDef.attributes do
              match attr with
              | ComplexAttr (key, [value]) -> yield (table.TableDefName, ColumnDef.actualName col, key, value)
              | _ -> ()
      }
    let invalidPrefixIndexInfos =
      indexInfos
      |> Seq.choose (function
                     | (table, col, "PK", value) -> match invalidPrimaryKeyPrefix value with Some p -> Some (table, col, "PK", p) | _ -> None
                     | (table, col, "FK", value) -> match invalidForeignKeyPrefix value with Some p -> Some (table, col, "FK", p) | _ -> None
                     | (table, col, "unique", value) -> match invalidUniqueKeyPrefix value with Some p -> Some (table, col, "unique", p) | _ -> None
                     | (table, col, "index", value) -> match invalidIndexPrefix value with Some p -> Some (table, col, "index", p) | _ -> None
                     | _ -> None)
    invalidPrefixIndexInfos
    |> Seq.map (function
                | (table, col, "PK", value) -> { Level = RuleLevel.Fatal; Message = sprintf "主キーのプレフィックスはPKですが、%sが指定されました: %s.%s" value table col }
                | (table, col, "FK", value) -> { Level = RuleLevel.Fatal; Message = sprintf "外部キーのプレフィックスはFKですが、%sが指定されました: %s.%s" value table col }
                | (table, col, "unique", value) -> { Level = RuleLevel.Fatal; Message = sprintf "一意制約のプレフィックスはUQですが、%sが指定されました: %s.%s" value table col }
                | (table, col, "index", value) -> { Level = RuleLevel.Fatal; Message = sprintf "インデックスのプレフィックスはIXですが、%sが指定されました: %s.%s" value table col }
                | other -> failwithf "oops!: %A" other)
    |> Seq.toList

[<RuleCheckerPlugin("char-enum", RuleLevel.Warning, "")>]
module CharEnum =
  let check (level: RuleLevel) (_arg: string) (elems: Element list) : DetectedItem list =
    let enums =
      elems
      |> Seq.choose (function ColTypeDef { ColumnTypeDef = (EnumTypeDef enum) } -> Some enum | _ -> None)
    enums
    |> Seq.choose (fun enum ->
         match enum.BaseType.TypeName with
         | "char" | "varchar" | "text" | "ntext" | "nchar" | "nvarchar" ->
             Some { Level = level; Message = sprintf "文字型をベースにした列挙型があります: %s" enum.EnumTypeName }
         | _ -> None)
    |> Seq.toList
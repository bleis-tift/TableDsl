open System
open System.IO
open System.Text
open System.Reflection

type SubCommandType =
  | Print
  | Check

type Args = {
  SubCommandType: SubCommandType
  TargetFile: string
  Options: Map<string, string>
}

let (|StartsWith|_|) (prefix: string) (target: string) =
  if target.StartsWith(prefix) then
    Some (target.Substring(prefix.Length))
  else
    None

let (|Split2By|_|) (separator: string) (target: string) =
  match target.Split([| separator |], 2, StringSplitOptions.None) with
  | [| s1; s2 |] -> Some (s1, s2)
  | _ -> None

let parse argv =
  let rec parse' acc = function
  | (StartsWith "--" (Split2By ":" (key, value)))::rest ->
      parse' { acc with Options = acc.Options |> Map.add key value } rest
  | targetFile::rest -> parse' { acc with TargetFile = targetFile } rest
  | [] -> acc

  match argv with
  | "print"::argv -> parse' { SubCommandType = Print; TargetFile = ""; Options = Map.empty } argv
  | "check"::argv -> parse' { SubCommandType = Check; TargetFile = ""; Options = Map.empty } argv
  | other::_ -> failwithf "unknown subcommand: %s" other
  | [] -> failwith "please input subcommand."

let getPrinterFunction (typ: Type) =
  let f (m: MethodInfo) (output: string option, options: Map<string, string>, elems: TableDsl.Element list) =
    m.Invoke(null, [| output; options; elems |]) |> ignore

  f (typ.GetMethod("print"))

let getRuleCheckerFunction (attr: TableDsl.RuleCheckerPluginAttribute) (typ: Type) =
  let f (m: MethodInfo) (elems: TableDsl.Element list) =
    m.Invoke(null, [| attr.Arg; elems |]) :?> TableDsl.CheckResult list

  f (typ.GetMethod("check"))

let tryLoadPlugin f =
  seq {
    for dll in Directory.GetFiles("plugins", "*.dll") do
      let asm = Assembly.LoadFrom(dll)
      for typ in asm.GetTypes() -> typ
  }
  |> Seq.tryPick (fun typ ->
       let attrs = typ.GetCustomAttributes(true)
       attrs |> Array.tryPick (f typ)
     )

let print targetFile options =
  match options |> Map.tryFind "format" with
  | Some (format: string) ->
      let input = File.ReadAllText(targetFile, Encoding.UTF8)
      let elems = TableDsl.Parser.parse input
      let format = format.ToLower()
      if format = "tabledsl" then
        let printed = TableDsl.Printer.print elems
        match options |> Map.tryFind "output" with
        | Some output ->
            File.WriteAllText(output, printed, Encoding.UTF8)
        | None ->
            printfn "%s" printed
      else
        match tryLoadPlugin (fun typ ->
            function
            | :? TableDsl.PrinterPluginAttribute as attr when attr.Name.ToLower() = format -> Some (getPrinterFunction typ)
            | _ -> None) with
        | Some printer ->
            let output = options |> Map.tryFind "output"
            printer (output, options, elems)
        | None -> failwithf "printer(%s) is not found." format
  | None ->
      failwith "print subcommand needs format option."

let parseRuleLine (line: string) =
  match line.Split([|':'|], 3) with
  | [| name |] -> (name, None, None)
  | [| name; TableDsl.RuleLevelPatterns.RuleLevel level; arg |] -> (name, Some level, Some arg)
  | [| name; arg |] -> (name, None, Some arg)
  | [| name; arg; argRest |] ->
      let arg = arg + ":" + argRest
      (name, None, Some arg)

let check targetFile options =
  match options |> Map.tryFind "rule" with
  | Some (ruleFile: string) ->
      let input = File.ReadAllText(targetFile, Encoding.UTF8)
      let elems = TableDsl.Parser.parse input

      let tryLoadRuleChecker line =
        let name, level, arg = parseRuleLine line
        tryLoadPlugin (fun typ ->
          function
          | :? TableDsl.RuleCheckerPluginAttribute as attr when attr.Name.ToLower() = name ->
              level |> Option.iter (fun l -> attr.Level <- l)
              arg |> Option.iter (fun a -> attr.Arg <- a)
              Some (getRuleCheckerFunction attr typ)
          | _ -> None)

      let rules =
        File.ReadAllLines(ruleFile)
        |> Array.choose tryLoadRuleChecker
      let results =
        rules |> Seq.collect (fun rule -> rule elems)

      do
        results
        |> Seq.iter (fun res -> printfn "%s" (string res))

      let detected = Seq.length results
      let counts = results |> Seq.countBy (fun r -> r.Level)
      let fatals = defaultArg (counts |> Seq.tryPick (function (TableDsl.Fatal, x) -> Some x | _ -> None)) 0
      let warnings = defaultArg (counts |> Seq.tryPick (function (TableDsl.Warning, x) -> Some x | _ -> None)) 0
      let suggestions = defaultArg (counts |> Seq.tryPick (function (TableDsl.Suggestion, x) -> Some x | _ -> None)) 0
      printfn "==========================================================================="
      printfn "detected: %d, fatals: %d, warnings: %d, suggestions: %d" detected fatals warnings suggestions
      fatals 
  | None ->
      failwith "check subcommand needs rule option."

[<EntryPoint>]
let main argv =
  try
    let args = parse (List.ofArray argv)
    match args.SubCommandType with
    | Print -> print args.TargetFile args.Options; 0
    | Check -> check args.TargetFile args.Options
  with
    e ->
      eprintfn "%s" e.Message
      if e.InnerException <> null then
        eprintfn "%s" e.InnerException.Message
        eprintfn "%s" e.InnerException.StackTrace
      -1
open System
open System.IO
open System.Text
open System.Reflection

type SubCommandType =
  | Print

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
  | other::_ -> failwithf "unknown subcommand: %s" other
  | [] -> failwith "please "

type Plugin =
  | PrinterPlugin of (string option * Map<string, string> * TableDsl.Element list -> unit)

let getPrinterFunction (typ: Type) =
  let f (m: MethodInfo) (output: string option, options: Map<string, string>, elems: TableDsl.Element list) =
    m.Invoke(null, [| output; options; elems |]) |> ignore

  f (typ.GetMethod("print"))

let tryLoadPlugin pred =
  seq {
    for dll in Directory.GetFiles("plugins", "*.dll") do
      let asm = Assembly.LoadFrom(dll)
      for typ in asm.GetTypes() -> typ
  }
  |> Seq.tryPick (fun typ ->
       let attrs = typ.GetCustomAttributes(true)
       if attrs |> Array.exists pred then
         Some (PrinterPlugin (getPrinterFunction typ))
       else
         None
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
        match tryLoadPlugin (function :? TableDsl.PrinterAttribute as attr -> attr.Name.ToLower() = format | _ -> false) with
        | Some (PrinterPlugin printer) ->
            let output = options |> Map.tryFind "output"
            printer (output, options, elems)
        | None -> failwithf "printer(%s) is not found." format
  | None ->
      failwith "print subcommand needs format option."

[<EntryPoint>]
let main argv =
  try
    let args = parse (List.ofArray argv)
    match args.SubCommandType with
    | Print ->
        print args.TargetFile args.Options
    0
  with
    e ->
      eprintfn "%s" e.Message
      if e.InnerException <> null then
        eprintfn "%s" e.InnerException.Message
        eprintfn "%s" e.InnerException.StackTrace
      -1
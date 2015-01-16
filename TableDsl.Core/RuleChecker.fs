namespace TableDsl

type RuleLevel = Suggestion = 0 | Warning = 1 | Fatal = 2

module RuleLevelPatterns =
  let (|RuleLevel|_|) (str: string) =
    match str.ToLower() with
    | "suggestion" -> Some RuleLevel.Suggestion
    | "warning" -> Some RuleLevel.Warning
    | "fatal" -> Some RuleLevel.Fatal
    | _ -> None

type RuleCheckerPluginAttribute (name: string, level: RuleLevel) =
  inherit System.Attribute()

  member __.Name = name
  member val DefaultLevel = level with get, set
  member val Arg = "" with get, set

type CheckResult = {
  Level: RuleLevel
  Message: string
}
with
  override this.ToString () =
    sprintf "%A: %s" this.Level this.Message
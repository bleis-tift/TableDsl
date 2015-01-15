namespace TableDsl
 
open FParsec
open TableDsl.Parser
open TableDsl.Parser.Types
open TableDsl.Parser.Primitives
open TableDsl.Parser.Impl
 
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Parser =
  // HACK: 外のアセンブリからTableDsl.Parser名前空間のImplモジュールが見えないので、
  //       それを回避するためにParserモジュールにもImplモジュールを作り、
  //       こちら経由でアクセスするようにする
  module internal Impl =
    let pSummaryLine = pSummaryLine
    let pSummary = pSummary
    let pNonQuotedTypeParamElem = pNonQuotedTypeParamElem

  // HACK: 外のアセンブリからTableDsl.Parser名前空間のPrimitivesモジュールが見えないので、
  //       それを回避するためにParserモジュールにもPrimitivesモジュールを作り、
  //       こちら経由でアクセスするようにする
  module internal Primitives =
    let pSqlValue = pSqlValue

  type Position = {
    Line: int64
    Column: int64
  }

  type UserState =
    | TypeEnv of State
    | Internal of obj

  type Message =
    | UserFriendlyMessages of (Position * UserState * string) list
    | FParsecDefaultMessage of string

  type Error = {
    Position: Position
    UserState: UserState
    Message: Message
  }

  let collectMessages pos (state: UserState) (msg: ErrorMessageList) =
    let pos2pos (pos: FParsec.Position) = { Line = pos.Line; Column = pos.Column }
    let rec collectMessages' (pos: Position) (state: UserState) (msg: ErrorMessageList) =
      [
        match msg.Head with
        | Message msg -> yield (pos, state, msg)
        | NestedError (pos, state, rest)
        | CompoundError (_, pos, state, rest) ->
            let state =
              match state with
              | :? State as env -> TypeEnv env
              | other -> Internal other
            yield! collectMessages' (pos2pos pos) state rest
        | _ -> yield! []

        if msg.Tail <> null then
          yield! collectMessages' pos state msg.Tail
      ]
    if msg = null then
      []
    else
      collectMessages' pos state msg

  let tryParse' parser (input: string) =
    let err2err (err: ParserError) =
      let pos = { Line = err.Position.Line; Column = err.Position.Column }
      let state =
        match err.UserState with
        | :? State as env -> TypeEnv env
        | other -> Internal other
      match collectMessages pos state err.Messages with
      | [] -> FParsecDefaultMessage (sprintf "%A" err)
      | notEmpty -> UserFriendlyMessages notEmpty

    match CharParsers.runParserOnString parser State.initialState "" (input.TrimEnd()) with
    | Success (res, _, endPos) -> Basis.Core.Success (res, input.Substring(int endPos.Index))
    | Failure (_, err, _) -> Basis.Core.Failure (err2err err)

  let tryParse input = tryParse' Impl.parser input
 
  let parse input =
    match tryParse input with
    | Basis.Core.Success (res, _) -> res
    | Basis.Core.Failure err ->
        match err with
        | UserFriendlyMessages msgs -> eprintf "%A" msgs
        | FParsecDefaultMessage msg -> eprintf "%s" msg
        []

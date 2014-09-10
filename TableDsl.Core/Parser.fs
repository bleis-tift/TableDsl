namespace TableDsl
 
open FParsec
 
module Parser =
  module internal Impl =

    type State = (string * int * TypeDef) list

    let initialState: State =
      let builtin name paramCount =
        let typeVar i =
          TypeVariable ("@" + string (i + 1))
        [ for i in 0..paramCount ->
            (name, i, BuiltinType { TypeName = name; TypeParameters = List.init i typeVar }) ]
      [ builtin "bigint" 0
        builtin "int" 0
        builtin "smallint" 0
        builtin "tinyint" 0
        builtin "bit" 0
        builtin "decimal" 2
        builtin "numeric" 2
        builtin "money" 0
        builtin "smallmoney" 0
        builtin "float" 1
        builtin "real" 0
        builtin "date" 0
        builtin "datetime2" 1
        builtin "datetime" 0
        builtin "datetimeoffset" 1
        builtin "smalldatetime" 0
        builtin "time" 1
        builtin "char" 1
        builtin "varchar" 1
        builtin "text" 0
        builtin "ntext" 0
        builtin "image" 0
        builtin "nchar" 0
        builtin "nvarchar" 1
        builtin "binary" 1
        builtin "varbinary" 1
        builtin "hierarchyid" 0
        builtin "sql_variant" 0
        builtin "rowversion" 0
        builtin "timestamp" 0
        builtin "uniqueidentifier" 0
        builtin "xml" 1
        builtin "geography" 0
        builtin "geometry" 0
        [("nullable", 1, BuiltinType { TypeName = "nullable"; TypeParameters = [TypeVariable "@type"] })]
      ] |> List.concat

    type Parser<'T> = Parser<'T, State>
 
    let ws = many (choice [pchar ' '; pchar '\t'; pchar '\n'])
 
    let pSkipToken str =
      ws >>. pstring str >>. ws |>> ignore

    let pName: Parser<_> = regex "[a-zA-Z_][a-zA-Z0-9_]*"

    let pSummaryLine = parse {
      do! pSkipToken "///"
      let! line = manyCharsTill anyChar newline
      return line
    }
 
    let pSummary = (attempt pSummaryLine) |> many1 |>> fun lines -> System.String.Join("\n", lines)

    let pColTypeName = pName
    let pTableName = pName
    let pColName = pName
 
    let pJpName = parse {
      do! pSkipToken "["
      let! jpName = many1Chars (noneOf "\n]")
      do! pSkipToken "]"
      return jpName
    }

    let trimSpace p =
      let ws = many (pchar ' ' <|> pchar '\t')
      ws >>. p .>> ws

    let pQuotedTypeParamElem = parse {
      do! pSkipToken "`"
      let! elem = many1Chars (noneOf "`")
      do! pSkipToken "`"
      return elem
    }

    let pNonQuotedTypeParamElem =
      let rec pNonQuotedTypeParamElem terminators : Parser<string> =
        let p = parse {
          let! ch = anyChar
          return!
            match ch with
            | '(' -> parse {
                       let! result = pNonQuotedTypeParamElem [')']
                       do! pchar ')' |>> ignore
                       return "(" + result + ")" }
            | x when List.exists ((=)x) terminators -> pzero
            | x -> preturn (string x)
        }
        many (attempt p) |>> (fun xs -> System.String.Concat(xs))
      pNonQuotedTypeParamElem [','; ')']

    let pTypeParamElem = (pQuotedTypeParamElem <|> pNonQuotedTypeParamElem) >>= (function "" -> pzero | other -> preturn other)

    let pTypeParam =
      sepBy pTypeParamElem (pchar ',') |> between (pchar '(') (pchar ')')

    let resolveType typeName typeParams env =
      match env |> List.tryFind (fun (name, paramCount, _) -> name = typeName && paramCount = (List.length typeParams)) with
      | Some (name, paramCount, t) ->
          let typ = { ColumnSummary = None; ColumnTypeDef = t; ColumnJpName = None; ColumnAttributes = [] }
          typ, preturn ()
      | None -> Unchecked.defaultof<_>, failFatally (sprintf "%sという型が見つかりませんでした。" typeName)

    let pClosedTypeRefWithoutAttributes = parse {
      let! typeName = pName
      let! typeParams = opt (attempt pTypeParam)
      let typeParams =
        match typeParams with
        | None -> []
        | Some x -> x
      let! env = getUserState
      let typ, perror = resolveType typeName typeParams env
      do! perror
      return ({ Type = typ; TypeParameters = typeParams }, [])
    }

    let pComplexAttribute = parse {
      do! ws |>> ignore
      let! name = pName
      do! pSkipToken "="
      let! value = regex "[a-zA-Z0-9_.]+"
      return ComplexAttr (name, { Value = value })
    }
    let pSimpleAttribute = ws >>. pName |>> (fun name -> SimpleAttr name)
    let pAttribute = attempt pComplexAttribute <|> pSimpleAttribute
    let pAttributes = sepBy1 pAttribute (pchar ';')

    let pClosedTypeRefWithAttributes = parse {
      do! pSkipToken "{"
      let! typ, _ = pClosedTypeRefWithoutAttributes
      do! pSkipToken "with"
      let! attrs = pAttributes
      do! pSkipToken "}"
      return (typ, attrs)
    }

    let pClosedTypeRef = pClosedTypeRefWithAttributes <|> pClosedTypeRefWithoutAttributes
 
    let pAliasDef name typeParams = parse {
      let! body, attrs = pClosedTypeRef
      return (AliasDef ({ TypeName = name; TypeParameters = typeParams }, body.Type), attrs)
    }

    let pEnumTypeDef name = parse {
      return! pzero
    }

    let pTypeDef name typeParams = pEnumTypeDef name <|> pAliasDef name typeParams

    let pTypeParams =
      sepBy (regex "@[a-zA-Z0-9_]+") (pSkipToken ",")
      |> between (pSkipToken "(") (pSkipToken ")")

    let checkColTypeParams name xs =
      let rec tryFindDup existsParams = function
      | x::_ when List.exists ((=)x) existsParams -> Some x
      | x::xs -> tryFindDup (x::existsParams) xs
      | [] -> None

      Basis.Core.OptionDefaultOps.option {
        let! xs = xs
        let! dup = tryFindDup [] xs
        return failFatally (sprintf "型%sの定義で型変数%sが重複しています。" name dup)
      }

    let pColTypeDef = parse {
      let! colTypeSummary = pSummary |> attempt |> opt
      do! pSkipToken "coltype"
      let! colTypeName = pColTypeName
      let! colTypeParams = pTypeParams |> attempt |> opt
      do! match checkColTypeParams colTypeName colTypeParams with Some p -> p | None -> preturn ()
      let colTypeParams =
        colTypeParams
        |> function Some x -> x | _ -> []
        |> List.map (fun p -> TypeVariable p)
      let! colTypeJpName = pJpName |> attempt |> opt
      do! pSkipToken "="
      let! typ, attrs = pTypeDef colTypeName colTypeParams
      return ColTypeDef { ColumnSummary = colTypeSummary; ColumnTypeDef = typ; ColumnJpName = colTypeJpName; ColumnAttributes = attrs }
    }

    let pColumnDef = parse {
      let! colSummary = pSummary |> attempt |> opt
      do! many (choice [pchar ' '; pchar '\t']) |>> ignore
      let! colName = pColName
      let! colJpName =
        if colName = "_" then preturn None else pJpName |> attempt |> opt
      do! pSkipToken ":"
      let! colType, attrs = pClosedTypeRef
      return { ColumnSummary = colSummary
               ColumnName = if colName = "_" then Wildcard else ColumnName (colName, colJpName)
               ColumnType = (colType, attrs) }
    }
 
    let pTableDef = parse {
      let! tableSummary = pSummary |> attempt |> opt
      do! pSkipToken "table"
      let! tableName = pTableName
      let! tableJpName = pJpName |> attempt |> opt
      do! pSkipToken "=" >>. pSkipToken "{"
      let! colDefs = sepEndBy (attempt pColumnDef) (newline |>> ignore)
      do! pSkipToken "}"
      return TableDef { TableSummary = tableSummary; TableName = tableName; TableJpName = tableJpName; ColumnDefs = colDefs }
    }
 
    let parser = many (attempt pColTypeDef <|> pTableDef) .>> eof

  type Position = {
    Line: int64
    Column: int64
  }

  type UserState =
    | TypeEnv of Impl.State
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
              | :? Impl.State as env -> TypeEnv env
              | other -> Internal other
            yield! collectMessages' (pos2pos pos) state rest
        | _ -> yield! []

        if msg.Tail <> null then
          yield! collectMessages' pos state msg.Tail
      ]
    collectMessages' pos state msg

  let tryParse' parser input =
    let err2err (err: ParserError) =
      let pos = { Line = err.Position.Line; Column = err.Position.Column }
      let state =
        match err.UserState with
        | :? Impl.State as env -> TypeEnv env
        | other -> Internal other
      match collectMessages pos state err.Messages with
      | [] -> FParsecDefaultMessage (sprintf "%A" err)
      | notEmpty -> UserFriendlyMessages notEmpty

    match CharParsers.runParserOnString parser Impl.initialState "" input with
    | Success (res, _, _) -> Basis.Core.Success res
    | Failure (_, err, _) -> Basis.Core.Failure (err2err err)

  let tryParse input = tryParse' Impl.parser input
 
  let parse input =
    match tryParse input with
    | Basis.Core.Success res -> res
    | Basis.Core.Failure err ->
        match err with
        | UserFriendlyMessages msgs -> eprintf "%A" msgs
        | FParsecDefaultMessage msg -> eprintf "%s" msg
        []

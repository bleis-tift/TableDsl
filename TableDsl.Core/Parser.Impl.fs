namespace TableDsl.Parser

open Basis.Core
open FParsec
open TableDsl
open TableDsl.Parser
open TableDsl.Parser.Types
open TableDsl.Parser.Primitives

module internal Impl =
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

  let pOpenTypeRefWithoutAttributes, pOpenTypeRefWithoutAttributesRef = createParserForwardedToRef ()

  let pNullableParam = parse {
    do! pSkipOnelineToken "("
    let! typ = pOpenTypeRefWithoutAttributes
    do! pSkipOnelineToken ")"
    return [BoundType typ]
  }

  let pOpenTypeParamElem =
    pTypeVariableName |>> TypeVariable <|> (pSqlValue |>> BoundValue) // ここはpSqlValueじゃなくて、pSqlTypeか？

  let pOpenTypeParam typeName =
    if typeName = "nullable" then
      pNullableParam
    else
      sepBy pOpenTypeParamElem (pSkipToken ",") |> between (pchar '(') (pchar ')')

  let expandAttr' replacingMap = function
  | Var v ->
      match Map.tryFind v replacingMap with
      | Some (BoundValue v) -> Lit v
      | _ -> Var v
  | Lit l -> Lit l

  let expandAttr replacingMap = function
  | ComplexAttr (key, values) -> ComplexAttr (key, values |> List.map (expandAttr' replacingMap))
  | other -> other

  let replaceTypeParams'' replacingMap = List.map (function TypeVariable key -> replacingMap |> Map.find key | other -> other)

  let rec replaceTypeParams' replacingMap = function
  | BuiltinType ty -> BuiltinType { ty with TypeParameters = ty.TypeParameters |> replaceTypeParams'' replacingMap }
  | AliasDef (ty, originalType) ->
      let replaced = ty.TypeParameters |> replaceTypeParams'' replacingMap
      AliasDef ({ ty with TypeParameters = replaced }, replaceTypeParams replacingMap originalType)
  | EnumTypeDef ty -> EnumTypeDef ty

  and replaceTypeParams replacingMap colTypeDef =
    { colTypeDef with ColumnTypeDef = replaceTypeParams' replacingMap colTypeDef.ColumnTypeDef
                      ColumnAttributes = colTypeDef.ColumnAttributes |> List.map (expandAttr replacingMap) }

  let resolveType typeName typeParams env =
    let resolved =
      env |> List.tryFind (fun (name, paramCount, _, _) -> name = typeName && paramCount = (List.length typeParams)) 
    match resolved with
    | Some (name, paramCount, t, attrs) ->
        let typeDef, attrs =
          match t with
          | BuiltinType ty -> (BuiltinType { ty with TypeParameters = typeParams }), attrs
          | AliasDef (ty, originalType) ->
              let replacingMap =
                Map.ofList (
                  (ty.TypeParameters, typeParams)
                  ||> List.zip
                  |> List.choose (function (TypeVariable key, value) -> Some (key, value) | _ -> None))
              let attrs = attrs |> List.map (expandAttr replacingMap)
              (AliasDef ({ ty with TypeParameters = typeParams }, replaceTypeParams replacingMap originalType)), attrs
          | EnumTypeDef ty -> (EnumTypeDef ty), attrs
        preturn { ColumnSummary = None; ColumnTypeDef = typeDef; ColumnJpName = None; ColumnAttributes = attrs }
    | None -> failFatally (sprintf "%sという型が見つかりませんでした。" typeName)

  let pClosedTypeRefWithoutAttributes = parse {
    let! typeName = pName
    let! typeParams = opt (attempt (pOpenTypeParam typeName))
    let typeParams =
      match typeParams with
      | None -> []
      | Some x -> x
    let! env = getUserState
    let! typ = resolveType typeName typeParams env
    return typ
  }

  pOpenTypeRefWithoutAttributesRef := pClosedTypeRefWithoutAttributes

  let pClosedComplexAttribute = parse {
    do! wsnl |>> ignore
    let! name = pName
    do! pSkipToken "="
    let! value = pSqlValue
    return ComplexAttr (name, [ Lit value ])
  }
  let pVarAttrValueElem = pTypeVariableName |>> Var
  let pLitAttrValueElem = pSqlValue |>> Lit
  let pAttributeValueElem = pVarAttrValueElem <|> pLitAttrValueElem
  let pOpenComplexAttribute = parse {
    do! wsnl |>> ignore
    let! name = pName
    do! pSkipToken "="
    let! values = many1 pAttributeValueElem
    return ComplexAttr (name, values)
  }
  let pSimpleAttribute = wsnl >>. pName |>> (fun name -> SimpleAttr name)
  let pClosedAttribute = attempt pClosedComplexAttribute <|> pSimpleAttribute
  let pOpenAttribute = attempt pOpenComplexAttribute <|> pSimpleAttribute
  let pClosedAttributes = sepBy1 pClosedAttribute (pchar ';') // 改行もセパレータ扱いにする？
  let pOpenAttributes = sepBy1 pOpenAttribute (pchar ';') // 改行もセパレータ扱いにする？

  let pClosedTypeRefWithAttributes = parse {
    do! pSkipOnelineToken "{"
    let! typ = pClosedTypeRefWithoutAttributes
    do! pSkipToken "with"
    let! attrs = pClosedAttributes
    do! pSkipOnelineToken "}"
    return (typ, attrs)
  }

  let pOpenTypeRefWithAttributes = parse {
    do! pSkipOnelineToken "{"
    let! typ = pOpenTypeRefWithoutAttributes
    do! pSkipToken "with"
    let! attrs = pOpenAttributes
    do! pSkipOnelineToken "}"
    return (typ, attrs)
  }

  let pClosedTypeRef = pClosedTypeRefWithAttributes <|> (pClosedTypeRefWithoutAttributes |>> fun t -> (t, []))
  let pOpenTypeRef = pOpenTypeRefWithAttributes <|> (pOpenTypeRefWithoutAttributes |>> fun t -> (t, []))

  let pAliasDef name typeParams = parse {
    let! body, attrs = pOpenTypeRef
    return (AliasDef ({ TypeName = name; TypeParameters = typeParams }, body), attrs)
  }

  let pEnumCase = attempt (pSkipOnelineToken "|") >>. pName .>> pSkipOnelineToken "=" .>>. pInteger
  let pEnumCases = sepEndBy1 pEnumCase newline
  let pEnumTypeDef name typeParams = parse {
    let! cases = pEnumCases
    do! pSkipToken "based"
    let! based, attrs = pOpenTypeRef
    match based.ColumnTypeDef with
    | BuiltinType typ -> return (EnumTypeDef { EnumTypeName = name; BaseType = typ; Cases = cases }), attrs
    | AliasDef (typ, orig) -> return (EnumTypeDef { EnumTypeName = name; BaseType = typ; Cases = cases }), attrs
    | EnumTypeDef typ -> return! failFatally (sprintf "列挙型の定義(%s)の基底型に他の列挙型(%s)を指定することはできません。" name typ.EnumTypeName)
  }

  let pTypeDef name typeParams = pEnumTypeDef name typeParams <|> pAliasDef name typeParams

  let pTypeParams =
    sepBy pTypeVariableName (pSkipToken ",")
    |> between (pSkipToken "(") (pSkipToken ")")

  let checkColTypeParams name xs =
    let rec tryFindDup existsParams = function
    | x::_ when List.exists ((=)x) existsParams -> Some x
    | x::xs -> tryFindDup (x::existsParams) xs
    | [] -> None

    option {
      let! xs = xs
      let! dup = tryFindDup [] xs
      return failFatally (sprintf "型%sの定義で型変数%sが重複しています。" name dup)
    }

  let pColTypeDef = parse {
    let! colTypeSummary = pSummaryOpt
    do! pSkipToken "coltype"
    let! colTypeName = pColTypeName
    let! colTypeParams = pTypeParams |> attempt |> opt
    do! match checkColTypeParams colTypeName colTypeParams with Some p -> p | None -> preturn ()
    let colTypeParams =
      colTypeParams
      |> function Some x -> x | _ -> []
      |> List.map (fun p -> TypeVariable p)
    let! colTypeJpName = pJpNameOpt
    do! pSkipToken "="
    let! typ, attrs = pTypeDef colTypeName colTypeParams
    do! updateUserState (fun state -> (colTypeName, colTypeParams.Length, typ, attrs)::state)
    return ColTypeDef { ColumnSummary = colTypeSummary; ColumnTypeDef = typ; ColumnJpName = colTypeJpName; ColumnAttributes = attrs }
  }

  let pColumnDef = parse {
    let! colSummary = pSummaryOpt
    do! wsnl |>> ignore
    let! colName = pColName
    let! colJpName =
      if colName = "_" then preturn None else pJpNameOpt
    do! pSkipToken ":"
    let! colType, attrs = pClosedTypeRef
    return { ColumnSummary = colSummary
             ColumnName = if colName = "_" then Wildcard else ColumnName (colName, colJpName)
             ColumnType = (colType, attrs) }
  }

  let pTableDef = parse {
    let! tableSummary = pSummaryOpt
    do! pSkipToken "table"
    let! tableName = pTableName
    let! tableJpName = pJpNameOpt
    do! pSkipToken "="
    let! colDefs =
      sepEndBy (attempt pColumnDef) newline
      |> between (pSkipOnelineToken "{") (pSkipOnelineToken "}")
    return TableDef { TableSummary = tableSummary; TableName = tableName; TableJpName = tableJpName; ColumnDefs = colDefs }
  }

  // TODO : 一通り完成したら、エラーメッセージを入れる？(例: 「トップレベルの要素はcoltypeかtableのみが許されています。」)
  let parser = (sepBy ((attempt pColTypeDef) <|> (attempt pTableDef)) newline) .>> eof

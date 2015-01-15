namespace TableDsl.Parser

open FParsec
open TableDsl.Parser.Types

module internal Primitives =
  let wsnl: Parser<_> = many (choice [pchar ' '; pchar '\t'; pchar '\n'])
  let ws: Parser<_> = many (pchar ' ' <|> pchar '\t')

  let pSkipToken str = wsnl >>. pstring str >>. wsnl |>> ignore
  let pSkipOnelineToken str = ws >>. pstring str >>. ws |>> ignore

  let pInteger: Parser<_> = regex "[1-9][0-9]*" |>> int

  let pName: Parser<_> = regex "[a-zA-Z_][a-zA-Z0-9_]*"

  let pSummaryLine: Parser<_> = attempt (pSkipOnelineToken "///") >>. manyChars (noneOf "\n")
  let pSummary = wsnl >>. sepEndBy1 pSummaryLine newline |>> fun lines -> System.String.Join("\n", lines)
  let pSummaryOpt = opt (attempt pSummary)

  let pColTypeName = pName
  let pTableName = pName
  let pColName = pName

  let pJpName = many1Chars (noneOf "\n]") |> between (pSkipToken "[") (pSkipToken "]")
  let pJpNameOpt = opt (attempt pJpName)

  let pTypeVariableName: Parser<_> = regex "@[a-zA-Z0-9_]+"

  module private SqlParsers =
    let pSqlExpr, pSqlExprRef = createParserForwardedToRef ()
    let pSqlNum: Parser<_> =
      regex @"-?[0-9]+(\.[0-9]*)?"
      <??> "sql number"
    let pSqlString: Parser<_> =
      parse {
        let! startSqlStr = regex "N?'"
        let! sqlStr =
          ((parse { let! ch = noneOf "'" in return string ch })
            <|> 
            pstring "''") |> many |>> String.concat ""
        let! endSqlStr = regex "'"
        return startSqlStr + sqlStr + endSqlStr
      }
      <??> "sql string"
    let pFunctionCall: Parser<_> =
      regex @"[a-zA-Z_][a-zA-Z0-9_]*"
      .>>. between (pstring "(") (pstring ")") (sepBy pSqlExpr (pchar ',' .>> opt (pchar ' ' |> many)))
      |>> fun (name, args) -> name + "(" + String.concat ", " args + ")"
      <??> "sql function call"
    let pSqlIdent: Parser<_> =
      regex @"[a-zA-Z_][a-zA-Z0-9_]*(\.[a-zA-Z_][a-zA-Z0-9_]*)*"
      <??> "sql ident"

    do pSqlExprRef := choice [ pSqlNum; attempt pSqlString; attempt pFunctionCall; pSqlIdent ]

  // TODO : SQLの値に対応する
  // 対応しなければならないもの
  // * 演算 ex) 1 + 2
  // * キャスト ex) cast(value as type)
  // * 値 ex) 0x42
  // * 優先順位のカッコ ex) (1 + 2) * 3
  // * CASE式(優先度低)
  // * サブクエリ(優先度低)
  let pSqlValue: Parser<_> = SqlParsers.pSqlExpr

  let pIndexSetting: Parser<_> = sepBy1 (regex @"[a-zA-Z0-9_]+") (pchar '.') |>> String.concat "."

  let pPlaceholders: Parser<_> = regex @"[a-zA-Z0-9_.]+(\([a-zA-Z0-9_., ]*\))?"

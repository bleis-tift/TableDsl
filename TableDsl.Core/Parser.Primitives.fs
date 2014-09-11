namespace TableDsl.Parser

open FParsec
open TableDsl
open TableDsl.Parser.Types

module internal Primitives =
  let wsnl: Parser<_> = many (choice [pchar ' '; pchar '\t'; pchar '\n'])
  let ws: Parser<_> = many (pchar ' ' <|> pchar '\t')

  let pSkipToken str = wsnl >>. pstring str >>. wsnl |>> ignore
  let pSkipOnlineToken str = ws >>. pstring str >>. ws |>> ignore

  let pName: Parser<_> = regex "[a-zA-Z_][a-zA-Z0-9_]*"

  let pSummaryLine: Parser<_> = attempt (pSkipOnlineToken "///") >>. manyChars (noneOf "\n")
  let pSummary = wsnl >>. sepEndBy1 pSummaryLine newline |>> fun lines -> System.String.Join("\n", lines)
  let pSummaryOpt = opt (attempt pSummary)

  let pColTypeName = pName
  let pTableName = pName
  let pColName = pName

  let pJpName = many1Chars (noneOf "\n]") |> between (pSkipToken "[") (pSkipToken "]")
  let pJpNameOpt = opt (attempt pJpName)

  let pTypeVariableName: Parser<_> = regex "@[a-zA-Z0-9_]+"

  // TODO : '文字列'や4.2に対応すること
  // 対応しなければならないもの
  // * 関数呼び出し ex) NEWID(), f(x, y)
  // * 演算 ex) 1 + 2
  // * キャスト ex) cast(value as type)
  // * 値 ex) 42.1, N'hoge', 'hogehoge', 0x, 0x42
  // * 優先順位のカッコ ex) (1 + 2) * 3
  // * CASE式(優先度低)
  // * サブクエリ(優先度低)
  let pSqlValue: Parser<_> = regex @"[a-zA-Z0-9_.]+(\(\))?"
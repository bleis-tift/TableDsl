namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module SqlExprParserTest =
  open Basis.Core
  open TableDsl
  open TableDsl.Parser

  let tryParse p input =
    input
    |> tryParse' p
    |> function Success (res, _) -> res | Failure f -> failwithf "failure: %A" f

  [<TestCase("42")>]
  [<TestCase("42.0")>]
  [<TestCase("''")>]
  [<TestCase("'hoge'")>]
  [<TestCase("'ho''ge'")>]
  [<TestCase("N''")>]
  [<TestCase("N'hoge'")>]
  [<TestCase("N'ho''ge'")>]
  [<TestCase("ColName")>]
  [<TestCase("Function()")>]
  [<TestCase("Function(42, 10)")>]
  let test_pSqlValue str =
    str
    |> tryParse Primitives.pSqlValue
    |> should equal str
namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module ParserImplTest =
  open Basis.Core
  open TableDsl
  open TableDsl.Parser
  open TableDsl.Parser.Impl

  let tryParse p input =
    input
    |> tryParse' p
    |> function Success res -> res | Failure f -> failwithf "failure: %A" f

  [<TestCase("()")>]
  [<TestCase("((()()))")>]
  [<TestCase("((xxxx()yyyy()))")>]
  [<TestCase("hoge(piyo(foo(bar)))")>]
  [<TestCase("hoge((piyo + foo) * bar)")>]
  [<TestCase("hoge(piyo, foo, bar)")>]
  let test_pNonQuotedTypeParamElem str =
    str
    |> tryParse pNonQuotedTypeParamElem
    |> should equal (str, "")

  [<TestCase("///hoge", "hoge", "")>]
  [<TestCase("///  hoge", "hoge", "")>]
  [<TestCase("   ///  hoge", "hoge", "")>]
  [<TestCase("///hoge\n", "hoge", "\n")>]
  [<TestCase("///  hoge\n", "hoge", "\n")>]
  [<TestCase("   ///  hoge\n", "hoge", "\n")>]
  let test_pSummaryLine input (expected: string) (expectedRest: string) =
    input
    |> tryParse pSummaryLine
    |> should equal (expected, expectedRest)

  [<TestCase("///hoge", "hoge", "")>]
  [<TestCase("///  hoge", "hoge", "")>]
  [<TestCase("   ///  hoge", "hoge", "")>]
  [<TestCase("\n///hoge", "hoge", "")>]
  [<TestCase("\n///  hoge", "hoge", "")>]
  [<TestCase("\n   ///  hoge", "hoge", "")>]
  [<TestCase("///hoge\na", "hoge", "a")>]
  [<TestCase("///  hoge\na", "hoge", "a")>]
  [<TestCase("   ///  hoge\n table", "hoge", " table")>]
  [<TestCase("///hoge\n///piyo\n table", "hoge\npiyo", " table")>]
  [<TestCase("///  hoge\n///piyo\n table", "hoge\npiyo", " table")>]
  [<TestCase("   ///  hoge\n///piyo\n table", "hoge\npiyo", " table")>]
  let test_pSummary input (expected: string) (expectedRest: string) =
    input
    |> tryParse pSummary
    |> should equal (expected, expectedRest)
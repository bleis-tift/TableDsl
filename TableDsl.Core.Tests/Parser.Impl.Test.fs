namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module ParserImplTest =
  open Basis.Core
  open TableDsl
  open TableDsl.Parser
  open TableDsl.Parser.Impl

  [<TestCase("()")>]
  [<TestCase("((()()))")>]
  [<TestCase("((xxxx()yyyy()))")>]
  [<TestCase("hoge(piyo(foo(bar)))")>]
  [<TestCase("hoge((piyo + foo) * bar)")>]
  [<TestCase("hoge(piyo, foo, bar)")>]
  let test_pNonQuotedTypeParamElem str =
    str |> tryParse' pNonQuotedTypeParamElem
    |> function Success s -> s | Failure f -> failwithf "failure: %A" f
    |> should equal str
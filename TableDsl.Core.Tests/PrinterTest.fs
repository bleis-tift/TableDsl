namespace TableDsl.Core.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module PrinterTest =
  open TableDsl

  [<Test>]
  let ``print empty list`` () =
    []
    |> Printer.print
    |> should equal ""

  [<TestCase("coltype Created = datetime2")>]
  let tests str =
    str
    |> Parser.parse
    |> Printer.print
    |> should equal str
namespace TableDsl.Sql.Tests

open FsUnit
open NUnit.Framework

[<TestFixture>]
module PrinterTest =
  open TableDsl.Sql
  open Basis.Core

  [<Test>]
  let ``print empty list`` () =
    []
    |> Printer.print
    |> should equal ""

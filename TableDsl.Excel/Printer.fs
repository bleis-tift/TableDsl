namespace TableDsl.Excel

open TableDsl
open TableDsl.PrinterPluginUtil

open System.IO
open OfficeOpenXml

open Basis.Core

[<PrinterPlugin("excel")>]
module Printer =

  /// マージを解除する
  let unmerge cell count (sheet: ExcelWorksheet) =
    let range = ExcelAddress(cell)
    for i in 0..(count - 1) do
      let target = ExcelAddress(range.Start.Row + (i * 2), range.Start.Column, range.End.Row + (i * 2), range.End.Column)
      sheet.Cells.[target.Address].Merge <- false

  /// マージをいったん解除して、もう一回マージする(マージされたセルを含むより大きな範囲をマージするためには、この手順が必要)
  let remerge (col, row) totalLines (sheet: ExcelWorksheet) =
    let cell = sheet.MergedCells |> Seq.find (fun c -> c.StartsWith(col + string row + ":"))
    let endCol = System.String.Concat(cell |> Seq.skipWhile ((<>)':') |> Seq.skip 1 |> Seq.takeWhile (System.Char.IsLetter))
    sheet |> unmerge cell (totalLines / 2)
    let mergingCell = col + string row + ":" + endCol + string (row + totalLines - 1)
    sheet.Cells.[mergingCell].Merge <- true
    mergingCell

  /// 罫線を引く
  let drawBox cell (sheet: ExcelWorksheet) =
    sheet.Cells.[cell].Style.Border.Top.Style <- Style.ExcelBorderStyle.Thin
    sheet.Cells.[cell].Style.Border.Left.Style <- Style.ExcelBorderStyle.Thin
    sheet.Cells.[cell].Style.Border.Bottom.Style <- Style.ExcelBorderStyle.Thin
    sheet.Cells.[cell].Style.Border.Right.Style <- Style.ExcelBorderStyle.Thin

  let setString totalLines (col, row) (str: string) (sheet: ExcelWorksheet) =
    // 一旦マージを解除し、再度マージし、枠線を引き、値を設定する
    let mergedCell = sheet |> remerge (col, row) totalLines
    sheet |> drawBox mergedCell
    sheet.Cells.[col + string row].Value <- str

  let writeColumns (sheet: ExcelWorksheet) (i: int) (colDef: ColumnDef) =
    let no = string (i + 1)
    let row = i + 8

    let colTypeDef, attrs = colDef.ColumnType
    match ColumnTypeDef.tryToTypeName attrs colTypeDef with
    | Success typeName ->
        sheet |> setString 1 ("A", row) no
        sheet |> setString 1 ("C", row) (match colDef.ColumnName with Wildcard -> (* todo *) "not implemented" | ColumnName (name, _) -> name)
        sheet |> setString 1 ("K", row) (match colDef.ColumnName with Wildcard -> (* todo *) "not implemented" | ColumnName (_, Some name) -> name | _ -> "-")
        sheet |> setString 1 ("S", row) typeName.TypeName
        sheet |> setString 1 ("X", row) (if typeName.Nullable then "■" else "□")
        sheet |> setString 1 ("AA", row) (match typeName.Attributes |> List.tryFind (fst >> (=)"default") with Some (_, v) -> v | None -> "-")
        sheet |> setString 1 ("AF", row) (match colDef.ColumnSummary with Some summary -> summary | None -> "-")
    | Failure f ->
        failwith (ConvertError.toStr f)

  let write (package: ExcelPackage) (table: TableDef) =
    let sheetName = match table.TableJpName with Some name -> name | None -> table.TableName
    let sheet = package.Workbook.Worksheets.Copy("0", sheetName)
    package.Workbook.Worksheets.MoveBefore(sheetName, "0")

    sheet |> setString 1 ("I", 1) table.TableName
    sheet |> setString 1 ("I", 2) (match table.TableJpName with Some name -> name | None -> "-")
    sheet |> setString 2 ("I", 3) (match table.TableSummary with Some summary -> summary | None -> "-")

    table.ColumnDefs
    |> List.iteri (writeColumns sheet)

  let print (output: string option, options: Map<string, string>, elems: Element list) =
    match output, options |> Map.tryFind "template" with
    | Some output, Some template ->
        let output = FileInfo(output)
        let template = FileInfo(template)
        use package = new ExcelPackage(output, template)

        //let sheet = package.Workbook.Worksheets.["0"]
        elems
        |> List.choose (function TableDef table -> Some table | _ -> None)
        |> List.iter (write package)

        package.Workbook.Worksheets.Delete("0")

        package.Save()
    | None, None ->
        eprintfn "output and template are required options."
    | _, None ->
        eprintfn "template is required option."
    | None, _ ->
        eprintfn "output is required option."
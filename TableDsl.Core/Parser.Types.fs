namespace TableDsl.Parser

open FParsec
open TableDsl

type ColumnSummary = string option
type ColumnJpName = string option

module Types =
  type State = (string * int * TypeDef * ColumnAttribute list * ColumnSummary * ColumnJpName) list
  type internal Parser<'T> = Parser<'T, State>

open Types

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal State =
  let initialState: State =
    let builtin name paramCount =
      let typeVar i =
        TypeVariable ("@" + string (i + 1))
      [ for i in 0..paramCount ->
          (name, i, BuiltinType { TypeName = name; TypeParameters = List.init i typeVar }, [], None, None) ]
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
      builtin "nchar" 1
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
    ] |> List.concat

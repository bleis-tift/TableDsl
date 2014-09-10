namespace TableDsl
 
/// 属性値は値を持つ。
type AttributeValue = {
  Value: string
  // 追加のフィールドはある？
}
 
/// 列の属性には、名前のみを持つSimpleAttrと、名前と値を持つComplexAttrがある。
/// SimpleAttrは、フラグを意味しており、存在する場合にそのフラグがtrueであることを示す。
type ColumnAttribute =
  | SimpleAttr of string
  | ComplexAttr of string * AttributeValue
 
/// 開いた型パラメータは、型変数か、文字列のいずれか。
type OpenTypeParam =
  | TypeVariable of string
  | StringValue of string
 
// 列の型は大きく分類すると、列挙型と非列挙型に分類できる。
// 組み込みの型は非列挙型のみとなっている。

/// 非列挙型は名前と型パラメータを持つ。
type NonEnumType = {
  ColumnTypeName: string
  TypeParameters: OpenTypeParam list
}
/// 列挙型は基本型とケースを持つ。
and EnumType = {
  BaseType: NonEnumType
  Cases: (string * int) list
}
/// 型定義は組み込み型か、別名定義か、列挙型定義のいずれか。
/// 別名定義の場合はオリジナルの列型定義を持つ。
and TypeDef =
  | BuiltinType of NonEnumType
  | AliasDef of NonEnumType * originalType: ColumnTypeDef
  | EnumTypeDef of EnumType
/// 列型定義は、列の型定義と列の属性を持つ。
/// ただし、トップレベルの列の定義はBuiltinTypeにはならない(組み込み型は定義できない)。
and ColumnTypeDef = {
  ColumnTypeDef: TypeDef
  ColumnAttributes: ColumnAttribute list
}
 
/// サマリは単なる文字列型。
type Summary = string
 
/// 閉じた型パラメータは単なる文字列として扱う。
type ClosedTypeParam = string

/// 閉じた型参照は型名と型パラメータを持つ。
type ClosedTypeRef = {
  Type: ColumnTypeDef
  TypeParameters: ClosedTypeParam list
}
 
/// 列名はワイルドカードか、名前を持ったもののいずれか。
type ColumnName =
  | Wildcard
  | ColumnName of name: string * jpName: string option
 
/// 列定義
type ColumnDef = {
  ColumnSummary: Summary option
  ColumnName: ColumnName
  ColumnType: ClosedTypeRef * ColumnAttribute list
}
 
/// テーブル定義
type TableDef = {
  TableSummary: Summary option
  TableName: string
  TableJpName: string option
  ColumnDefs: ColumnDef list
}

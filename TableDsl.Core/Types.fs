namespace TableDsl
 
type AttributeValueElem =
  | Lit of string
  | Var of string
with
  override this.ToString() = sprintf "%A" this

/// 属性値は属性値の要素のリスト
type AttributeValue = AttributeValueElem list
 
/// サマリは単なる文字列型。
type Summary = string
 
/// 列の属性には、名前のみを持つSimpleAttrと、名前と値を持つComplexAttrがある。
/// SimpleAttrは、フラグを意味しており、存在する場合にそのフラグがtrueであることを示す。
type ColumnAttribute =
  | SimpleAttr of string
  | ComplexAttr of string * AttributeValue
with
  override this.ToString() = sprintf "%A" this
 
/// 開いた型パラメータは、型変数か、文字列のいずれか。
type OpenTypeParam =
  | TypeVariable of string
  | BoundValue of string
  | BoundType of ColumnTypeDef
with
  override this.ToString() = sprintf "%A" this
 
// 列の型は大きく分類すると、列挙型と非列挙型に分類できる。
// 組み込みの型は非列挙型のみとなっている。

/// 非列挙型は名前と型パラメータを持つ。
and NonEnumType = {
  TypeName: string
  TypeParameters: OpenTypeParam list
}
with
  override this.ToString() = sprintf "%A" this
/// 列挙型は基本型とケースを持つ。
and EnumType = {
  EnumTypeName: string
  BaseType: NonEnumType
  Cases: (string * int) list
}
with
  override this.ToString() = sprintf "%A" this
/// 型定義は組み込み型か、別名定義か、列挙型定義のいずれか。
/// 別名定義の場合はオリジナルの列型定義を持つ。
and TypeDef =
  | BuiltinType of NonEnumType
  | AliasDef of NonEnumType * originalType: ColumnTypeDef
  | EnumTypeDef of EnumType
with
  override this.ToString() = sprintf "%A" this
/// 列型定義は、列の型定義と列の属性を持つ。
/// ただし、トップレベルの列の定義はBuiltinTypeにはならない(組み込み型は定義できない)。
and ColumnTypeDef = {
  ColumnSummary: Summary option
  ColumnTypeDef: TypeDef
  ColumnJpName: string option
  ColumnAttributes: ColumnAttribute list
}
with
  override this.ToString() = sprintf "%A" this
 
/// 列名はワイルドカードか、名前を持ったもののいずれか。
type ColumnName =
  | Wildcard
  | ColumnName of name: string * jpName: string option
with
  override this.ToString() = sprintf "%A" this
 
/// 列定義
type ColumnDef = {
  ColumnSummary: Summary option
  ColumnName: ColumnName
  ColumnType: ColumnTypeDef * ColumnAttribute list
}
with
  override this.ToString() = sprintf "%A" this
 
/// テーブル定義
type TableDef = {
  TableSummary: Summary option
  TableName: string
  TableJpName: string option
  ColumnDefs: ColumnDef list
}
with
  override this.ToString() = sprintf "%A" this

type Element =
  | TableDef of TableDef
  | ColTypeDef of ColumnTypeDef
  //| Comment of string
  //| BlankLine
with
  override this.ToString() = sprintf "%A" this
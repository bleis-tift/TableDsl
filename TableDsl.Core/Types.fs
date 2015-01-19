namespace TableDsl

type Attribute =
  | SimpleAttr of string
  | ComplexAttr of string * string list
with
  member this.Key =
    match this with SimpleAttr k | ComplexAttr (k, _) -> k
 
type ColumnAttributeValueElem =
  | Lit of string
  | Var of string
with
  override this.ToString() = sprintf "%A" this

/// 属性値は属性値の要素のリスト
type ColumnAttributeValue = ColumnAttributeValueElem list
 
/// サマリは単なる文字列型。
type Summary = string
 
/// 列の属性には、名前のみを持つSimpleAttrと、名前と値を持つComplexAttrがある。
/// SimpleAttrは、フラグを意味しており、存在する場合にそのフラグがtrueであることを示す。
type ColumnAttribute =
  | SimpleColAttr of string
  | ComplexColAttr of string * ColumnAttributeValue
with
  member this.Key =
    match this with SimpleColAttr k | ComplexColAttr (k, _) -> k
  member this.ToAttribute() =
    match this with
    | SimpleColAttr k -> SimpleAttr k
    | ComplexColAttr (k, v) -> ComplexAttr (k, [v |> List.map (function Lit l -> l) |> String.concat ""])
  override this.ToString() = sprintf "%A" this
 
/// 開いた型パラメータは、型変数か、文字列のいずれか。
type OpenTypeParam =
  | TypeVariable of string
  | BoundValue of string
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
  member this.TypeNameWithParams =
    let paramsStr =
      if this.TypeParameters.IsEmpty then ""
      else
        "(" + (this.TypeParameters |> List.map (function BoundValue p -> p) |> String.concat ", ") + ")"
    sprintf "%s%s" this.TypeName paramsStr
  override this.ToString() = sprintf "%A" this
/// 列挙型は基本型とケースを持つ。
and EnumType = {
  EnumTypeName: string
  BaseType: NonEnumType
  Cases: (string * string option * int) list
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
  member this.Summary =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> None
    | AliasDef (_, org) -> org.Summary
  member this.TypeName =
    match this with
    | BuiltinType t -> t.TypeNameWithParams
    | AliasDef (_, org) -> org.RootTypeName
    | EnumTypeDef t -> t.BaseType.TypeName
  member this.JpName =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> None
    | AliasDef (_, org) -> org.JpName
  member this.Attributes(parentAttrs: ColumnAttribute seq) =
    match this with
    | BuiltinType _ | EnumTypeDef _ -> Seq.empty
    | AliasDef (_, org) ->
        org.Attributes
        |> Seq.filter (fun (a: ColumnAttribute) -> parentAttrs |> Seq.forall (fun p -> a.Key <> p.Key))
  override this.ToString() = sprintf "%A" this
/// 列型定義は、列の型定義と列の属性を持つ。
/// ただし、トップレベルの列の定義はBuiltinTypeにはならない(組み込み型は定義できない)。
and ColumnTypeDef = {
  ColumnTypeDefSummary: Summary option
  ColumnTypeDef: TypeDef
  ColumnTypeDefJpName: string option
  ColumnTypeDefAttributes: ColumnAttribute list
}
with
  member this.Summary =
    match this.ColumnTypeDefSummary with
    | Some summary -> Some summary
    | None -> this.ColumnTypeDef.Summary
  /// coltype TypeName = ...のTypeName(パラメータを含まない)
  member this.TypeName =
    match this.ColumnTypeDef with
    | BuiltinType t
    | AliasDef (t, _) -> t.TypeName
    | EnumTypeDef t -> t.EnumTypeName
  member this.Params =
    match this.ColumnTypeDef with
    | BuiltinType t
    | AliasDef (t, _) -> t.TypeParameters
    | EnumTypeDef t -> t.BaseType.TypeParameters
  member this.RootTypeName =
    match this.ColumnTypeDef with
    | BuiltinType t -> t.TypeNameWithParams
    | AliasDef (_, org) -> org.RootTypeName
    | EnumTypeDef t -> t.BaseType.TypeNameWithParams
  member this.JpName =
    match this.ColumnTypeDefJpName with
    | Some jpName -> Some jpName
    | None -> this.ColumnTypeDef.JpName
  member this.Attributes =
    seq {
      let attrs = this.ColumnTypeDefAttributes
      yield! attrs
      yield! this.ColumnTypeDef.Attributes(attrs)
    }
  override this.ToString() = sprintf "%A" this

/// 列の型。ただし、ColumnTypeDefとは違い、ネストが解決された情報を持つ。
type ColumnType = {
  /// 組み込み型を表す文字列
  RootType: string
  ColumnTypeSummary: Summary option
  ColumnTypeJpName: string option
  /// 列定義で指定されたすべての属性
  ColumnTypeAttributes: Attribute list
}
with
  static member Create(rootType, summary, jpName, attrs) =
    {
      RootType = rootType
      ColumnTypeSummary = summary
      ColumnTypeJpName = jpName
      ColumnTypeAttributes = attrs
    }

/// 非列挙型(NonEnum)と列挙型(Enum)の型情報
type TypeInfo =
  | NonEnum of ColumnType
  | Enum of ColumnType * (string * string option * int) list
with
  static member FromColumnTypeDef(colTypeDef: ColumnTypeDef) =
    let name = colTypeDef.RootTypeName
    let summary = colTypeDef.Summary
    let jpName = colTypeDef.JpName
    let attrs = colTypeDef.Attributes |> Seq.map (fun a -> a.ToAttribute()) |> Seq.toList
    match colTypeDef.ColumnTypeDef with
    | BuiltinType _ -> NonEnum (ColumnType.Create(name, summary, jpName, attrs))
    | AliasDef (_, org) ->
        NonEnum (ColumnType.Create(name, summary, jpName, attrs))
    | EnumTypeDef t -> Enum (ColumnType.Create(name, summary, jpName, attrs), t.Cases)
  member this.ColumnType =
    match this with NonEnum t | Enum (t, _) -> t
  member this.Attributes =
    this.ColumnType.ColumnTypeAttributes

/// 列の型の参照
type ColumnTypeRef = {
  Type: TypeInfo
  IsNullable: bool
  /// 参照名。列定義の名前が指定される場合と、組み込み型の名前が指定される場合がある。
  ColumnTypeRefName: string
  ColumnTypeRefParams: string list
  /// 型の参照時に追加指定される属性(ColName : { type with index })
  ColumnTypeRefAttributes: Attribute list
}
with
  member this.AllAttrbutes =
    seq {
      let attrs = this.ColumnTypeRefAttributes
      yield! attrs
      yield! this.Type.Attributes |> Seq.filter (fun a -> attrs |> Seq.forall (fun a2 -> a2.Key <> a.Key))
    }
 
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
  ColumnType: ColumnTypeRef
}
with
  override this.ToString() = sprintf "%A" this
 
/// テーブル定義
type TableDef = {
  TableSummary: Summary option
  TableAttributes: Attribute list
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
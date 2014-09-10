namespace TableDsl

type Summary = string

type Attr =
  | SimpleAttr of string
  | ComplexAttr of string * string

type TypeParameterDef =
  | Open
  | Close

type TypeDef =
  | 

type EnumType = {
  BaseType: TypeDef

}

type AliasType = {
  OriginType: TypeDef
  Attributes: Attr list
}

type ColumnTypeDef =
  | AliasType of AliasType
  | EnumType of EnumType

type ColumnName =
  | Wildcard
  | ColumnName of name:string * jpName:string option

/// 型パラメータとして許されるのは、数値か、開かれた型パラメータを持たない型(TypeRef)
type TypeParam =
  | ConstNumParam of int
  | TypeRefParam of TypeRef

/// 開かれた型パラメータを持たない型をTypeRefと呼ぶ
and TypeRef =
  | AtomicTypeRef of TypeDef // 型パラメータを持たないTypeDefをAtomicTypeRefとする(型パラメータを持つTypeDefも指定できてしまうので注意)
  | NestedTypeRef of TypeDef * TypeParam list // 型パラメータを持つTypeDefに型パラメータを適用したものをNestedTypeRefとする

type TypeDef' = unit

type ClosedTypeParam =
  | LiteralNum of int
  | ClosedTypeParam of ClosedTypeRef

and ClosedTypeRef = {
  Name: string
  TypeParameters: ClosedTypeParam list
}

type OpenTypeParam =
  | LiteralNum of int
  | TypeVariable of string
  | NestedType of OpenTypeRef

and OpenTypeRef = {
  Name: string
  TypeParameters: OpenTypeParam list
}

type ColumnTypeDef' =
  | ClosedColTypeDef of ClosedTypeRef
  | OpenColTypeDef of OpenTypeRef
  | EnumTypeDef of EnumType

type ColumnDef = {
  ColumnSummary: Summary option
  ColumnName: ColumnName
  TypeRef: ClosedTypeRef
}

type TableDef = {
  TableSummary: Summary option
  TableName: string
  TableJpName: string option
  ColumnDefs: ColumnDef list
}
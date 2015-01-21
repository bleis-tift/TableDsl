namespace TableDsl

type ClusteredType =
  | NonClustered
  | Clustered

type LayoutOrder =
  | Asc
  | Desc

type SingleTableKeyColumn = {
  Name: string
  LayoutOrder: LayoutOrder
}

type SingleTableKey = {
  Name: string
  ClusteredType: ClusteredType
  Columns: SingleTableKeyColumn list
}

type ColumnPair = {
  OwnColumnName: string
  ParentColumnName: string
}

type RelationshipKey = {
  KeyName: string
  RelationshipTableName: string
  ColumnPairs: ColumnPair list
}

type DefaultConstraint = {
  Name: string
  ColumnName: string
  Value: string
}

type Key =
  | PrimaryKey of SingleTableKey
  | UniqueKey of SingleTableKey
  | Index of SingleTableKey
  | ForeignKey of RelationshipKey
  | DefaultConstraint of DefaultConstraint

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Key =
  val collectTableKeys: tableDef:TableDef -> Key list
  val collectKeys: elems:Element list -> (TableDef * Key list) list

namespace ProdLang0

module ProdLang =
    type Object = string
    type Variable = string
    type Value = string
    type ProductionId = string

    type ObjectVar = Object * Variable

    type ConditionExpression = ExpValue of Value | ExpVariable of Variable

    type Condition = Eq of ObjectVar * ConditionExpression | TRUE
    type Production = Condition list * ProductionId

    type System = {productions : Production list}

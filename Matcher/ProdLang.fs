namespace Matcher

module ProdLang =
    type Object = string
    type Variable = string
    type Value = string

    type ObjectVar = Object * Variable

    type Condition = Eq of ObjectVar * Value | TRUE
    type Production = Condition list * string

    type System = {productions : Production list}

﻿namespace ProdLang0

open FParsec
open ProdLang0.ProdLang

module Parser =

    let wsc = spaces

    let isAsciiIdStart c = isAsciiLetter c || c = '_' || c = '?'

    let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '\''

    let id = identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                                           isAsciiIdContinue = isAsciiIdContinue)) .>> wsc

    let objVar = id .>> pstring "." .>>. id

    let value = pfloat .>> wsc |>> fun cs -> ExpValue(cs.ToString())

    let expVar = id |>> fun id -> ExpVariable id

    let exp = value <|> expVar

    let equals = objVar .>> pstring "=" .>> wsc .>>. exp |>> fun cs -> Eq cs

    let comp = stringReturn "true" TRUE .>> wsc <|> equals

    let prod = sepBy1 comp (pstring "and" .>> wsc) .>> pstring ">>" .>> wsc .>>. id

    let prods : Parser<System,Unit> = wsc >>. many prod |>> fun prods -> {productions = prods}
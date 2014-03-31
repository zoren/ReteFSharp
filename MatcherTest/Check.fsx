#r @"bin\\Debug\CoreLib.dll"
#r @"bin\\Debug\Matcher.dll"
#r @"bin\\Debug\FsCheck.dll"
#r @"bin\\Debug\ProdLang0.dll"

#load "Check.fs"
open FsCheck
open MatcherTest.Check

open ProdLang0.ProdLang

// Check.Quick referenceAndReteAgree
Check.Verbose referenceAndReteAgree

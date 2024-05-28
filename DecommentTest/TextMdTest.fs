module TextMdTest

open NUnit.Framework
open Decomment
    open ResultMd
    open TextMd

let tabLen = 4

[<SetUp>]
let Setup () =
    ()

[<TestCase("foo","foo")>]

[<TestCase(" foo", " foo")>]
   
[<TestCase("foo", "foo ")>]

[<TestCase("foo", "foo  ")>]

[<TestCase("foo bar", "foo bar ")>]


let removeTrailingSpacesTest
        expected src =
    
    createLine src
    >>= _removeTrailingSpaces tabLen
    |> fun res ->            
        match res with
        | Ok actual ->
            Assert.AreEqual(expected, valueLine actual)
        | Error msg ->
            failwith msg    



[<TestCase(
@"hello,
 world
  !

",
@"hello, 
 world  
  ! 
  
",
false)>]

[<TestCase(
@"hello,
 world
  !
  
",
@"hello, 
 world  
  ! 
  
",
true)>]

let removeSpacesTest
        expected src exceptIndents =
    createText src
    <&> flip (removeSpaces tabLen) exceptIndents
    |> join
    |> fun res ->
        match res with
        | Ok actual ->
            Assert.AreEqual(expected, valueText actual)
        | Error msg ->
            failwith msg


[<TestCase(
false,
"Hello"
)>]

[<TestCase(
true,
"  "
)>]

[<TestCase(
true,
""
)>]

let isEndWithSpaceLineTest
        expected src =
    createLine src
    <&> isSpaceLine
    |> fun res ->
        match res with
        | Ok actual ->
            Assert.AreEqual(expected, actual)
        | Error msg ->
            failwith msg



[<TestCase(
@"1
2
3
",
@"1
 
2
 
3

  
"
)>]

let removeEmptyLinesTest
        expected src =
    
    createText src
    <&> removeEmptyLines
    |> join
    |> fun res ->
        match res with
        | Ok actual ->
            Assert.AreEqual(expected, valueLineOrText actual)
        | Error msg ->
            failwith msg
            
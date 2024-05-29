module DecommentMdTest

open NUnit.Framework
open Decomment
    open ResultMd

let tabLen = 4

[<SetUp>]
let Setup () =
    ()

[<TestCase(
    @"foo 
    ", // expected
    @"foo // bar
    ", // src
    ".cs", // ext
    false  // except document comments
    )>]

[<TestCase(
    @"foo 
    ",
    @"/* baz */foo /// bar
    ",
    ".cs",
    false
    )>]

[<TestCase(
    @"foo
    
    ",
    @"/* baz */foo
    /// bar
    ",
    ".cs",
    false
    )>]

[<TestCase(
    @"/** baz */foo 
    /// bar
    ",
    @"/** baz */foo // hoge
    /// bar
    ",
    ".cs",
    true
    )>]


// C#
[<TestCase(
@"using System;

public class Sample
{
    public Sample()
    {
        var msg = @""Hello //,
                    C#!"";
        Console.WriteLine(msg);
    }
    
    
    
     
     
     
     
    
     
     
     
}

",
@"using System;

public class Sample
{
    public Sample()
    {
        var msg = @""Hello //,
                    C#!"";
        Console.WriteLine(msg);
    }
    // Single-line comment //
    /// Single-line document comment ///
    /*
     * multiline
     * comments /*
     */
     
    /**
     * multiline
     * document comments /*
     */
}
// End1
",
        ".cs", false)>]

[<TestCase(
@"
using System;

public class Sample
{
    public Sample()
    {
        var msg = @""Hello //,
                    C#!"";
        Console.WriteLine(msg);
    }
    
    
    
     
     
     
    
    /// Single-line document comment ///
    
    /**
     * multiline
     * document comments
     /// Hello C# ///
     */
}",
@"// Begin2
using System;

public class Sample
{
    public Sample()
    {
        var msg = @""Hello //,
                    C#!"";
        Console.WriteLine(msg);
    }
    // Single-line comment //
    
    /*
     * multiline
     * comments /*
     */
    
    /// Single-line document comment ///
    
    /**
     * multiline
     * document comments
     /// Hello C# ///
     */
}
// End2",
        ".cs", true)>]

// F#
[<TestCase(
@"open System

[<EntryPoint>]
let main argv =
    printfn ""Hello // World from F#!""
    0 
    
    
    
    
     
     
     
    
",
@"open System

[<EntryPoint>]
let main argv =
    printfn ""Hello // World from F#!""
    0 // return an integer exit code
    
    // Single-line comment
    
    (*
     * Multiline
     * comments (*
     *)
    // Single-line comment
",
        ".fs", false)>]


[<TestCase(
@"open System

[<EntryPoint>]
let main argv =
    printfn ""Hello // World from F#!""
    0 
    ",
@"open System

[<EntryPoint>]
let main argv =
    printfn ""Hello // World from F#!""
    0 // return an integer exit code    
    //"

,
        ".fs", false)>]


[<TestCase(
@"
import sys

def main():
    
    
    
    
    
    
    
    
     
    
    
        
        
        
        
    
    
    
    
    print('Hello \' "" # Python2')
    print("" # Python3 \
        world"")
"
,

@"
import sys

def main():
    """"""
    multi-line
    comments1
    """"""
    
    '''
    multi-line
    comments2
    ''' 
    
    """"""
        '''
        multi-line
        comments3
        '''
    """"""
    
    # single-line comment
    
    print('Hello \' "" # Python2')
    print("" # Python3 \
        world"")
"
,
        ".py", false)>]


let decommentTest expected src (extStr:string) exceptDocCmt =    
    let res =
        DecommentMd.createExt extStr
        >>= fun ext ->
            DecommentMd.decomment
                ext
                tabLen
                exceptDocCmt
                src
    
    match res with
    | Ok actual ->            
        Assert.AreEqual(expected, actual)
    | Error msg ->
        failwith msg
    
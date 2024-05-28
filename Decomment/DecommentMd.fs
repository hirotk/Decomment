module Decomment.DecommentMd

open System
open System.Text.RegularExpressions
open Decomment.TextMd
open ResultMd


let strRegEx = @"""([^""]*)""";
let verbatimStrRegEx = @"@(""[^""]*"")+";

let CsS = ".cs"
let FsS = ".fs"
let JsS = ".js"
let JavaS = ".java"
let CppS = ".cpp"
let MlS = ".ml"
let HsS = ".hs"
let PyS = ".py"

type Extension =
    | CsE of string
    | FsE of string
    | JsE of string
    | JavaE of string
    | CppE of string
    | MlE of string
    | HsE of string
    | PyE of string

let createExt (extStr:string) =
    match extStr with
    | s when s = CsS -> Ok (CsE s)
    | s when s = FsS -> Ok (FsE s)
    | s when s = JsS -> Ok (JsE s)    
    | s when s = JavaS -> Ok (JavaE s)    
    | s when s = CppS -> Ok (CppE s)
    | s when s = MlS -> Ok (MlE s)
    | s when s = HsS -> Ok (HsE s)
    | s when s = PyS -> Ok (PyE s)
    | _ -> Error "Unsupported extension!"

let valueExt (ext:Extension) =
    match ext with
    | CsE s -> s
    | FsE s -> s
    | JsE s -> s
    | JavaE s -> s
    | CppE s -> s
    | MlE s -> s
    | HsE s -> s
    | PyE s -> s            
              
    
let private _getLineCommentRegEx (ext:Extension) =
        match ext with
        | HsE _ -> @"--(.*?)\r?\n"
        | PyE _ -> @"#(.*?)\r?\n"
        | JsE _ 
        | JavaE _ 
        | CppE _
        | CsE _ -> @"//(.*?)\r?\n"
        | FsE _ -> @"//(.*?)\r?\n"
        | MlE _ -> "_none_"

let private _getBlockCommentRegEx (ext:Extension) =
        match ext with
        | HsE _ -> (@"{\-(.*?)\-}", None)
        | PyE _ -> (@"""""""(.*?)""""""", Some @"'''(.*?)'''")
        | JsE _ 
        | JavaE _ 
        | CppE _
        | CsE _ -> (@"\/\*(.*?)\*\/", None)
        | MlE _
        | FsE _  -> (@"\(\*(.*?)\*\)", None)        


let private _getStartOfLineComment (ext:Extension) =
        match ext with
        | HsE _ -> @"--"
        | PyE _ -> @"#"
        | JsE _
        | JavaE _
        | CppE _
        | CsE _
        | FsE _ -> @"//"
        | MlE _ -> "_none_"
        
let private _getStartOfBlockComment (ext:Extension) = 
        match ext with
        | HsE _ -> (@"{-", None)
        | PyE _ -> (@"""""""",Some @"'''")
        | JsE _
        | JavaE _
        | CppE _
        | CsE _ -> (@"/*", None)
        | MlE _
        | FsE _ -> (@"(*", None)                    
 
        
let private _getStartOfDocLineComment (ext:Extension) =
        match ext with
        | CppE _
        | CsE _
        | FsE _ -> @"///"
        | _ ->
            "_none_"            

let private _getStartOfDocBlockComment (ext:Extension) =
        match ext with
        | JsE _
        | JavaE _
        | CppE _
        | CsE _ -> @"/**"
        | FsE _ -> @"(**"
        | _ ->
            "_none_"     
             
        
let private _createCommentRegEx
            (ext:Extension) strRegEx verbatimStrRegEx
            lineCommentRegEx blockCommentRegEx =
        let bc1, bc2Op =
            blockCommentRegEx
        match ext with
        | MlE _ ->
             bc1 + "|" +
             strRegEx
        | HsE _ ->
             bc1 + "|" +
             lineCommentRegEx + "|" +
             strRegEx
        | PyE _ ->
            match bc2Op with
            | Some bc2 ->
                bc1 + "|" +
                bc2 + "|" +
                lineCommentRegEx + "|" +
                strRegEx + "|" +
                @"'([^']*)'"
            | None ->
                bc1 + "|" +
                lineCommentRegEx + "|" +
                strRegEx + "|" +
                @"'([^']*)'"
        | JsE _
        | JavaE _
        | CppE _ ->
            bc1 + "|" +
            lineCommentRegEx + "|" +
            strRegEx
        | CsE _ 
        | FsE _ ->
            bc1 + "|" +
            lineCommentRegEx + "|" +
            strRegEx + "|" +
            verbatimStrRegEx


let private _getEscapeQuoteFun : Extension ->
        Result<string->string, ErrMsg> =
    fun extension ->
        try
            match extension with
            | PyE _ -> fun (s:string) ->
                            s.Replace(@"\""", "_dq_")
                             .Replace(@"\'", "_sq_")
                       |> Ok
            | HsE _
            | JsE _
            | JavaE _
            | CppE _
            | CsE _
            | MlE _
            | FsE _ ->  fun (s:string) ->
                            s.Replace(@"\""", "_dq_")
                        |> Ok
        with
        | ex ->
            Error ex.Message

let private _getUnescapeQuoteFun : Extension ->
        Result<string->string, ErrMsg> =
    fun extension ->
        try
            match extension with
            | PyE _ -> fun (s:string) ->
                           s.Replace("_dq_", @"\""")
                            .Replace("_sq_", @"\'")
                       |> Ok
            | HsE _
            | JsE _
            | JavaE _
            | CppE _
            | CsE _
            | MlE _
            | FsE _ ->  fun (s:string) ->
                            s.Replace("_dq_", @"\""")
                        |> Ok
        with
        | ex ->
            Error ex.Message

        
let isStartOfComment : string->string*string option->string ->
        Result<bool, ErrMsg> =
    fun lineComment blockComment code ->
        try
            match blockComment with
            | bc1, Some bc2 ->
                code.StartsWith lineComment ||
                code.StartsWith bc1 ||
                code.StartsWith bc2
            | bc1, None ->            
                code.StartsWith lineComment ||
                code.StartsWith bc1
            |> Ok
        with
        | ex ->
            Error ex.Message
        
                
let isStartOfDocComment : string->string->string ->
        Result<bool, ErrMsg> =
    fun lineDocComment blockDocComment code ->
        try
            (code.StartsWith lineDocComment ||
             code.StartsWith blockDocComment)
            |> Ok
        with
        | ex ->
            Error ex.Message
        
        
let createMatchEvaluator :
        bool->
        string->
        (string -> Result<bool,ErrMsg>)->
        (string -> Result<bool,ErrMsg>)->
        Result<MatchEvaluator, ErrMsg> =
    fun exceptDocComment startOfLineComment
        isComment isDocComment ->
        try
            let matcher =
                fun (mc:Match) ->           
                    (fun isDocCmt isCmt ->
                        match exceptDocComment && isDocCmt,
                            isCmt with
                        | true, _ ->
                            mc.Value
                        | _, true when mc.Value.StartsWith(startOfLineComment) ->
                            Environment.NewLine
                        | _, true ->
                            mc.Value.Split(Environment.NewLine)
                            |> fun (sa:string array) ->
                                Array.map
                                    (fun line ->
                                        Regex.Replace(line, "^(\\s*).*$", "$1")
                                    )
                                    sa
                            |> fun (sa:string array) ->
                                 String.Join(Environment.NewLine, sa)
                        | _ ->
                            mc.Value
                    )
                    <%> (isDocComment mc.Value)
                    <*> (isComment mc.Value)                    
                    |> fun res ->
                        match res with
                        | Ok ret ->
                            ret
                        | Error msg ->
                            failwith msg
            
            MatchEvaluator(matcher)
            |> Ok
        with
        | ex ->
            Error ex.Message
       
        
let reReplace :
        RegexOptions->
        MatchEvaluator->
        string->
        string->
        Result<string,ErrMsg> =                    
    fun regOptions matchEvaluator pattern src ->
    try        
        Regex.Replace(
            src,
            pattern,
            matchEvaluator,
            regOptions)
        |> Ok
    with
    | ex ->
        Error ex.Message        
        
        
let canHaveDocComment (ext:Extension) =
    match ext with
    | CsE _
    | FsE _
    | JsE _
    | JavaE _
    | CppE _ -> true
    | _ -> false
    
    
let decomment : Extension->bool->string ->
        Result<string, ErrMsg> =
    fun ext exceptDocCmt src ->
        let isComment =
            isStartOfComment
                (_getStartOfLineComment ext)
                (_getStartOfBlockComment ext)            
        
        let exceptDocCmt =
                exceptDocCmt && canHaveDocComment ext
        
        let isDocComment =
            match exceptDocCmt with
            | true ->
                isStartOfDocComment
                    (_getStartOfDocLineComment ext)
                    (_getStartOfDocBlockComment ext)
            | false ->
                fun _ -> Ok false        

        let re = _createCommentRegEx                    
                    ext
                    strRegEx
                    verbatimStrRegEx
                    (_getLineCommentRegEx ext)
                    (_getBlockCommentRegEx ext)        
        
        let escapeRes = _getEscapeQuoteFun ext
        
        let unescapeRes = _getUnescapeQuoteFun ext
        
        let replaceRes =
                createMatchEvaluator
                    exceptDocCmt
                    (_getStartOfLineComment ext)
                    isComment
                    isDocComment
                <&> fun matchEval ->                            
                    reReplace
                        RegexOptions.Singleline
                        matchEval
                        re
        
        (fun escape replace unescape -> 
            createLineOrText src
            >>= fun lineOrText ->
                match lineOrText with
                | LineTC line ->
                    valueLine line
                    |> createLineOrText
                    >>= addEmptyLineAtLast
                    <&> valueText
                    <&> escape
                    >>= replace
                    <&> unescape
                    >>= createText
                    >>= removeEmptyLineFromLast
                    <&> valueLineOrText                
                    
                | LTextC text ->
                    match text with                
                    | TextC (s,true) ->
                        escape s
                        |> replace
                        <&> unescape

                    | TextC (s,false) ->
                        createLineOrText s
                        >>= addEmptyLineAtLast
                        <&> valueText
                        <&> escape
                        >>= replace
                        <&> unescape
                        >>= createText
                        >>= removeEmptyLineFromLast
                        <&> valueLineOrText
        )
        <%> escapeRes
        <*> replaceRes
        <*> unescapeRes
        |> join

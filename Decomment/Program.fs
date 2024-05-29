module Decomment.Program

open System
open Decomment.ResultMd
open Decomment.DecommentMd
open Decomment.TextMd

let Success = 0
let Usage = @"Usage: Decomment [-d] [-e] <path>
    -d : remove comments except for document comments
    -e : replace comments with indented empty lines 
    <path> : path to a source code file
             (*.cs, *.fs, *.java *.cpp, *js, *.ml, *.hs, *.py)"

let tabLen = 4

let private _tryGetPathArg (args:string list) =
        List.sort args
        |> fun args ->
        match args with
        | "-d"::"-e"::arg3::_ ->
            PathMd.createPath arg3
            <&> fun path -> (path, true, true)
        | "-d"::arg2::_ ->
            PathMd.createPath arg2
            <&> fun path -> (path, true, false)
        | "-e"::arg2::_ ->
            PathMd.createPath arg2
            <&> fun path -> (path, false, true)            
        | "-h"::_ ->
            Error Usage            
        | arg1::_ ->
            PathMd.createPath arg1
            <&> fun path -> (path, false, false)            
        | _ -> Error Usage


let parseArgs : string array ->
        Result<PathMd.Path*Extension*bool*bool, ErrMsg> =
    fun args ->
        if args.Length < 1 || 3 < args.Length then
            Error Usage
        else            
            try 
                Ok (args |> Array.toList)
            with
            | ex ->
                Error ex.Message
            |> fun argLsRes ->    
                argLsRes
                >>= _tryGetPathArg
                >>= fun (path, docCmt, empLn) ->
                        PathMd.getExt
                            [CsS; FsS; JsS; JavaS; CppS; MlS; HsS; PyS]
                            (PathMd.valuePath path)
                        >>= DecommentMd.createExt
                        >>= fun ext ->
                                match DecommentMd.canHaveDocComment ext with
                                | false when docCmt ->
                                    Error "The -d option is not supported for the input file!"
                                | _ ->
                                    Ok ext                            
                        <&> fun ext ->
                                (path, ext, docCmt, empLn)        


let _main : string array -> unit =
    fun args ->
        args
        |> parseArgs
        >>= fun (path, ext, exceptDocCmt, replaceWithEmptyLine) ->
                PathMd.readFile path
                >>= (DecommentMd.decomment ext tabLen exceptDocCmt) 
                >>= TextMd.createLineOrText                    
                >>= (fun lt ->
                        match lt with
                        | LineTC _ ->
                            Ok lt
                        | LTextC text ->
                            (match replaceWithEmptyLine with
                            | true ->
                                // true : Leave indents as they are
                                TextMd.removeSpaces tabLen text true
                                >>= fun t ->
                                        createLineOrText (valueText t)
                            | false ->
                                TextMd.removeSpaces tabLen text false
                                >>= fun t ->
                                        TextMd.removeEmptyLines t
                            )
                    )
                <&> fun lt ->                
                        (path, TextMd.valueLineOrText lt)
                
        >>= fun (path, ltStr) ->
                PathMd.writeFile
                    path
                    ltStr
                    (Some "_dc")                
        
        |> fun res ->
                match res with
                | Ok _  -> ()
                | Error errMsg ->
                    Console.Error.WriteLine errMsg

[<EntryPoint>]
let main args =
    _main args
    Success

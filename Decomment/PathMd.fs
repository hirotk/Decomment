module Decomment.PathMd

open System.IO
open ResultMd


type Path =
    | PathC of string


let getExt : string list->string->
        Result<string, ErrMsg>=
    fun supportedExts path ->
        try
            Path.GetExtension path
            |> fun ext ->        
                match ext with
                 | s when List.contains s supportedExts ->
                     Ok ext
                 | _ ->
                     Error $"{ext} file is not supported!"
        with
        | ex ->
            Error ex.Message

let getFullPath : string->
        Result<string, ErrMsg> =
    fun path ->
        try
            Path.GetFullPath path
            |> fun fullPath ->
                match Path.Exists fullPath with
                | true -> Ok fullPath
                | false -> Error $"{path} does not exist!"
        with
        | ex ->
            Error ex.Message


let createPath : string->
        Result<Path, ErrMsg> =
    fun pathStr ->
        getFullPath pathStr
        <&> fun _ -> PathC pathStr 

let valuePath : Path->
        string =
    fun path ->
        match path with
        | PathC p -> p


let readFile : Path ->
        Result<string,ErrMsg> =
    fun path ->
        try
            valuePath path
            |> File.ReadAllText
            |> Ok
        with
        | ex ->
            Error ex.Message

let writeFile : Path->string->string option ->
        Result<unit,ErrMsg> =
    fun path text postfixOp ->
        try
            let pathStr = valuePath path
            let dir = Path.GetDirectoryName pathStr
            let fileName = Path.GetFileNameWithoutExtension pathStr
            let ext = Path.GetExtension pathStr
            let postfix =
                match postfixOp with
                | None ->
                    "_fixed"
                | Some pf ->
                    pf
            let newPath =
                Path.Combine(
                    dir,
                    fileName + postfix + ext)            
            Ok (File.WriteAllText(newPath, text))
        with
        | ex ->
            Error ex.Message    

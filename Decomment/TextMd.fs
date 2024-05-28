module Decomment.TextMd

open System
open System.Text.RegularExpressions
open ResultMd


type Line =
    | LineC of string

let createLine : string->
        Result<Line, ErrMsg> =
    fun lineStr ->
        try
            match 1 = lineStr
                    .Split(Environment.NewLine)
                    .Length with
            | true -> Ok (LineC lineStr)
            | false -> Error "Line does not have a line feed!"
        with
        | ex ->
            Error ex.Message
        
let valueLine : Line->
        string =
    fun line ->
        match line with
        | LineC l -> l


type Text =
    | TextC of string*bool  // bool: has the empty line at last

let createText : string->
        Result<Text, ErrMsg> =
    fun textStr ->
        try
            match 1 < textStr
                    .Split(Environment.NewLine)
                    .Length with
            | true ->
                match Regex.IsMatch(textStr, "\\s*\\r?\\n$") with
                | true -> Ok (TextC (textStr,true))
                | false -> Ok (TextC (textStr,false))
            | false ->
                Error "Text has at least one line feed!"                
        with
        | ex ->
            Error ex.Message
        
let valueText : Text->
        string =
    fun text ->
        match text with
        | TextC (t,_) -> t

let concatLines : Line list->
        Result<Text,ErrMsg> =
    fun lines ->
        try
            lines
            |> List.map valueLine
            |> String.concat Environment.NewLine
            |> createText
        with
        | ex ->
            Error ex.Message


let splitText : Text ->
        Result<Line list, ErrMsg> =
    fun text ->
        try
            valueText text
            |> fun s -> s.Split(Environment.NewLine)
            |> Array.map createLine
            |>  fun results ->
                match Array.tryFind Result.isError results with
                | Some err ->
                        match err with
                        | Error x -> Error x
                        | _ -> failwith "Unreachable"                    
                | None ->
                    let valueOk r =
                       match r with
                       | Ok x -> x
                       | _ -> failwith "Unreachable"
                    Ok (Array.map valueOk results)                
            <&> Array.toList
            >>= fun ls ->
                    match text with
                    | TextC (_,false) ->
                        createLine String.Empty
                        <&> fun last ->
                            List.append ls [last]                        
                    | TextC (_,true) ->
                        Ok ls                
        with
        | ex ->
            Error ex.Message


type LineOrText =
    | LTextC of Text
    | LineTC of Line

let createLineOrText : string->
        Result<LineOrText, ErrMsg> =
    fun str ->
    match 1 < str
            .Split(Environment.NewLine)
            .Length with
    | true ->
        createText str
        <&> LTextC
    | false ->
        createLine str
        <&> LineTC
        
let valueLineOrText : LineOrText->
        string =
    fun lineOrtext ->
        match lineOrtext with
        | LTextC t -> valueText t
        | LineTC l -> valueLine l

let addEmptyLineAtLast : LineOrText->
        Result<Text,ErrMsg> =
    fun lineOrText ->                
        createText
            (valueLineOrText lineOrText + Environment.NewLine)

let removeEmptyLineFromLast : Text ->
        Result<LineOrText,ErrMsg> =
    fun text ->
        try            
            valueText text
            |> fun s ->
                createLineOrText
                    (Regex.Replace(s, "\\r?\\n$", ""))
        with
        | ex ->
            Error ex.Message

let isSpaceLine : Line -> bool =
    fun line ->
        valueLine line
        |> fun s ->
            Regex.IsMatch(s, "^\\s*$")


let replaceTabToSpace : int32->LineOrText ->
        Result<LineOrText,ErrMsg> =
    fun tabLen lineOrtext ->
        try
            valueLineOrText lineOrtext
            |> fun s ->
                s.Replace("\t", String(' ', tabLen))
            |> createLineOrText
        with
        | ex ->
            Error ex.Message                  

let _removeSpaces : int32->Line ->
        Result<Line, ErrMsg> =
    fun tabLen line ->
        try
            valueLine line
            |> createLineOrText
            >>= replaceTabToSpace tabLen 
            <&> valueLineOrText
            <&> fun s ->
                    Regex.Replace(s, "\\s+$", "")
            >>= createLine
        with
        | ex ->
            Error ex.Message
                                
let _removeTrailingSpaces : int32->Line ->
        Result<Line,ErrMsg> =
    fun tabLen line ->
        try
            valueLine line
            |> createLineOrText
            >>= replaceTabToSpace tabLen 
            <&> valueLineOrText
            <&> (fun s ->
                    match Regex.IsMatch(s, "^\\s+$") with
                    | true -> s
                    | false -> Regex.Replace(s, "\\s+$", "")
                )
            >>= createLine
        with
        | ex ->
            Error ex.Message

                   
let removeSpaces : int32->Text->bool ->
        Result<Text,ErrMsg> =
    fun tabLen text exceptEmptyIndents ->
        try
            valueText text
            |> fun s ->
                s.Split(Environment.NewLine)
            |> Array.map
                (fun s ->
                    createLine s
                    >>= fun line ->
                        match exceptEmptyIndents with
                        | true -> _removeTrailingSpaces tabLen line
                        | false -> _removeSpaces tabLen line
                )
            |>  fun results ->
                match Array.tryFind Result.isError results with
                | Some err ->
                        match err with
                        | Error x -> Error x
                        | _ -> failwith "Unreachable"                    
                | None ->
                    let valueOk r =
                       match r with
                       | Ok x -> x
                       | _ -> failwith "Unreachable"
                    Ok (Array.map valueOk results)
            >>= fun removedLines ->
                    concatLines (Array.toList removedLines)
        with
        | ex ->
            Error ex.Message

let removeEmptyLines : Text ->
        Result<LineOrText, ErrMsg> =
    fun text ->
        try
            text
            |> splitText
            >>= fun lines ->
                    lines
                    |> List.filter
                           (fun line -> not (isSpaceLine line))
                    |> fun lines ->
                        match lines.Length with
                        | 1 ->
                            createLineOrText (valueLine lines.Head)
                        | _ ->
                            concatLines lines                    
                            >>= fun text ->
                                    match text with
                                    | TextC (s,true) ->
                                        Ok s
                                    | TextC (s,false) ->
                                        createLineOrText s
                                        >>= addEmptyLineAtLast
                                        <&> valueText

                            >>= createLineOrText                        

        with
        | ex ->
            Error ex.Message
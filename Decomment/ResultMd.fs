module Decomment.ResultMd

type ErrMsg = string

let flip f x y = f y x

let (=<<) = Result.bind
let (>>=) x = flip (=<<) x

let (<%>) = Result.map  
let (<&>) x = flip (<%>) x

let ap = fun f x -> f >>= (<&>) x        
let (<*>) = ap

let join x = x >>= id

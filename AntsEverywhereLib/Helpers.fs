//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missle Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereLib.Helpers

open System
open System.Reflection

module Array2D = 
    let foldi f arr i = 
        let s = ref i in Array2D.iteri (fun x y cur -> s := (f x y !s cur)) arr 
        !s 

module Array =
    let randomPermute a =
        let n = Array.length a
        let rand = new Random()
        let rec aux = function
            | 0 -> a
            | k ->
                let i = rand.Next(k+1)
                let tmp = a.[i]
                a.[i] <- a.[k]
                a.[k] <- tmp
                aux (k-1)
        aux (n-1)

module Seq = 
    let randomPermute a =
        a |> Seq.toArray |> Array.randomPermute |> Array.toSeq

module List = 
    let rec combinations n l =
        match (n,l) with
        | (0,_) -> [[]]
        | (_,[]) -> []
        | (n,x::xs) ->
            let useX = List.map (fun l -> x::l) (combinations (n-1) xs)
            let noX = combinations n xs
            useX @ noX

    let private r = Random(int DateTime.Now.Ticks)
    let random l =
        let index = r.Next(0, List.length l) in
            l.[index]

let input (x : 'a) app = app x
let inline holds (v : 'a) (conds : ('a -> bool) list) = List.forall (fun cond -> cond v) conds
let inline (<?<) l r = (fun x -> x > l && x < r)
let inline (<=?<) l r = (fun x -> x >= l && x < r)
let inline (<?=<) l r = (fun x -> x > l && x <= r)
let inline (<=?=<) l r = (fun x -> x >= l && x <= r)





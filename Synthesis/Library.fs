module Synthesis

let abelar y =
  (12 < y) && (y<3097) && (y%12 = 0)
   // failwith "Not implemented"

(* 1. Create a function abelar to return true if the input is greater than 12, and less than
3097, and is a multiple of 12. [target: 1 line] *)

let area b h =
  match h<0.0 || b<0.0 with
    |true -> failwith "Negative Value"
    |false -> ((b * 0.5) * h)
 //   failwith "Not implemented"

let zollo p =
 match p>0 with
    |true -> 2 * p
    |false -> abs p
   // |_ failwith "Exception"

let min a b =
 match a<b with
   |true -> a
   |false -> b
   // failwith "Not implemented"

let max a b =
 match a<b with
   |true -> b
   |false -> a
   // failwith "Not implemented"

let ofTime h m s =
  ((h * 60 * 60) + (m * 60) + (s))
//    failwith "Not implemented"

let toTime s =
  match s < 0 with
        |true -> (0,0,0)
        |false ->
            let h = s / 3600 
            let m = (s - (h * 3600)) / 60 
            let s = (s - (h * 3600)) - (m * 60) 
            (h,m,s)
   // failwith "Not implemented"

let digits p =
    let rec cnt u k = 
        match u/10 = 0 with
        |true -> k
        |_ -> cnt (u/10) (k + 1) 
    cnt p 1

let minmax (a,b,c,d) =
  let x = min (min a b) (min c d) 
  let y = max (max a b) (max c d) 
  (x,y)
   // failwith "Not implemented"

let isLeap z =
  match z<1582 with  
    |true -> failwith "input year is less than 1582"
    |_ -> match z%100=0 with
            |true ->   z%400 = 0 && z%4 = 0
            |_ -> z%4 = 0
  //  failwith "Not implemented"

let month  = function
    |1 -> "January",31
    |2 -> "February",28
    |3 -> "March",31
    |4 -> "April",30
    |5 -> "May",31
    |6 -> "June",30
    |7 -> "July",31
    |8 -> "August",31
    |9 -> "September",30
    |10 -> "October",31
    |11 -> "November",30
    |12 -> "December",31
    |_ -> failwith "Out Of Bounds"
  //  failwith "Not implemented"

let rec toBinary w =
  match w<0 with
  |true -> failwith "Negative"
  |_ -> match w with
        | 0 | 1 -> string w
        | _ ->
          let bit = string (w % 2)
          (toBinary (w / 2)) + bit
    //failwith "Not implemented"

let bizFuzz x =
  match x > 0 with
  | false -> (0,0,0)
  |_ -> ((x/3),(x/5),((x/3)/5))
    //failwith "Not implemented"

let monthDay dy yr =
      failwith "Not implemented"
let coord _ =
    failwith "Not implemented"
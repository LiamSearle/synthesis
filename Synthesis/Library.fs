module Synthesis

let abelar x =
  (12 < x) && (x<3097) && (x%12 = 0)
    //failwith "Not implemented"

let area b h =
  match b<0.0 || h<0.0 with 
    |true -> failwith "Negative Value"  
    |false -> ((b * 0.5) * h)

  
    //failwith "Not implemented"

let zollo x =
  match x>0 with
    |true -> 2*x
    |false -> abs x
    //failwith "Not implemented"

let min x y =
  match x>y with
    |true -> y
    |false -> x
    //failwith "Not implemented"

let max x y =
  match x<y with
    |true -> y
    |false -> x
    //failwith "Not implemented"

let ofTime h m s =
  ((h * 3600) + (m * 60) + (s))
    //failwith "Not implemented"

let toTime s =
  match s<0 with 
  |true -> (0,0,0)
  |false ->
    let h = s / 3600 //get hours
    let m = (s - (h * 3600)) / 60 //get minutes
    let r2 = (s - (h * 3600)) - (m * 60) //get seconds
    (h,m,r2) //output as tuple
    //failwith "Not implemented"

let digits x =
  let rec count i c =
    match i/10 = 0 with
    |true -> c
    |_ -> count (i/10) (c + 1)
  count x 1
    //failwith "Not implemented"

let minmax (a,b,c,d) =
  let x = min (min a b) (min c d) 
  let y = max (max a b) (min c d) 
  (x,y)
    //failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"
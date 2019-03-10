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
  let y = max (max a b) (max c d) 
  (x,y)
    //failwith "Not implemented"

let isLeap x =
  match x<1582 with  
    |true -> failwith "Year out of bounds"
    |_ -> match x%100=0 with
            |true -> x%4 = 0 && x%400 = 0
            |_ -> x%4 = 0
    
    
    (*match ((x%4 = 0) || ((x%100 = 0) && (x%400 = 0))) with
            |true -> true
            |_ -> false*)

    //failwith "Not implemented"

let month = function
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
    |_ -> failwith "Invalid Month"
    //failwith "Not implemented"

let rec toBinary i =
  match i<0 with
  |true -> failwith "Negative Number"
  |_ -> match i with
        | 0 | 1 -> string i
        | _ ->
          let bit = string (i % 2)
          (toBinary (i / 2)) + bit
  //failwith "Not implemented"

let bizFuzz x = 
  match x > 0 with
  | false -> (0,0,0)
  |_ -> ((x/3),(x/5),((x/3)/5))
    //failwith "Not implemented"

(*.
Create a function ​monthDay​ which accepts an integer ​d​ and a year ​y​, and returns astring for the month that the day ​d​ 
falls within.  The function must accept a range ofd​ from 1 to 365 if ​y​ isn’t a leap year, and must accept ​d​ between 1 and
366 if ​y​ is aleap year.  If ​d​ is out of range, or if ​y​ is less than 1582, then an exception must bethrown.  
Remember that:
 a.April, June, September, and November have 30 days.
 b.January, March, May, July, August, October, and December have 31 days.
 c.February has 29 days in a leap year, and 28 days otherwise.)
*)

let monthDay day year =
  match isLeap(year) with
    |true -> match day<367 with 
      |false -> failwith "day out of range"
      |_-> 
        match day<32 with
          | true -> "January"
        match day<61 && day > 32 with
          | true -> "February"
        match day<92 && day> 60 with
          |true -> "March"
        match day<122 && day> 92 with
          |true -> "April"
        match day<153 && day> 122 with
          |true -> "May"
        match day<183 && day> 153 with
          |true -> "June"
        match day<214 && day> 183 with
          |true -> "July"
        match day<245 && day> 214 with
          |true -> "August"
        match day<275 && day> 245 with
          |true -> "September"
        match day<306 && day> 275 with
          |true -> "October"
        match day<336 && day> 306 with
          |true -> "November"
        match day<367 && day> 336 with
          |true -> "December"
        
        
        
        
        
        
        
        

        //todo 
    |_-> match day<366 with
      |false -> failwith "day out of range"
      |_->let month = function
            | <=31 -> "January"//todo
            | _ -> failwith "idk what happened"
          
            
  //failwith "Not implemented"

let coord _ =
    failwith "Not implemented"
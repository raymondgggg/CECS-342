module ListOps

type Account =
    | Balance of int
    | Overdrawn of int
    | Empty

// For the functions described below, first give a comment that expresses the
// type of the function, then provide an implementation that matches the
// description. (Note: make sure you understand the type of the function if your
// editor provides you with the type automatically). All of the functions below
// take lists. Make sure to note when the list can be generic.
//
// The implementation *must* be recursive.
//
// Descriptions below were provided by Mr. Neal Terrell.

// count x coll
//
// count the number of values equal to x in coll.
let rec count x coll =
    match coll with 
    | [] -> 0
    | head::tail when head = x -> 1 + (count x tail)
    | _ :: tail -> count x tail
    
// countEvens coll
//
// count the number of even integers in coll.
let rec countEvens coll =
    match coll with 
    | [] -> 0
    | head::tail when head % 2 = 0 -> 1 + (countEvens tail)
    | _ :: tail -> countEvens tail

// lastElement coll
//
// return the last element in the list
let rec lastElement coll =
    let listLen = List.length coll
    if listLen = 1 then
        List.head coll
    else
        coll
        |> List.tail
        |> lastElement 

// maxOverdrawn coll
//
// given a list of Accounts, return the largest Overdrawn amount, or 0 if none
// are overdrawn

let maxOverdrawn coll = 
    let rec maxOverdrawnInner coll oAcctList =
        match coll with 
        | [] -> oAcctList
        | head::tail -> 
            match head with 
            | Balance b -> maxOverdrawnInner tail oAcctList
            | Empty -> maxOverdrawnInner tail oAcctList
            | Overdrawn o -> 
                let newOAcctList = List.append oAcctList [o]
                maxOverdrawnInner tail newOAcctList
    let x = maxOverdrawnInner coll [0]
    List.max x
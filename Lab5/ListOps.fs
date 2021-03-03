module ListOps

let suffixes coll =
    let rec suffixes' coll accumulator =
        match coll with
        | [] -> List.append accumulator [[]]
        | head::tail -> suffixes' tail (List.append accumulator [head::tail])

    let newList = suffixes' coll []
    newList
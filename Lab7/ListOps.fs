module ListOps

type Account =
    | Overdrawn of int
    | Balance of int
    | Empty

type Customer = { Name: string; Account: Account }

let withdraw amount acc =
    match acc with
    | Empty -> Overdrawn amount
    | Overdrawn n -> Overdrawn(amount + n)
    | Balance n when n - amount > 0 -> Balance(n - amount)
    | Balance n when n - amount < 0 -> Overdrawn(abs (n - amount))
    | Balance _ -> Empty

let deposit amount acc =
    match acc with
    | Empty -> Balance amount
    | Balance n -> Balance(amount + n)
    | Overdrawn n when n - amount > 0 -> Overdrawn(n - amount)
    | Overdrawn n when n - amount < 0 -> Balance(abs (n - amount))
    | Overdrawn _ -> Empty


let join str strList =
    let str = List.fold (fun acc elem -> acc + elem + str) "" strList
    str.[0..String.length str-2] // return everything up until the second to last element


let simplifyBank customerList nameList = 
    let rec simplifyBank' customerList names simplifiedList =
        if (List.length names = 0) then
            simplifiedList
        else    
            let filteredName = List.filter (fun customer -> customer.Name = List.head names) customerList
            let balanceAccts = 
                filteredName |> List.choose(fun customer ->
                                match customer.Account with
                                | Balance b -> Some(b)
                                | Empty -> None
                                | Overdrawn o -> None)

            let overdrawnAccts = 
                filteredName |> List.choose(fun customer ->
                                match customer.Account with
                                | Overdrawn o -> Some(o)
                                | Empty -> None
                                | Balance b -> None)

            let blanceSum = List.sum balanceAccts
            let overdrawnSum = List.sum overdrawnAccts
            let newAcctAmount = blanceSum - overdrawnSum
            
            if newAcctAmount < 0 then
                simplifyBank' customerList (List.tail names) ({Name = List.head names; Account = Overdrawn (abs newAcctAmount)}::simplifiedList)
            elif newAcctAmount > 0 then
                simplifyBank' customerList (List.tail names) ({Name = List.head names; Account = Balance newAcctAmount}::simplifiedList)
            else
                simplifyBank' customerList (List.tail names) ({Name = List.head names; Account = Empty}::simplifiedList)
    let solution = List.rev (simplifyBank' customerList nameList [])
    solution
  
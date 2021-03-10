module ListOps

type Account =
    | Balance of int
    | Overdrawn of int
    | Empty

type Customer = { Name: string; Account: Account }

let makeCustomerWithBalance name (amount: int) =
    let account =
        if amount > 0 then
            Balance amount
        elif amount < 0 then
            Overdrawn(abs amount)
        else
            Empty


    { Name = name; Account = account }

let unknownCustomer = makeCustomerWithBalance "Unknown" 0

let totalOverdrawn name coll =
    // filter the list to only have the customers of the name we passed in
    let filteredList = 
        coll
        |> List.filter (fun customer -> customer.Name = name)
    // recursively determine the sum of the overdrawn account 
    let rec getOverdrawnSum coll sum =
        match coll with 
        | [] -> sum
        | head::tail -> match head.Account with
                        | Balance b -> getOverdrawnSum tail (sum + 0)
                        | Overdrawn o -> getOverdrawnSum tail (sum + o)
                        | Empty -> getOverdrawnSum tail (sum + 0)
    // get the sum and return it              
    let totalSum = getOverdrawnSum filteredList 0
    totalSum

let maxBalance name coll = 
    // filter the list to only have the customers of the name we passed in
    let filteredList = 
        coll
        |> List.filter (fun customer -> customer.Name = name)

    // recursively iterate through the list to find the account with the highest balance
    let rec maxBalance' coll customer acctVal=
        match coll with
        | [] -> customer
        | head::tail -> match head.Account with
                        | Balance b -> if b > acctVal then
                                           maxBalance' tail head b
                                       else
                                           maxBalance' tail customer acctVal
                        | Overdrawn o -> maxBalance' tail customer acctVal
                        | Empty -> maxBalance' tail customer acctVal

    let customer = maxBalance' filteredList unknownCustomer 0 // last param is the value of the highest account
    customer
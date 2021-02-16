module Bank
type Account =
    | Balance of int
    | Overdrawn of int
    | Empty

type Password = string

type Name = string

type Customer =
    { Name: Name
      Password: Password
      Account: Account }

type Action =
    | Withdraw of int
    | Deposit of int

type Session =
    | Valid of Customer
    | BadPassword

type TransactionResult =
    | AccountUpdated of Customer
    | Failed

let makeAccount () = Empty

let withdraw amount acct = 
    match acct with 
    | Empty -> Overdrawn amount
    | Overdrawn o -> Overdrawn (abs (o-amount))
    | Balance b when b < amount -> Overdrawn (abs (b-amount))
    | Balance b when b > amount -> Balance (b-amount)
    | Balance b -> Empty
    
let deposit amount acct = 
    match acct with 
    | Empty -> Balance amount
    | Overdrawn o when o < amount -> Balance (amount - o)
    | Overdrawn o when o > amount -> Overdrawn (o - amount)
    | Overdrawn o -> Empty
    | Balance b -> Balance (b + amount)
    
let makeCustomer name password =
    {Name = name; Password = password; Account = Empty}

let makeSession password customer =
    if (password <> customer.Password) then 
        BadPassword 
    else 
        Valid customer

let performTransaction action session = 
    match session with
    | BadPassword -> Failed
    | Valid customer -> 
        match action with
        | Deposit d -> AccountUpdated {Name = customer.Name; Password = customer.Password; 
                                        Account = deposit d customer.Account}
        | Withdraw w -> AccountUpdated {Name = customer.Name; Password = customer.Password; 
                                        Account = withdraw w customer.Account}
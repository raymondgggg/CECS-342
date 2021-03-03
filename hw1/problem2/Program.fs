// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System

let isPrime n = 
    let mutable i = float 2
    let mutable primeState = true
    while ((i*i) <= n)  do
        if (n % i =  0.0) then 
            primeState <- false
            i <- n + float 1 // Break out of the loop
        i <- i + float 1
    primeState

let sumPrimes max =
    let mutable sum = float 2
    let mutable i = float 3
    while i <= max do 
        if isPrime i then
            sum <- sum + i
        i <- i + float 2 // all even numbers are not prime so we only check odds
    sum

[<EntryPoint>]
let main argv =
    printfn "The sum of all the primes up till 2 million is: %.2f" (sumPrimes (float (2000000)))
    0 // return`an integer exit code
//Questions:
// Do we need to do exception handeling for getAngle() usr input if not convertable to float?
open System

let placeTarget () = (1000.0) * ((Random()).NextDouble()) 

let getAngle () = 
    printfn "Enter the angle to fire the cannon at (0°-90°):"
    let mutable usrInput = Console.ReadLine()
    let mutable conversion = Double.Parse usrInput 
    
    while conversion < 0.0 || conversion > 90.0 do
        printfn "Enter a valid angle (0°-90°):"
        usrInput <- Console.ReadLine() 
        conversion <- Double.Parse usrInput 
    conversion  

let getGunpowder () =
    printfn "Enter the amount of gunpowder you'd like put in the cannon:"
    let mutable usrInput = Console.ReadLine()
    let mutable conversion = Double.Parse usrInput

    while conversion <= 0.0 do 
        printfn "Enter a valid amount of powder:"
        usrInput <- Console.ReadLine() 
        conversion <- Double.Parse usrInput 
    conversion 

let calculateDistance angle gunPowder = 
    let angleRad = (angle * Math.PI) / 180.0 //convert angle to radians
    let x time = (gunPowder * 30.0 * Math.Cos angleRad) * time // x(t) = Vi*t + xi projectile equation
    let yVelocity = 30.0 * gunPowder * Math.Sin angleRad //
    
    let a = -9.81 * 0.5
    let b = yVelocity
    
    // functions for finding the roots of y(t) = 1/2 * a * t^2 + Vi * t + Yi
    let quadraticRoot1 a b c =
        let root1 = (-1.0 * b + Math.Sqrt (b*b - 4.0*a*c)) / (2.0 * a)
        root1
    let quadraticRoot2 a b c =
        let root2 = (-1.0 * b - Math.Sqrt (b*b - 4.0*a*c)) / (2.0 * a)
        root2
    //get the roots
    let root1 = quadraticRoot1 a b 0.0
    let root2 = quadraticRoot2 a b 0.0
    let mutable time = 0.0
    //figure out which root to use
    if root1 <= 0.0 then 
        time <- root2
    else
        time <- root1
    //return the root
    let projectilePos = x time
    projectilePos

let isHit (location:float) (distance:float) = 
    Math.Abs (location - distance) <= 1.0

[<EntryPoint>]
let main argv =
    let target = placeTarget ()
    let mutable angle = getAngle ()
    let mutable gunPowder = getGunpowder ()
    let mutable distance = calculateDistance angle gunPowder
    printfn "Distance: %f" distance
    let mutable difference = distance - target

    while Math.Abs difference > 1.0 do
        if difference < 0.0 then 
            printfn "You were short %f m" (Math.Abs difference)
        else 
            printfn "You were long %f m" (Math.Abs difference) 
        angle <- getAngle ()
        gunPowder <- getGunpowder ()
        distance <- calculateDistance angle gunPowder
        difference <- distance - target
    
    printfn "Congrats! You were within %f m of the target" (Math.Abs difference)
    0
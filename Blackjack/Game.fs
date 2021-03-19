/// Card representations.
// An "enum"-type union for card suit.
type CardSuit = 
    | Spades 
    | Clubs
    | Diamonds
    | Hearts

// Kinds: 1 = Ace, 2 = Two, ..., 11 = Jack, 12 = Queen, 13 = King.
type Card = {suit : CardSuit; kind : int}


/// Game state records.
// One hand being played by the player: its cards, and a flag for whether it was doubled-down.
type PlayerHand = {
    cards: Card list; 
    doubled: bool
}

// All the hands being played by the player: the hands that are still being played (in the order the player must play them),
// and the hands that have been finished (stand or bust).
type PlayerState = {
    activeHands: PlayerHand list; 
    finishedHands: PlayerHand list
}

// The state of a single game of blackjack. Tracks the current deck, the player's hands, and the dealer's hand.
type GameState = {
    deck : Card list; 
    player : PlayerState; 
    dealer: Card list
}

// A log of results from many games of blackjack.
type GameLog = {playerWins : int; dealerWins : int; draws : int}

/// Miscellaneous enums.
// Identifies whether the player or dealer is making some action.
type HandOwner = 
    | Player 
    | Dealer

// The different actions a player can take.
type PlayerAction = 
    | Hit
    | Stand
    | DoubleDown
    | Split

// The result of one hand that was played.
type HandResult = 
    | Win
    | Lose
    | Draw


// This global value can be used as a source of random integers by writing
// "rand.Next(i)", where i is the upper bound (exclusive) of the random range.
let rand = new System.Random()


// UTILITY METHODS

// Returns a string describing a card.
let cardToString card =
    // TODO: replace the following line with logic that converts the card's kind to a string.
    // Reminder: a 1 means "Ace", 11 means "Jack", 12 means "Queen", 13 means "King".
    // A "match" statement will be necessary. (The next function below is a hint.)
    let kind = string card.kind

    // "%A" can print any kind of object, and automatically converts a union (like CardSuit)
    // into a simple string.
    match kind with 
    | "1" -> sprintf "Ace of %A" card.suit
    | "11" -> sprintf "Jack of %A" card.suit
    | "12" -> sprintf "Queen of %A" card.suit
    | "13" -> sprintf "King of %A" card.suit
    | _ -> sprintf "%s of %A" kind card.suit

// Returns a string describing the cards in a hand.    
let handToString hand =
    // TODO: replace the following line with statement(s) to build a string describing the given hand.
    // The string consists of the results of cardToString when called on each Card in the hand (a Card list),
    // separated by commas. You need to build this string yourself; the built-in "toString" methods for lists
    // insert semicolons and square brackets that I do not want.

    let stringList = List.map (cardToString) hand 
    let handString = String.concat ", " stringList
    sprintf "%A" handString

    // Hint: transform each card in the hand to its cardToString representation. Then read the documentation
    // on String.concat.
    
// Returns the "value" of a card in a poker hand, where all three "face" cards are worth 10
// and an Ace has a value of 11.
let cardValue card =
    match card.kind with
    | 1 -> 11
    | 11 | 12 | 13 -> 10  // This matches 11, 12, or 13.
    | n -> n
    
    // Reminder: the result of the match will be returned

// Calculates the total point value of the given hand (Card list). 
// Find the sum of the card values of each card in the hand. If that sum
// exceeds 21, and the hand has aces, then some of those aces turn from 
// a value of 11 to a value of 1, and a new total is computed.
// TODO: fill in the marked parts of this function.
let handTotal hand =
    // TODO: modify the next line to calculate the sum of the card values of each
    // card in the list. Hint: List.map and List.sum. (Or, if you're slick, List.sumBy)
    let sum = List.sumBy cardValue hand

    // TODO: modify the next line to count the number of aces in the hand.
    // Hint: List.filter and List.length. 
    let numAces = List.length (List.filter (fun card -> card.kind = 1) hand)

    // Adjust the sum if it exceeds 21 and there are aces.
    if sum <= 21 then
        // No adjustment necessary.
        sum
    else 
        // Find the max number of aces to use as 1 point instead of 11.
        let maxAces = (float sum - 21.0) / 10.0 |> ceil |> int
        // Remove 10 points per ace, depending on how many are needed.
        sum - (10 * (min numAces maxAces))


// FUNCTIONS THAT CREATE OR UPDATE GAME STATES

// Creates a new, unshuffled deck of 52 cards.
// A function with no parameters is indicated by () in the parameter list. It is also invoked
// with () as the argument.
let makeDeck () =
    // Make a deck by calling this anonymous function 52 times, each time incrementing
    // the parameter 'i' by 1.
    // The Suit of a card is found by dividing i by 13, so the first 13 cards are Spades.
    // The Kind of a card is the modulo of (i+1) and 13. 
    List.init 52 (fun i -> let s = match i / 13 with
                                   | 0 -> Spades
                                   | 1 -> Clubs
                                   | 2 -> Diamonds
                                   | 3 -> Hearts
                           {suit = s; kind = i % 13 + 1})

// Shuffles a list by converting it to an array, doing an in-place Fisher-Yates 
// shuffle, then converting back to a list.
// Don't worry about this.
let shuffleDeck deck =
    let arr = List.toArray deck

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp
    
    Array.iteri (fun i _ -> swap arr i (rand.Next(i, Array.length arr))) arr
    Array.toList arr

// Creates a new game state using the given deck, dealing 2 cards to the player and dealer.
let newGame (deck : Card list) =
    // Construct the starting hands for player and dealer.
    let playerCards = [deck.Head ; List.item 2 deck] // First and third cards.
    let dealerCards = [deck.Tail.Head ; List.item 3 deck] // Second and fourth.

    // Return a fresh game state.
    {deck = List.skip 4 deck;
    // the initial player has only one active hand.
     player = {activeHands = [{cards = playerCards; doubled = false}]; finishedHands = []}
     dealer = dealerCards}

// Given a current game state and an indication of which player is "hitting", deal one
// card from the deck and add it to the given person's hand. Return the new game state.
let hit handOwner gameState = 
    let topCard = List.head gameState.deck
    let newDeck = List.tail gameState.deck
    
    // Updating the dealer's hand is easy.
    if handOwner = Dealer then
        let newDealerHand = topCard :: gameState.dealer
        // Return a new game state with the updated deck and dealer hand.
        {gameState with deck = newDeck;
                        dealer = newDealerHand}
    else
        // TODO: updating the player is trickier. We are always working with the player's first
        // active hand. Create a new first hand by adding the top card to that hand's card list.
        // Then update the player's active hands so that the new first hand is head of the list; and the
        //     other (unchanged) active hands follow it.
        // Then construct the new game state with the updated deck and updated player.
        let currentHand = gameState.player.activeHands 
                          |> List.head 
        let currentPlayerState = gameState.player
        let remainingHands = gameState.player.activeHands
                             |> List.tail 
        let newCardList = topCard :: currentHand.cards
        let newHand = {currentHand with cards = newCardList}
        let newPlayerState = {currentPlayerState with activeHands = newHand :: remainingHands}
        {gameState with deck = newDeck; player = newPlayerState}
        
// Take the dealer's turn by repeatedly taking a single action, hit or stay, until 
// the dealer busts or stays.
let rec dealerTurn gameState =
    let dealer = gameState.dealer
    let score = handTotal dealer

    printfn "Dealer's hand: %s; %d points" (handToString dealer) score
    
    // Dealer rules: must hit if score < 17.
    if score > 21 then
        printfn "Dealer busts!"
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        gameState
    elif score < 17 then
        printfn "Dealer hits"
        // The game state is changed; the result of "hit" is used to build the new state.
        // The dealer gets to take another action using the new state.
        gameState
        |> hit Dealer
        |> dealerTurn
    else
        // The game state is unchanged because we did not hit. 
        // The dealer does not get to take another action.
        printfn "Dealer must stay"
        gameState

// My own functs
let moveActiveHand gameState = 
    let playerState = gameState.player
    let topActive = playerState.activeHands |> List.head
    let remainingHands = playerState.activeHands |> List.tail
    let newPlayerState = {playerState with activeHands = remainingHands;
                                            finishedHands = topActive :: playerState.finishedHands}
    {gameState with player = newPlayerState}

let rec moveAllActiveHands gamestate = 
    if List.length gamestate.player.activeHands = 0 then
        gamestate
    else 
        moveAllActiveHands (moveActiveHand gamestate) 

let classify playerHand dealerHand =
    let playerScore = handTotal playerHand.cards
    let dealerScore = handTotal dealerHand

    match playerScore with
    | playerScore when playerScore = dealerScore -> Draw
    | playerScore when playerScore > 21 -> Lose
    | playerScore when playerScore < dealerScore -> Lose
    | _ -> Win
// My own functs
    
// Take the player's turn by repeatedly taking a single action until they bust or stay.
let rec playerTurn (playerStrategy : GameState->PlayerAction) (gameState : GameState) =
    // TODO: code this method using dealerTurn as a guide. Follow the same standard
    // of printing output. This function must return the new game state after the player's
    // turn has finished, like dealerTurn.

    // Unlike the dealer, the player gets to make choices about whether they will hit or stay.
    // The "elif score < 17" code from dealerTurn is inappropriate; in its place, we will
    // allow a "strategy" to decide whether to hit. A "strategy" is a function that accepts
    // the current game state and returns true if the player should hit, and false otherwise.
    // playerTurn must call that function (the parameter playerStrategy) to decide whether
    // to hit or stay.
    let playerState = gameState.player

    if playerState.activeHands.IsEmpty then
        // A player with no active hands cannot take an action.
        gameState
    else
        // The next line is just so the code compiles. Remove it when you code the function.
        // TODO: print the player's first active hand. Call the strategy to get a PlayerAction.
        // Create a new game state based on that action. Recurse if the player can take another action 
        // after their chosen one, or return the game state if they cannot.
        printfn $"First active hand: {handToString (List.head playerState.activeHands).cards}"
        let newPlayerAction = playerStrategy gameState
        //fix the remainder of this match expression when I implement the different strategies!!!!

        // Still todo: make sure that you return a gameState that corresponds to the action taken
        // Meaning that: the active hand is moved to finished hand, the bool flag for double 
        // is set if needed, the player's hand has a new card when they hit.
        // split the cards and have multiple active hands (program that game logic)
        // Use helper methods where I pass in a gameState and the gameState gets modified and 
        // returned according to the action that I matched with.
        let chooseSplit gameState = 
            let splitVal = List.distinct (List.head gameState.player.activeHands).cards
            let newHands = {List.head gameState.player.activeHands with cards = splitVal}
            let newPlayerState = {playerState with 
                                                activeHands = newHands :: newHands :: List.tail gameState.player.activeHands}
            {gameState with player = newPlayerState}

        let chooseDoubleDown gameState = 
            let newGameState = hit Player gameState
            let handUpdate = {List.head newGameState.player.activeHands with doubled = true}
            let remainingHands = List.tail newGameState.player.activeHands
            let updatePState = {newGameState.player with activeHands = handUpdate :: remainingHands}
            moveActiveHand {newGameState with player = updatePState}

        match newPlayerAction with
        | Hit -> moveActiveHand (hit Player gameState)
        | Stand -> moveActiveHand gameState
        | Split -> chooseSplit gameState
        | DoubleDown -> chooseDoubleDown gameState

// Plays one game with the given player strategy. Returns a GameLog recording the winner of the game.
let oneGame playerStrategy gameState =
    // TODO: print the first card in the dealer's hand to the screen, because the Player can see
    // one card from the dealer's hand in order to make their decisions.
    printfn "Dealer is showing: %A"  (cardToString (List.head gameState.dealer)) // fix this line

    if handTotal gameState.dealer = 21 then
        if handTotal (gameState.player.activeHands |> List.head).cards = 21 then
            {playerWins = 0; dealerWins = 0; draws = 1}
        else
            {playerWins = 0; dealerWins = 1; draws = 0}
    
    else
        printfn "Player's turn"
        // TODO: play the game! First the player gets their turn. The dealer then takes their turn,
        // using the state of the game after the player's turn finished.
        let afterPlayerState = playerTurn playerStrategy gameState

        printfn "\nDealer's turn"
        let afterDealerState = dealerTurn afterPlayerState

        // TODO: determine the winner(s)! For each of the player's hands, determine if that hand is a 
        // win, loss, or draw. Accumulate (!!) the sum total of wins, losses, and draws, accounting for doubled-down
        // hands, which gets 2 wins, 2 losses, or 1 draw

        // The player wins a hand if they did not bust (score <= 21) AND EITHER:
        // - the dealer busts; or
        // - player's score > dealer's score
        // If neither side busts and they have the same score, the result is a draw.
       
        let rec getGameLog handsToCheck dealerHand gameLog = 
            let currentPWins = gameLog.playerWins
            let currentDWins = gameLog.dealerWins
            let currentDraws = gameLog.draws
            if List.length handsToCheck = 0 then
                gameLog
            else
                let handResult = classify (List.head handsToCheck) dealerHand
                if handResult = Win then
                    getGameLog (List.tail handsToCheck) dealerHand {gameLog with playerWins = currentPWins + 1}

                elif handResult = Win && (List.head handsToCheck).doubled = true then
                    getGameLog (List.tail handsToCheck) dealerHand {gameLog with playerWins = currentPWins + 2}

                elif handResult = Draw then
                    getGameLog (List.tail handsToCheck) dealerHand {gameLog with draws = currentDraws + 1}

                elif handResult = Lose && (List.head handsToCheck).doubled = true then
                    getGameLog (List.tail handsToCheck) dealerHand {gameLog with dealerWins = currentDWins + 2}
                else
                    getGameLog (List.tail handsToCheck) dealerHand {gameLog with dealerWins = currentDWins + 1}
        // TODO: this is a "blank" GameLog. Return something more appropriate for each of the outcomes
        // described above.
        let handsToCheck' = (moveAllActiveHands afterDealerState).player.finishedHands
        getGameLog handsToCheck' afterDealerState.dealer {playerWins = 0; dealerWins = 0; draws = 0}

// Plays n games using the given playerStrategy, and returns the combined game log.
let manyGames n playerStrategy =
    // TODO: run oneGame with the playerStrategy n times, and accumulate the result. 
    // If you're slick, you won't do any recursion yourself. Instead read about List.init, 
    // and then consider List.reduce.
    let gameResults = List.init n (fun n -> oneGame playerStrategy (newGame (makeDeck () |> shuffleDeck )))
    let combineGameLogs logs = 
        let combineLogs elem1 elem2 = 
            let currentPlayerWins = elem1.playerWins
            let currentDealerWins = elem1.dealerWins
            let currentDraws = elem1.draws
            match elem2 with
            | {playerWins = p; dealerWins = 0; draws = 0} -> {elem1 with playerWins = currentPlayerWins + p}
            | {playerWins = 0; dealerWins = 0; draws = dr} -> {elem1 with draws = currentDraws + dr}
            | {dealerWins = d} -> {elem1 with dealerWins = currentDealerWins + d}

        List.reduce combineLogs logs

    combineGameLogs gameResults
    // List.reduce (+) gameResults
      
    // TODO: this is a "blank" GameLog. Return something more appropriate.
    // {playerWins = 0; dealerWins = 0; draws = 0}
  
// PLAYER STRATEGIES
// Returns a list of legal player actions given their current hand.
let legalPlayerActions playerHand =
    let legalActions = [Hit; Stand; DoubleDown; Split]
    // One boolean entry for each action; True if the corresponding action can be taken at this time.
    let requirements = [
        handTotal playerHand < 21; 
        true; 
        playerHand.Length = 2;
        playerHand.Length = 2 && cardValue playerHand.Head = cardValue playerHand.Tail.Head
    ]

    List.zip legalActions requirements // zip the actions with the boolean results of whether they're legal
    |> List.filter (fun (_, req) -> req) // if req is true, the action can be taken
    |> List.map (fun (act, _) -> act) // return the actions whose req was true

// Get a nice printable string to describe an action.
let actionToString = function
    | Hit -> "(H)it"
    | Stand -> "(S)tand"
    | DoubleDown -> "(D)ouble down"
    | Split -> "S(p)lit"

// This strategy shows a list of actions to the user and then reads their choice from the keyboard.
let rec interactivePlayerStrategy gameState =
    let playerHand = gameState.player.activeHands.Head
    let legalActions = legalPlayerActions playerHand.cards

    legalActions
    |> List.map actionToString
    |> String.concat ", "
    |> printfn "What do you want to do? %s" 

    let answer = System.Console.ReadLine()
    // Return true if they entered "y", false otherwise.
    match answer.ToLower() with
    | "h" when List.contains Hit legalActions -> Hit
    | "s" -> Stand
    | "d" when List.contains DoubleDown legalActions -> DoubleDown
    | "p" when List.contains Split legalActions -> Split
    | _ -> printfn "Please choose one of the available options, dummy."
           interactivePlayerStrategy gameState

let inactivePlayerStrategy (gamestate:GameState) =
    Stand

let greedyPlayerStrategy (gameState:GameState) = 
    if handTotal ((List.head gameState.player.activeHands).cards) >= 21 then
        Stand
    else
        Hit

let coinFlipPlayerStrategy (gameState:GameState)  = 
    let choice = rand.Next(2)
    match choice with
    | 0 -> Hit //heads
    | _ -> Stand //tails (value of 1)

let basicPlayerStrategy gameState =
    let activeHand = List.head gameState.player.activeHands
    let card1 = List.head (activeHand.cards)
    let card2 = List.head (List.tail activeHand.cards)
    let handSum = handTotal activeHand.cards
    let dealerTopCard = cardValue (List.head gameState.dealer)

    if (handSum = 10 && dealerTopCard < 10) || handSum = 11 || (handSum = 9 && dealerTopCard <> 2) || (handSum = 9 && dealerTopCard < 7) then
        DoubleDown
    elif (handSum = 10 && dealerTopCard >= 10) || (handSum = 9 && dealerTopCard = 2) || (handSum = 9 && dealerTopCard >= 7) then
        Hit
    elif cardValue card1 = cardValue card2 && handSum < 20 then
        Split
    elif (cardValue card1 = cardValue card2 && handSum >= 20) then
        Stand
    else
        let dealerRange1 = [2..6]
        let dealerRange2 = [7..10]

        if handSum > 12 && List.contains dealerTopCard dealerRange1 then
            Stand
        elif handSum < 12 && List.contains dealerTopCard dealerRange1 then
            Hit
        elif handSum <= 16 && List.contains dealerTopCard dealerRange2 then
            Hit
        elif handSum >= 16 && List.contains dealerTopCard dealerRange2 then
            Stand
        else
            let rec hasAce playerHand = 
                match playerHand.cards with
                | [] -> false
                | head::tail when cardValue head = 11 -> true
                | _ -> hasAce ({playerHand with cards = List.tail playerHand.cards})

            if handSum <= 16 && hasAce activeHand = true then
                Hit
            elif handSum <= 11 then
                Hit
            else
                Stand

open Blackjack

[<EntryPoint>]
let main argv =
    // Blackjack.makeDeck () 
    // |> Blackjack.shuffleDeck
    // |> Blackjack.newGame
    // |> Blackjack.oneGame Blackjack.recklessPlayer
    // |> printfn "%A"

    // let deck1 = shuffleDeck (makeDeck ())
    
    
    // let cards1 = [{suit = Diamonds; kind = 5}; {suit = Spades; kind = 5}; {suit = Hearts; kind = 3};]
    // let cards2 = [{suit = Diamonds; kind = 6}; {suit = Spades; kind = 3}; {suit = Hearts; kind = 5};]

    // let pHand1 = {cards = cards1; doubled = false}
    // let phand2 = {cards = cards2; doubled = false}
    // let activeHands1 = [phand2; pHand1]
    // let pState = {activeHands = activeHands1; finishedHands = []}
    // let dealer = [{suit = Spades; kind = 4}; {suit = Spades; kind = 5}; {suit = Clubs; kind = 8};]

    // let gameState = {deck = deck1; player = pState; dealer = dealer}

    // let nGames = manyGames 1000 inactivePlayerStrategy
    // let nGames = manyGames 1000 greedyPlayerStrategy
    // let nGames = manyGames 1000 coinFlipPlayerStrategy
    let nGames = manyGames 1000 basicPlayerStrategy

    printfn $"{nGames}"
    

    // TODO: call manyGames to run 1000 games with a particular strategy.

    0 // return an integer exit code
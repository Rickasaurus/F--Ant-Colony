//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereApp.GameControl

open System
open System.Text
open System.IO
open System.Windows
open System.Windows.Controls
open System.Text.RegularExpressions

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes
open AntsEverywhereLib.World

let startGame onExit setContent startApp onWinner antAIs numCycles logIt = 

    // F# Disposable Pattern
    let disposables = ref []
    let remember (disposable : IDisposable) = disposables := disposable :: !disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = !disposables |> List.iter dispose; disposables := []

    // Load Controls / Windows
    let mainControl = new MainScreen()
    do  remember mainControl
    let simulationControl = new SimulationControl()
    do  remember simulationControl
    let defaultAI = AI.TestAntBehavior() :> AntsEverywhereLib.UserTypes.IAntBehavior
    let selectionControl = new AISelectionControl(defaultAI)
    do  remember selectionControl
    
    do setContent mainControl

    // Register Events for Interop
    let loadAISelectionScreenHandler () = mainControl.SwitchControls selectionControl
    do simulationControl.ClickedEvent.Subscribe loadAISelectionScreenHandler |> ignore    
    
    let simulationEndedHandler result = simulationControl.StartSimulation result.RedAI result.BlackAI numCycles
    do simulationControl.SimulationEndedEvent.Subscribe simulationEndedHandler |> ignore

    let aiSelectedHandler (redAI, blackAI, timed) =
        simulationControl.StartSimulation redAI blackAI timed
        mainControl.SwitchControls simulationControl
    do selectionControl.AISelectedEvent.Subscribe aiSelectedHandler |> ignore 
    do selectionControl.CancelEvent.Subscribe (fun _ -> mainControl.SwitchControls simulationControl) |> ignore
    do selectionControl.addLoadedAIs antAIs

    let contestmodeHandler (ants, cycles) = 
        do logIt ("--- Starting Contest ---")
        do ants |> Array.iter (fun (ant: IAntBehavior) -> logIt (ant.Name))

        let ants = ants |> Array.toList
        let generatePairs ants = 
            // Randomize Ants
            let rndAIs = let rnd = System.Random() in ants |> List.sortBy (fun _ -> rnd.Next())
            let len = List.length rndAIs

            let pairs = 
                [
                    for i in 1 .. 2 .. len - 1 do
                        let red = rndAIs.[i - 1] 
                        let black = rndAIs.[i]
                        yield red, black
                ]
            let rem = if len % 2 = 1 then [rndAIs.[len - 1]] else []
            pairs, rem       
        async {
            let nextRoundAnts = ref ants
            while List.length !nextRoundAnts > 1 do
                logIt("-- New Round --")

                let trf, leftoverAnt = generatePairs (!nextRoundAnts)
                let thisRoundPairs = ref trf
                nextRoundAnts := leftoverAnt
                
                let contestRows, contestColumns =
                    match List.length !thisRoundPairs with
                    | 1 -> 1,1
                    | 2 -> 1,2
                    | _ -> 2,2

                let grid = new Grid(HorizontalAlignment = HorizontalAlignment.Center, VerticalAlignment = VerticalAlignment.Top, ShowGridLines = true)

                // Add Rows and Columns
                do List.init contestRows (fun _ -> new RowDefinition()) |> List.iter grid.RowDefinitions.Add
                do List.init contestColumns (fun _ -> new ColumnDefinition()) |> List.iter grid.ColumnDefinitions.Add

                // Init Simulation Controls
                let scs = List.init contestRows (fun _ -> List.init contestColumns (fun _ -> new SimulationControl()))
                let init r c sc = Grid.SetRow (sc, r); Grid.SetColumn (sc, c); grid.Children.SafeAdd (sc) |> ignore
                scs |> List.iteri (fun r l -> l |> List.iteri (fun c sc -> init r c sc))
                let flatScs = List.collect id scs
                mainControl.SwitchControls grid

                // Count running events
                let sims = ref 0
                let rec mergeEvents (lst: SimulationControl list) event = 
                    match lst with
                    | [] -> event
                    | h :: rest -> 
                        match event with
                        | Some event -> mergeEvents rest (Some <| Event.merge h.SimulationEndedEvent event)
                        | None -> mergeEvents rest (Some <| h.SimulationEndedEvent)
                
                // Keep count and keep winners updated
                let merged = mergeEvents flatScs None 
                merged 
                |> Option.iter (fun ev -> 
                    ev |> Event.add (fun args -> nextRoundAnts := args.Winner :: !nextRoundAnts
                                                 logIt (sprintf "%s vs. %s -> %s" args.RedAI.Name args.BlackAI.Name args.Winner.Name)
                                                 sims := !sims - 1))

                // Start initial simulations
                for sc in flatScs do
                    match !thisRoundPairs with
                    | (r,b) :: rest -> 
                        thisRoundPairs := rest
                        sims := !sims + 1
                        sc.StartSimulation r b numCycles
                    | [] -> ()

                // Keep it going
                match merged with 
                | None -> ()
                | Some (me) -> 
                    for (r,b) in !thisRoundPairs do
                        let! args = Async.AwaitEvent(me)
                        args.Control.StartSimulation r b numCycles

                while !sims <> 0 do
                    do! Async.Sleep(50)           

            let winner = List.head !nextRoundAnts
            let wintext = "The winner of the contest is: " + winner.Name
            do logIt (wintext) |> ignore
            do onWinner(wintext)

        } |> Async.StartImmediate
        
    do selectionControl.ContestStartedEvent.Subscribe contestmodeHandler |> ignore

    // Switch to the Simulation
    mainControl.SwitchControls(simulationControl)   

    // Grab Ant Files if Necessary and Start
    match antAIs |> List.ofSeq with
    | b::r::rest -> simulationControl.StartSimulation b r numCycles
    | b::[] -> simulationControl.StartSimulation b defaultAI numCycles
    | [] -> simulationControl.StartSimulation defaultAI defaultAI numCycles
    startApp()

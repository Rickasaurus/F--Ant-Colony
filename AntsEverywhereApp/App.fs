//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

namespace AntsEverywhereApp

open System
open System.Text
open System.IO
open System.Windows
open System.Windows.Controls
open System.Text.RegularExpressions

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes
open AntsEverywhereLib.World

#if SILVERLIGHT
type App() as app =
    inherit Application()

    let gameController = new GameController()
    
    do  app.Startup.Add(fun e ->
        gameController.SetInitParams(e.InitParams)
        app.RootVisual <- gameController)
    do  app.Exit.Add(fun _ -> (gameController :> System.IDisposable).Dispose())
#else
module StartUp =
    // Parameters
    let logfile = "antwars.log"
    let logsb = new StringBuilder()

    let numCycles = 1500

    // F# Disposable Pattern
    let mutable disposables = []
    let remember (disposable : IDisposable) = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    // Set up App
    let app = new Application()
    do  app.Exit.Add(fun _ -> (forget()))

    // Load Controls / Windows
    let mainControl = new MainScreen()
    do  remember mainControl
    let simulationControl = new SimulationControl()
    do  remember simulationControl
    let defaultAI = AI.TestAntBehavior() :> AntsEverywhereLib.UserTypes.IAntBehavior
    let selectionControl = new AISelectionControl(defaultAI)
    do  remember selectionControl
    let win = new Window(Content = mainControl)

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

    let contestmodeHandler (ants, cycles) = 
        do logsb.AppendLine("--- Starting Contest ---") |> ignore
        do ants |> Array.iter (fun (ant: IAntBehavior) -> logsb.AppendLine(ant.Name) |> ignore)

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
                logsb.AppendLine("-- New Round --") |> ignore

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
                                                 logsb.AppendLine (sprintf "%s vs. %s -> %s" args.RedAI.Name args.BlackAI.Name args.Winner.Name) |> ignore
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
            do logsb.AppendLine(wintext) |> ignore
            do File.WriteAllText(logfile, logsb.ToString())
            do MessageBox.Show(wintext, "Contest Winner!", MessageBoxButton.OK, MessageBoxImage.Information) |> ignore

        } |> Async.StartImmediate
        

    do selectionControl.ContestStartedEvent.Subscribe contestmodeHandler |> ignore

    // Switch to the Simulation
    mainControl.SwitchControls(simulationControl)   

    let tryParse arg =
      let reOPts = RegexOptions.Compiled ||| RegexOptions.IgnoreCase ||| RegexOptions.ExplicitCapture
      let m = Regex.Match(arg,@"^(?<k>[a-z][a-z0-9_-]*),(?<v>.+)$",reOPts)
      if not m.Success
        then  None
        else  Some(m.Groups.["k"].Value,m.Groups.["v"].Value)

    [<STAThread;EntryPoint>]
    let main args =
        let antfiles = args |> Array.choose (tryParse) |> Array.map (fun (k,v) -> v)
        let antAI = selectionControl.addItemsFromDisk antfiles
        match antAI |> List.ofSeq with
        | b::r::rest -> simulationControl.StartSimulation b r numCycles
        | b::[] -> simulationControl.StartSimulation b defaultAI numCycles
        | [] -> simulationControl.StartSimulation defaultAI defaultAI numCycles
        app.Run win
#endif

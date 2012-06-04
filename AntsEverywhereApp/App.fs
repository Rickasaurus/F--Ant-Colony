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
open System.Windows
open System.Text.RegularExpressions

//        let generatePairs ants = 
//            // Randomize Ants
//            let rndAIs = let rnd = System.Random() in ants |> Array.sortBy (fun _ -> rnd.Next())
//            let len = Array.length rndAIs
//
//            let pairs = 
//                [
//                    for i in 1 .. 2 .. len - 1 do
//                        let red = rndAIs.[i - 1] 
//                        let black = rndAIs.[i]
//                        yield red, black
//                ]
//            let rem = if len % 2 = 1 then [rndAIs.[len - 1]] else []
//            pairs, rem
//
//        let timed =  Int32.Parse timeTextBox.Text
//        let contestIsGoing = ref true
//        let remainingAnts = ref loadedAI
//        while !contestIsGoing do
//            let randomPairs, remainder = generatePairs !remainingAnts
//            for redAI, blackAI in randomPairs do
//                aiSelectedEvent.Trigger(blackAI, redAI, timed)

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
    let numCycles = 2000

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

//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

namespace AntsEverywhereApp

open System.Windows

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
    open System
    open System.Text.RegularExpressions

    let mutable disposables = []
    let remember (disposable : IDisposable) = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let app = new Application()
    do  app.Exit.Add(fun _ -> (forget()))

    let mainControl = new MainScreen()
    do  remember mainControl
    let simulationControl = new SimulationControl()
    do  remember simulationControl
    let defaultAI = AI.TestAntBehavior() :> AntsEverywhereLib.UserTypes.IAntBehavior
    let selectionControl = new AISelectionControl(defaultAI)
    do  remember selectionControl

    let win = new Window(Content = mainControl)
    do selectionControl.LoadedAIEvent.Subscribe (fun (redAI, blackAI, timed) -> 
        simulationControl.StartSimulation redAI blackAI timed
        mainControl.SwitchControls simulationControl) |> ignore
    do simulationControl.LoadAIEvent.Subscribe (fun _ -> mainControl.SwitchControls selectionControl) |> ignore    

    mainControl.SwitchControls(simulationControl)   

    let tryParse arg =
      let reOPts = RegexOptions.Compiled ||| RegexOptions.IgnoreCase ||| RegexOptions.ExplicitCapture
      let m = Regex.Match(arg,@"^(?<k>[a-z][a-z0-9_-]*),(?<v>.+)$",reOPts)
      if not m.Success
        then  None
        else  Some(m.Groups.["k"].Value,m.Groups.["v"].Value)

    let numCycles = 1500

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

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
open System.Collections.Generic
open System.Reflection
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Threading

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes

type GameController () as this =
    inherit UserControl ()

    //
    // Phillip Trelford's great pattern for IDisposables
    //

    let mutable disposables = []
    let remember (disposable : IDisposable) = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let outerLayout = new StackPanel()
    let layout = Grid()
    do this.Content <- layout
    
    let defaultAI = AI.TestAntBehavior() :> AntsEverywhereLib.UserTypes.IAntBehavior

    let aiSelectionControl = new AISelectionControl(defaultAI)
    do aiSelectionControl |> remember

    let simulationControl = new SimulationControl()
    do simulationControl |> remember

    let switchControls newControl =
        layout.Children.Clear()
        layout.Children.SafeAdd newControl

    let startWithAI (blackAI : IAntBehavior) (redAI : IAntBehavior) maxCycles: unit =
        try 
            switchControls (simulationControl)
            simulationControl.StartSimulation blackAI redAI maxCycles
            
        with e -> layout.Children.SafeAdd( new TextBox(Text = sprintf "%O" e) )

#if SILVERLIGHT
   //HACK: the following function only works on WPF
#else      
    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    //HACK: this duplicates code found else where... not very D.R.Y. [PB]
    let loadAIFromStream (stream : System.IO.Stream) =
        try
            // here's to hoping the the file isn't bigger that Int32.MAX
            let buffer: byte[] = Array.zeroCreate (int stream.Length)
            stream.Read(buffer, 0, (int stream.Length)) |> ignore
            let asm = Assembly.Load buffer
            let ai = asm.GetTypes()
                     |> Seq.filter (fun t -> t.IsClass && not t.IsInterface && not t.IsAbstract && not t.IsGenericTypeDefinition)
                     |> Seq.filter (fun t -> typeof<IAntBehavior>.IsAssignableFrom(t))
                     |> Seq.map (fun t -> Activator.CreateInstance(t) :?> IAntBehavior)
                     |> List.ofSeq
                     |> List.nth <| 0
            Some ai
        with e -> None
#endif

    do aiSelectionControl.LoadedAIEvent.Subscribe (fun (redAI, blackAI, timed) -> startWithAI redAI blackAI timed) |> ignore
       simulationControl.LoadAIEvent.Subscribe (fun _ -> switchControls aiSelectionControl) |> ignore

    //do startWithAI defaultAI defaultAI maxWorldCycles

    member x.SetInitParams args = 
        aiSelectionControl.addItems args
 
#if SILVERLIGHT
     //HACK: the following function only works on WPF
#else   
    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    member x.AddItemsFromDisk args =
      aiSelectionControl.addItemsFromDisk args
#endif

    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    member x.StartWithAI blackAIName redAIName =
      let blackAI = (!aiSelectionControl.AIMap).TryGet blackAIName
      let redAI = (!aiSelectionControl.AIMap).TryGet redAIName
      match blackAI, redAI with
      | Some (AIResolved (blackAI)), Some (AIResolved (redAI)) -> 
          startWithAI blackAI redAI maxWorldCycles
      | _,_ ->  
          MessageBox.Show("Unable to load given AI. Using defaults instead.") |> ignore
          startWithAI defaultAI defaultAI maxWorldCycles

    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    member x.StartWithDefaults () = 
      x.StartWithAI defaultAI.Name defaultAI.Name

    interface System.IDisposable with
        member this.Dispose() = forget()

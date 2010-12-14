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
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Threading
open System.Reflection
open System.Net

open AntsEverywhereLib.Types

[<AutoOpen>]
module FsExtensions =
    type IDictionary<'Key,'Value> with
        /// Attempts to get the value associated with the specified key.
        member this.TryGet key =
            let ok, v = this.TryGetValue key
            if ok then Some v else None
        member this.toSeq () =
            seq { for keyVal in this do
                    yield keyVal.Key, keyVal.Value } 

type AIType =
        | AIAssemblyRefernece of String
        | AIResolved of IAntBehavior

type AISelectionControl (defaultAI : IAntBehavior) as this =
    inherit UserControl ()

    //
    // Phillip Trelford's great pattern for IDisposables
    //

    let mutable disposables = []
    let remember disposable = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    //
    // This AI map holds both loaded and unloaded AI
    //
    
    let aiMap = ref (Map.ofSeq <| seq { yield defaultAI.Name, AIResolved(defaultAI) })

    //
    // Event for when we are done picking AI
    //

    let loadedAIEvent = new Event<IAntBehavior * IAntBehavior>()

    //
    // Yay, pretty graphics
    //

    let globalStackPanel = StackPanel(Height = this.Height, Margin = Thickness(5.0), Orientation = Orientation.Vertical)
    
    let headerText = TextBlock(Text = "Select Your AI", HorizontalAlignment = HorizontalAlignment.Center)
    do globalStackPanel.Children.Add headerText

    let listStackPanel = StackPanel(Height = this.Height * 0.8, Margin = Thickness(5.0), Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Center)
    let listWidth, listHeight = listStackPanel.Width * 0.4, listStackPanel.Height * 1.0
    let leftList = ListBox(SelectedItem = 0, SelectionMode = SelectionMode.Single)
    let rightList = ListBox(SelectedItem = 0, SelectionMode = SelectionMode.Single)

    do listStackPanel.Children.Add leftList
       listStackPanel.Children.Add rightList
       globalStackPanel.Children.Add listStackPanel

    let buttonStackPanel = StackPanel(Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Center)
    let loadButton = Button(Content="Load AI")
    let startButton = Button(Content="Let's Go!")

    do  buttonStackPanel.Children.Add loadButton
        buttonStackPanel.Children.Add startButton
        globalStackPanel.Children.Add buttonStackPanel

    //
    // Let's refresh those lists with our latest AI findings
    //

    let refresh () =
        !aiMap |> Map.iter (fun key value ->  
                               leftList.Items.Clear()
                               rightList.Items.Clear()
                               leftList.Items.Add( ListBoxItem(Content = key) )    
                               rightList.Items.Add( ListBoxItem(Content = key) ))
        |> ignore

    //
    // Reflection based AI Loading
    //

    let addResolvedAIToMap (ai : IAntBehavior) = 
        aiMap := Map.add ai.Name (AIResolved(ai)) !aiMap

    let loadAIFromStream (stream : System.IO.Stream) =
        try
            let asmPart = AssemblyPart()
            let asm = asmPart.Load stream
            let ai = asm.GetTypes()
                     |> Seq.filter (fun t -> t.IsClass && not t.IsInterface && not t.IsAbstract && not t.IsGenericTypeDefinition)
                     |> Seq.filter (fun t -> typeof<IAntBehavior>.IsAssignableFrom(t))
                     |> Seq.map (fun t -> Activator.CreateInstance(t) :?> IAntBehavior)
                     |> List.ofSeq
                     |> List.nth <| 0
            Some ai
        with e -> headerText.Text <- e.Message
                  None
       
    let loadAIFromWeb url : Option<IAntBehavior>= 
        try
            let client = WebClient()
            let asyncDownload : Async<Option<IAntBehavior>> =                      
                    async {
                        let asyncArgs = Async.AwaitEvent( client.OpenReadCompleted )
                        client.OpenReadAsync url
                        let! args = asyncArgs
                        use resultStream = args.Result
                        return loadAIFromStream resultStream
                    }
            asyncDownload |> Async.RunSynchronously
        with e -> headerText.Text <- e.Message
                  None

    let loadAIFromDisk () = 
        try
            let ofd = OpenFileDialog(Filter = "Assemblies (*.dll;*.exe)|*.dll;*.exe|All Files(*.*)|*.*", FilterIndex = 1, Multiselect = false)

            let clicked = ofd.ShowDialog()
            if clicked.HasValue && clicked.Value then 
                use stream = ofd.File.OpenRead()
                loadAIFromStream stream
            else None        
        with 
        | e -> headerText.Text <- e.Message
               None

    //
    // Configure Button Actions
    //

    let loadButtonClicked () = 
        match loadAIFromDisk() with
        | Some(ai) -> addResolvedAIToMap ai
                      refresh()
        | None -> ()
 
    let resolveToAI selectedItem = 
        match Map.find selectedItem !aiMap with
        | AIResolved behavior -> Some behavior
        | AIAssemblyRefernece reference -> loadAIFromWeb( Uri(reference) )

    let startButtonClicked () = 
        let listBoxItemToString (obj: obj) = 
            let lba = obj :?> ListBoxItem
            lba.Content :?> string
        let blackAI = resolveToAI <| listBoxItemToString leftList.SelectedValue
        let redAI = resolveToAI <| listBoxItemToString rightList.SelectedValue
        match blackAI, redAI with
        | None, None -> headerText.Text <- "Neither AI Could Be Loaded!"
        | None, Some redAI -> headerText.Text <- "Black AI Couldn't Be Loaded!"
        | Some blackAI, None -> headerText.Text <- "Red AI Couldn't Be Loaded!"
        | Some blackAI, Some redAI -> loadedAIEvent.Trigger(blackAI, redAI)

    do loadButton.Click.Subscribe (fun _ -> try loadButtonClicked() with e -> headerText.Text <- e.Message) |> remember
       startButton.Click.Subscribe (fun _ -> try startButtonClicked() with e -> headerText.Text <- e.Message) |> remember

    //
    // Start things up
    //
    
    do refresh()
       this.Content <- globalStackPanel
       
    //
    // This is mainly used to add items from the Silverlight InitParams
    //

    member x.addItems (aiDict : IDictionary<string,string>) =
        if aiDict <> null then
            let newAiSeq = aiDict.toSeq() |> Seq.map (fun (k, v) -> k, AIAssemblyRefernece(v))
            aiMap := Map.ofSeq (Seq.concat [ newAiSeq; Map.toSeq !aiMap ])
            refresh()
    
    [<CLIEvent>]
    member this.LoadedAIEvent = loadedAIEvent.Publish

    interface System.IDisposable with
        member this.Dispose() = forget()
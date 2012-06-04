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
open System.IO

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes

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
    
    let aiMap = new Dictionary<String,AIType>();
    do aiMap.Add(defaultAI.Name, AIResolved(defaultAI))

    //
    // Event for when we are done picking AI
    //

    let aiSelectedEvent = new Event<IAntBehavior * IAntBehavior * int>()

    //
    // Event for when we are running a contest
    //

    let contestStartedEvent = new Event<IAntBehavior [] * int>()

    //
    // Event to Cancel out
    //

    let cancelEvent = new Event<unit>()

    //
    // Yay, pretty graphics
    //

    let globalStackPanel = new StackPanel(Height = this.Height, Margin = Thickness(5.0), Orientation = Orientation.Vertical)
    
    let headerText = new TextBlock(Text = "Select Your AI", HorizontalAlignment = HorizontalAlignment.Center)
    do globalStackPanel.Children.SafeAdd headerText

    let listStackPanel = new StackPanel(Height = this.Height * 0.8, Margin = Thickness(5.0), Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Center)
    let listWidth, listHeight = listStackPanel.Width * 0.4, listStackPanel.Height * 1.0
    let leftList = new ListBox(SelectedItem = 0, SelectionMode = SelectionMode.Single, Height = 250., Width = 150.)
    let rightList = new ListBox(SelectedItem = 0, SelectionMode = SelectionMode.Single, Height = 250., Width = 150.)

    let timeLabel = new TextBlock(Text = "Game Cycles")
    let timedGameStackPanel = new StackPanel(Height = this.Height * 0.8, Margin = Thickness(5.0), Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Center)

    let timeTextBox = new TextBox(Text = "1500")

    do timedGameStackPanel.Children.SafeAdd(timeTextBox)
       timedGameStackPanel.Children.SafeAdd(timeLabel)

    do listStackPanel.Children.SafeAdd leftList
       listStackPanel.Children.SafeAdd rightList
       globalStackPanel.Children.SafeAdd listStackPanel
       globalStackPanel.Children.SafeAdd timedGameStackPanel

    // Create Buttons

    let buttonStackPanel = new StackPanel(Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Center)

    let loadButton = Button(Content="Load AI!", Margin = Thickness 5.0)
    let startButton = Button(Content="Let's Go!", Margin = Thickness 5.0)
    let contestButton = Button(Content="Run Contest!", Margin = Thickness 5.0)
    let cancelButton = Button(Content="Cancel!", Margin = Thickness 5.0)

    do  buttonStackPanel.Children.SafeAdd loadButton
        buttonStackPanel.Children.SafeAdd startButton
        buttonStackPanel.Children.SafeAdd contestButton
        buttonStackPanel.Children.SafeAdd cancelButton
        globalStackPanel.Children.SafeAdd buttonStackPanel

    //
    // Let's refresh those lists with our latest AI findings
    //

    let refresh () =
        leftList.Items.Clear()
        rightList.Items.Clear()
        aiMap |> Seq.iter (fun kv ->  
                               leftList.Items.SafeAdd(new ListBoxItem(Content = kv.Key) )    
                               rightList.Items.SafeAdd(new ListBoxItem(Content = kv.Key) ))

    //
    // Reflection based AI Loading
    //

    let addResolvedAIToMap (ai : IAntBehavior) = 
        if not <| aiMap.ContainsKey ai.Name then
            aiMap.Add(ai.Name, AIResolved(ai))
        else
            do MessageBox.Show("Warning. AI Discarded due to same name: " + ai.Name) |> ignore

    let loadAIFromStream (stream : System.IO.Stream) =
        try
#if SILVERLIGHT
            let asmPart = AssemblyPart()
            let asm = asmPart.Load stream
#else
            // here's to hoping the the file isn't bigger that Int32.MAX
            let buffer: byte[] = Array.zeroCreate (int stream.Length)
            stream.Read(buffer, 0, (int stream.Length)) |> ignore
            let asm = Assembly.Load buffer
#endif
            let ai = asm.GetTypes()
                     |> Seq.filter (fun t -> t.IsClass && not t.IsInterface && not t.IsAbstract && not t.IsGenericTypeDefinition)
                     |> Seq.filter (fun t -> typeof<IAntBehavior>.IsAssignableFrom(t))
                     |> Seq.map (fun t -> Activator.CreateInstance(t) :?> IAntBehavior)
                     |> List.ofSeq
                     |> List.nth <| 0
            Some ai
        with e -> headerText.Text <- sprintf "%O" e.Message
                  None
       
    let loadAIFromWeb url : Option<IAntBehavior>= 
        try
            let client = new WebClient()
            let asyncDownload : Async<Option<IAntBehavior>> =                      
                    async {
                        let asyncArgs = Async.AwaitEvent( client.OpenReadCompleted )
                        client.OpenReadAsync url
                        let! args = asyncArgs
                        use resultStream = args.Result
                        return loadAIFromStream resultStream
                    }
            asyncDownload |> Async.RunSynchronously
        with e -> headerText.Text <- sprintf "%O" e.Message
                  None

    let loadAIFromDisk () = 
        try
#if SILVERLIGHT
            let ofd = new OpenFileDialog(Filter = "Assemblies (*.dll;*.exe)|*.dll;*.exe|All Files(*.*)|*.*", FilterIndex = 1, Multiselect = false)
#else
            let ofd = new Microsoft.Win32.OpenFileDialog(Filter = "Assemblies (*.dll;*.exe)|*.dll;*.exe|All Files(*.*)|*.*", FilterIndex = 1, Multiselect = true)
#endif

            let clicked = ofd.ShowDialog()
            if clicked.HasValue && clicked.Value then 
#if SILVERLIGHT
                use stream = ofd.File.OpenRead()
                [ loadAIFromStream stream ]
#else
                [ 
                    for filename in ofd.FileNames do
                        use stream = System.IO.File.OpenRead(filename)
                        yield! loadAIFromStream stream |> Option.toList 
                ]
#endif
            else []  
        with 
        | e -> headerText.Text <- sprintf "%O" e.Message
               []

    let resolveAI ai = 
        match ai with
        | AIResolved behavior -> Some behavior
        | AIAssemblyRefernece reference -> loadAIFromWeb( Uri(reference) )    
    //
    // Configure Button Actions
    //

    let loadButtonClicked () = 
        match loadAIFromDisk() with
        | [] -> ()
        | ais -> for ai in ais do 
                    addResolvedAIToMap ai
                 refresh()

    let startButtonClicked () = 
        let resolveToAI selectedItem = 
            aiMap.[selectedItem] |> resolveAI

        let listBoxItemToString (obj: obj) = 
            let lba = obj :?> ListBoxItem
            lba.Content :?> string
        
        let timed =  Int32.Parse timeTextBox.Text

        let blackAI = resolveToAI <| listBoxItemToString leftList.SelectedValue
        let redAI = resolveToAI <| listBoxItemToString rightList.SelectedValue
        match blackAI, redAI with
        | None, None -> headerText.Text <- "Neither AI Could Be Loaded!"
        | None, Some redAI -> headerText.Text <- "Black AI Couldn't Be Loaded!"
        | Some blackAI, None -> headerText.Text <- "Red AI Couldn't Be Loaded!"
        | Some blackAI, Some redAI -> aiSelectedEvent.Trigger(blackAI, redAI, timed)

    let contestButtonClicked () =
        let loadedAI = aiMap |> Seq.map (fun kv -> resolveAI kv.Value) |> Seq.choose id |> Seq.toArray
        let timed =  Int32.Parse timeTextBox.Text
        contestStartedEvent.Trigger(loadedAI, timed)

    do loadButton.Click.Subscribe (fun _ -> try loadButtonClicked() with e -> headerText.Text <- sprintf "%O" e.Message) |> remember
    do startButton.Click.Subscribe (fun _ -> 
                                        try 
                                            if leftList.SelectedValue = null || rightList.SelectedValue = null then
                                                headerText.Text <- "Must select a black and red stategy"
                                            else
                                                startButtonClicked() 
                                        with e -> headerText.Text <- sprintf "%O" e.Message) |> remember
    do contestButton.Click.Subscribe (fun _ -> contestButtonClicked()) |> remember
    do cancelButton.Click.Subscribe (fun _ -> cancelEvent.Trigger()) |> remember

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
            for (k,v) in newAiSeq do
                aiMap.Add(k, v)
            refresh()

#if SILVERLIGHT
     //HACK: the following function only works on WPF
#else   
    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    member x.addItemsFromDisk args =
        let loaded = args |> Seq.choose (fun fileName -> use stream = File.OpenRead(fileName) in loadAIFromStream stream)
        loaded |> Seq.iter (fun ai -> addResolvedAIToMap ai; refresh())
        loaded

#endif

    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB] 
    member x.AIMap = aiMap
        
    [<CLIEvent>]
    member this.AISelectedEvent = aiSelectedEvent.Publish

    [<CLIEvent>]
    member this.ContestStartedEvent = contestStartedEvent.Publish

    [<CLIEvent>]
    member this.CancelEvent = cancelEvent.Publish

    interface System.IDisposable with
        member this.Dispose() = forget()
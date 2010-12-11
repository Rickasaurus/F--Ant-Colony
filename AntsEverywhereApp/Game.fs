//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missle Command Example
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

open AntsEverywhereLib.Types
open AntsEverywhereLib.World

type GameControl () as this =
    inherit UserControl ()

    let mutable disposables = []
    let remember disposable = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let width, height = 450.0, 450.0
    
    let wm, hm = width / float (xSize + 1), height / float (ySize + 1)
    let offset x y = x * wm, y * hm  
    do  this.Width <- width; this.Height <- height
    let canvas = Canvas(Background=SolidColorBrush(Colors.Orange), Cursor=Cursors.Stylus)

    let layout = Grid()

    let mutable lastScore = None
    let updateScore s =
        lastScore |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Top
        t.Foreground <- SolidColorBrush Colors.White
        lastScore <- Some t
        layout.Children.Add t

    let mutable lastMessage = None
    let updateMessage s =
        lastMessage |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Center
        t.Foreground <- SolidColorBrush Colors.White
        lastMessage <- Some t
        layout.Children.Add t

    let drawAnt x y antColor = 
        let color = match antColor with
                    | AntColor.Black -> Colors.Black
                    | AntColor.Red -> Colors.Red
        let brush = SolidColorBrush(color) :> Brush
        let transform = TranslateTransform(X = x, Y = y)       
        let radius = wm * 0.5
        let yoffset = radius / 2.0
        let xoffset = radius / 6.0
        Ellipse(Width=radius, Height=radius, Fill=brush, RenderTransform = TranslateTransform(X = x + xoffset, Y = y + yoffset))
        |> canvas.Children.Add
        Ellipse(Width=radius, Height=radius, Fill=brush, RenderTransform = TranslateTransform(X = x + (radius / 1.5) + xoffset, Y = y + yoffset))
        |> canvas.Children.Add

    let drawFood x y = 
        let color = Colors.Green
        let brush = SolidColorBrush(color) :> Brush
        let transform = TranslateTransform(X = x, Y = y)
        Ellipse(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.Add

    let makeGradiant quantity max = (float quantity / float max)
    let drawPheromone x y antColor amount =
        let color = match antColor with                    
                    | AntColor.Black -> Colors.Gray
                    | AntColor.Red -> Colors.Purple
        let opacity = makeGradiant amount maxAntDropPheromoneQunatity
        let brush = SolidColorBrush(color, Opacity = opacity) :> Brush
        let transform = TranslateTransform(X = x, Y = y)        
        Rectangle(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.Add

    let drawScore bName bFood rName rFood = 
        do updateScore (sprintf "Black (%s): %d vs Red (%s): %d" bName bFood rName rFood)

    let drawUpdates (world: TheWorld) =
        canvas.Children.Clear()
        canvas.Background <- (SolidColorBrush(Colors.Orange) :> Brush)
        world
        |> Map.iter (fun uid cell -> let x, y = uid2xy uid
                                     let ox, oy = offset (float x) (float y)
                                     cell.Pheromones |> Map.iter (fun color amount -> if amount > 0 then drawPheromone ox oy color amount)
                                     if cell.Food > 0 then drawFood ox oy                                     
                                     if cell.Ant.IsSome then drawAnt ox oy cell.Ant.Value.Color)

    let startGame (blackAi : IAntBehavior) (redAi : IAntBehavior)  = 
        let world = ref (buildWorldInitialWorld())
        drawUpdates !world

        let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Food) 0 !world) * percentFoodToWin
        let cycles = ref 0

        let updateScoreAndCheckForWinner (world: TheWorld) = 
            cycles := !cycles + 1
            let bFood = BlackAntNest.CountFood world
            let rFood = RedAntNest.CountFood world
            drawScore blackAi.Name bFood redAi.Name rFood
            if bFood > foodToWin || rFood > foodToWin || !cycles > maxWorldCycles then
                if bFood > rFood then Some(blackAi)
                elif rFood > bFood then Some(redAi)
                else None
            else None
        
        let timer = DispatcherTimer()
        timer.Interval <- TimeSpan.FromMilliseconds(10.0)
        timer.Tick
        |> Observable.subscribe (fun _ ->
            world := worldCycle blackAi redAi !world
            drawUpdates !world
            let winner = updateScoreAndCheckForWinner !world
            winner |> Option.iter (fun win -> 
                                        updateMessage (sprintf "%s Wins! " win.Name)
                                        timer.Stop())
        )
        |> remember
        timer.Start()
        {new IDisposable with member this.Dispose() = timer.Stop()}
        |> remember

    let startDefaultGame () =
        let redAi = new AntsEverywhereApp.AI.TestAntBehavior() :> IAntBehavior
        let blackAi = new AntsEverywhereApp.AI.TestAntBehavior() :> IAntBehavior
        startGame redAi blackAi

    do  layout.Children.Add canvas
        this.Content <- layout

    let loadAi () = 
        try
            let ofd = OpenFileDialog(Filter = "Assemblies (*.dll;*.exe)|*.dll;*.exe|All Files(*.*)|*.*", FilterIndex = 1, Multiselect = false)

            let clicked = ofd.ShowDialog()
            if clicked.HasValue && clicked.Value then 
                use stream = ofd.File.OpenRead()
                let asmPart = AssemblyPart()
                let asm = asmPart.Load stream
                let ai = asm.GetTypes()
                            |> Seq.filter (fun t -> t.IsClass && not t.IsInterface && not t.IsAbstract && not t.IsGenericTypeDefinition)
                            |> Seq.filter (fun t -> typeof<IAntBehavior>.IsAssignableFrom(t))
                            |> Seq.map (fun t -> Activator.CreateInstance(t) :?> IAntBehavior)
                            |> List.ofSeq
                            |> List.nth <| 0
                Some ai
            else None        
        with 
        | e -> updateMessage e.Message
               None

    do 
        startDefaultGame()
        updateMessage "Click to Load AI"
        this.MouseLeftButtonUp
        |> Observable.subscribe (fun _ -> 
            match loadAi () with
            | Some loadedAi -> forget ()
                               let redAi = new AntsEverywhereApp.AI.TestAntBehavior() :> IAntBehavior
                               startGame loadedAi redAi 
            | None -> ()
            )
        |> remember

    interface System.IDisposable with
        member this.Dispose() = forget()
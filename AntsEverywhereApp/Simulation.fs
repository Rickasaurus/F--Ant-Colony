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

open AntsEverywhereLib.Types
open AntsEverywhereLib.World

type SimulationControl () as this =
    inherit UserControl ()

    let mutable disposables = []
    let remember disposable = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let width, height = 450.0, 450.0
    
    let wm, hm = width / float (xSize + 1), height / float (ySize + 1)
    let offset x y = x * wm, y * hm  
    do  this.Width <- width; this.Height <- height
#if SILVERLIGHT
    let canvas = Canvas(Background=SolidColorBrush(Colors.Orange), Cursor=Cursors.Stylus)
#else
    let canvas = Canvas(Background=SolidColorBrush(Colors.Orange))
#endif

    let layout = Grid()

    let mutable lastScore = None
    let updateScore s =
        lastScore |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Top
        t.Foreground <- SolidColorBrush Colors.White
        lastScore <- Some t
        layout.Children.SafeAdd t

    let mutable lastMessage = None
    let updateMessage s =
        lastMessage |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Center
        t.Foreground <- SolidColorBrush Colors.White
        lastMessage <- Some t
        layout.Children.SafeAdd t

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
        |> canvas.Children.SafeAdd
        Ellipse(Width=radius, Height=radius, Fill=brush, RenderTransform = TranslateTransform(X = x + (radius / 1.5) + xoffset, Y = y + yoffset))
        |> canvas.Children.SafeAdd

    let drawFood x y = 
        let color = Colors.Green
        let brush = SolidColorBrush(color) :> Brush
        let transform = TranslateTransform(X = x, Y = y)
        Ellipse(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.SafeAdd

    let makeGradiant quantity max = (float quantity / float max)
    let drawPheromone x y antColor amount =
        let color = match antColor with                    
                    | AntColor.Black -> Colors.Gray
                    | AntColor.Red -> Colors.Purple
        let opacity = makeGradiant amount maxAntDropPheromoneQunatity
        let brush = SolidColorBrush(color, Opacity = opacity) :> Brush
        let transform = TranslateTransform(X = x, Y = y)        
        Rectangle(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.SafeAdd
        |> ignore

    let drawScore bName bFood rName rFood remaing = 
        do updateScore (sprintf "Black (%s): %d vs Red (%s): %d - Remaining Cycles %d" bName bFood rName rFood remaing)

    let drawUpdates (world: TheWorld) =
        canvas.Children.Clear()
        canvas.Background <- (SolidColorBrush(Colors.Orange) :> Brush)
        world
        |> Map.iter (fun uid cell -> let x, y = uid2xy uid
                                     let ox, oy = offset (float x) (float y)
                                     cell.Pheromones |> Map.iter (fun color amount -> if amount > 0 then drawPheromone ox oy color amount)
                                     if cell.Food > 0 then drawFood ox oy                                     
                                     if cell.Ant.IsSome then drawAnt ox oy cell.Ant.Value.Color)

    let rec startGame (blackAI : IAntBehavior) (redAI : IAntBehavior) maxCycles  = 
        let world = ref (buildWorldInitialWorld())
        drawUpdates !world

        let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Food) 0 !world) * percentFoodToWin
        let cycles = ref 0

        let updateScoreAndCheckForWinner (world: TheWorld) = 
            cycles := !cycles + 1
            let bFood = BlackAntNest.CountFood world
            let rFood = RedAntNest.CountFood world
            drawScore blackAI.Name bFood redAI.Name rFood (maxCycles  - !cycles)
            if bFood > foodToWin || rFood > foodToWin || !cycles > maxCycles then
                if bFood > rFood then Some("Black", blackAI)
                elif rFood > bFood then Some("Red", redAI)
                else None
            else None
        
        let timer = DispatcherTimer()
        timer.Interval <- TimeSpan.FromMilliseconds(10.0)
        timer.Tick
        |> Observable.subscribe (fun _ ->
            try
                world := worldCycle blackAI redAI !world
                drawUpdates !world
                let winner = updateScoreAndCheckForWinner !world
                winner |> Option.iter (fun (color, ai) -> 
                                            updateMessage (sprintf "%s (%s) Won! Click to Load Custom AI." color ai.Name)                                        
                                            forget()
                                            startGame blackAI redAI maxCycles)
            with e -> 
                updateMessage (sprintf "%O" e.Message)
                timer.Stop()
        )
        |> remember
        timer.Start()

        {new IDisposable with member this.Dispose() = timer.Stop()}
        |> remember

    let loadAIEvent = new Event<_>()

    do  layout.Children.SafeAdd canvas
        this.Content <- layout

    do 
        updateMessage "Click to Change AI."
        this.MouseLeftButtonUp
        |> Event.add (fun _ -> 
            forget()
            loadAIEvent.Trigger())
    
    [<CLIEvent>]
    member this.LoadAIEvent = loadAIEvent.Publish

    member this.StartSimulation blackAI redAI maxCylces =
        forget()
        startGame blackAI redAI maxCylces
         
    interface System.IDisposable with
        member this.Dispose() = forget()
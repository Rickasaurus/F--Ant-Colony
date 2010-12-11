//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missle Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereLib.World

open System

open Types
open Helpers

let BlackAntNest = new Nest( 0, 0, nestSize - 1, nestSize - 1 )
let RedAntNest = new Nest( 1 + xSize - nestSize, 1 + ySize - nestSize, nestSize - 1, nestSize - 1)

let (|InBlackNest|InRedNest|Neither|) (x,y) = 
    if BlackAntNest.IsInBounds x y then InBlackNest
    elif RedAntNest.IsInBounds x y then InRedNest
    else Neither

let getAntNest ant =
    match ant.Color with
    | AntColor.Black -> BlackAntNest
    | AntColor.Red -> RedAntNest

//let emptyPheromoneSet = Map.ofSeq 
//                        <| seq { let colors = Enum.GetValues(typeof<AntColor>) :?> AntColor array
//                                 let smells = Enum.GetValues(typeof<PheromoneType>) :?> PheromoneType array
//                                 for color in colors do 
//                                    for smell in smells do 
//                                        yield (color, smell), 0 }

let emptyPheromoneSet = Map.ofSeq 
                        <| seq { let colors = [| AntColor.Black; AntColor.Red |]
                                 for color in colors do 
                                    yield color, 0 }

let defaultCell id = {Id = id; Food = 0; Ant = None; CellType = FieldCell; Pheromones = emptyPheromoneSet }

let defaultBlackAnt = Some { Color = AntColor.Black; FoodCarried = 0 }
let defaultRedAnt = Some { Color = AntColor.Red; FoodCarried = 0 }

let getAnt () = defaultBlackAnt 

let buildWorldInitialWorld () =

    let rnd = new System.Random() in 
        Map.ofSeq <|
        seq { for x in 0 .. xSize do
                for y in 0 .. ySize do
                    let uid = new UID(x, y)
                    let defaultcell = defaultCell uid
                    match x, y with
                    | InBlackNest -> yield uid, { defaultcell with Ant = defaultBlackAnt; CellType = NestCell(AntColor.Black) }
                    | InRedNest ->   yield uid, { defaultcell with Ant = defaultRedAnt; CellType = NestCell(AntColor.Red) }
                    | Neither ->     if chanceOfFood > rnd.NextDouble()
                                        then yield uid, { defaultcell with Food = rnd.Next(minGeneratedFoodPerSquare, maxGeneratedFoodPerSquare) }
                                        else yield uid, defaultcell 
            }

let getAntViews (world: TheWorld) = 
    let getWorldCell x y = Map.tryFind (new UID(x,y)) world
    Map.fold
        (fun state (uid: UID) cell ->
            let x, y = (uid.X, uid.Y)
            match cell.Ant with
            | None -> state
            | Some(ant) ->
                let visibleCells = [ getWorldCell x (y - 1); getWorldCell x (y + 1); getWorldCell (x - 1) y; getWorldCell (x + 1) y ]
                                    |> List.choose id
                state @ [ant, cell, visibleCells, getAntNest ant])
        [] world

let getAntActions (bBehave: IAntBehavior) (rBehave: IAntBehavior) (views: (Ant * WorldCell * WorldCell list * Nest) list) =
    let getAntBehavior ant =
        match ant.Color with
        | AntColor.Black -> bBehave
        | AntColor.Red -> rBehave
    views |> List.map (fun (ant, cell, antView, nest) -> let behavior = getAntBehavior ant in 
                                                            cell, behavior.Behave ant cell antView nest)

let buildTransaction (expectedCells: WorldCell list) actions = 
    let predicate = (fun (world: TheWorld) -> List.forall (fun (cell: WorldCell) -> (Map.find cell.Id world) = cell) expectedCells)
    let action = (fun (iworld: TheWorld) -> 
        List.fold (fun (cworld: TheWorld) (id, action) ->
            Map.add id (action cworld.[id]) cworld) iworld actions)
    predicate, action

let getWorldChangeTransactions actions =
    seq { for source, action in actions do
            let ant = Option.get source.Ant
            match action with
            | Nothing -> ()
            | Move (target) -> if Option.isSome target.Ant then ()
                                else yield buildTransaction 
                                            [ source; target ]
                                            [ source.Id, (fun oldcell -> { oldcell with Ant = None });
                                                target.Id, (fun oldtarget -> { oldtarget with Ant = source.Ant }) ]
            | TakeFood (target) -> if target.Food <= 0 then ()
                                    else 
                                        let foodToGet = min (target.Food) (maxFoodAntCanCarry - ant.FoodCarried)
                                        yield buildTransaction
                                                [ source; target ]
                                                [ target.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food - foodToGet });
                                                    source.Id, (fun oldcell -> { oldcell with Ant = Some { ant with FoodCarried = ant.FoodCarried + foodToGet } } ) ]
            | DropFood (target) -> if target.Food >= maxTotalFoodPerSquare then ()
                                    else 
                                        let foodToDrop = min (maxTotalFoodPerSquare - target.Food) (ant.FoodCarried)
                                        yield buildTransaction
                                                [ source; target ]
                                                [ target.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food + foodToDrop });
                                                    source.Id, (fun oldcell -> { source with Ant = Some { ant with FoodCarried = ant.FoodCarried - foodToDrop } }) ] 
            | DropPheromone (target, quantity) -> let newValue = max (target.Pheromones.[ant.Color] + quantity) maxCellPheromoneQuantity
                                                  yield buildTransaction
                                                            [ target ]
                                                            [ target.Id, (fun oldtarget -> { oldtarget with Pheromones = oldtarget.Pheromones.Add(ant.Color, newValue ) } ) ] }

let degradePheromones (world: TheWorld) = 
    world 
    |> Map.map (fun uid cell -> { cell with Pheromones = cell.Pheromones 
                                                            |> Map.map (fun key quantity -> max (quantity - 1) 0) } )

let applyWorldTransactions (oldWorld: TheWorld) changes = 
    Seq.fold (fun (world: TheWorld) (pred, action) ->
                if pred world 
                then action world  
                else world) 
                oldWorld changes

let uid2xy (uid: UID) = uid.X, uid.Y

let worldCycle bPlayer rPlayer world : TheWorld =
    world            
    |> getAntViews
    |> getAntActions bPlayer rPlayer
    |> Seq.randomPermute
    |> getWorldChangeTransactions
    |> applyWorldTransactions world
    |> degradePheromones

//start the great circle of life
//    let startWorldCycle bPlayer rPlayer drawScore uiUpdate =
//        let initialWorld = buildWorldInitialWorld()
//
//        let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Food) 0 initialWorld) * percentFoodToWin
//
//        let cycles = ref 0
//        let winningCondition (world: TheWorld) = 
//            cycles := !cycles + 1
//            let bFood = BlackAntNest.CountFood world
//            let rFood = RedAntNest.CountFood world
//            if bFood > foodToWin || rFood > foodToWin || !cycles > maxWorldCycles then
//                if bFood > rFood then Some(bPlayer)
//                elif rFood > bFood then Some(rPlayer)
//                else None
//            else None
//
//        let rec worldLoop (world: TheWorld) = 
//            System.Threading.Thread.Sleep(10)
//            let winner = winningCondition world        
//            drawScore (BlackAntNest.CountFood world) (RedAntNest.CountFood world)   
//            let cont = uiUpdate world
//            if Option.isNone winner && cont then
//                worldLoop (worldCycle bPlayer rPlayer world)
//            else winner
//        worldLoop initialWorld
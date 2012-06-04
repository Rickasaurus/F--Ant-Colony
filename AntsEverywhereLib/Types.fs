//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//


module AntsEverywhereLib.Types

open System

let xSize = 15
let ySize = 15
let nestSize = 3
let maxTotalFoodPerSquare = 200
let minGeneratedFoodPerSquare = 10
let maxGeneratedFoodPerSquare = 50
let maxFoodAntCanCarry = 5
let chanceOfFood = 0.12

let spawnFood = 50
let maxAntWounds = 2

let maxCellPheromoneQuantity = 255
let maxAntDropPheromoneQunatity = 50
let pheromoneDispersalRate = 1

let percentFoodToWin = 0.5
let maxWorldCycles = 1500

type UID = { X: int; Y: int }
let uid (x, y) = { X = x; Y = y}

//type PheromoneType =
//    | Sweet = 0 
//    | Sour = 1
//    | Gross = 2

type AntColor =
    | Black
    | Red
    with
        member t.Other = 
            match t with
            | Black -> Red
            | Red -> Black
              
type WorldCellType =
        | FieldCell
        | NestCell of AntColor

[<Struct>] 
type Ant =
    val Color: AntColor
    val FoodCarried: int
    val Wounds: int
    new (color) = { Color = color; FoodCarried = 0; Wounds = 0}
    new (color, food) = { Color = color; FoodCarried = food; Wounds = 0 }
    new (color, food, wounds) = { Color = color; FoodCarried = food; Wounds = wounds }
    member internal x.UpdateFood newFood = new Ant(x.Color, newFood)
    member internal x.UpdateWounds newWounds = new Ant(x.Color, x.FoodCarried, newWounds)
    member x.IsFullOfFood = x.FoodCarried >= maxFoodAntCanCarry
    member x.HasFood = x.FoodCarried > 0
    member x.MaxPheromonesToDrop = maxAntDropPheromoneQunatity

and WorldCell = {
    Id : UID
    Food : int
    Ant : option<Ant> 
    CellType : WorldCellType
    Pheromones : Map<AntColor, int> }
    with
        member t.IsFullOfFood = t.Food >= maxTotalFoodPerSquare

and TheWorld = Map<UID, WorldCell>    

type WorldChange = TheWorld -> TheWorld

[<Struct>] 
type Nest(ix: int, iy: int, sizex: int, sizey: int) =
    member internal t.MinX = ix
    member internal t.MinY = iy
    member internal t.MaxX = ix + sizex
    member internal t.MaxY = iy + sizey  
    member internal t.IsInBounds x y = x >= t.MinX && x <= t.MaxX && y >= t.MinY && y <= t.MaxY
    member t.Distance cell =
            let cx, cy = t.MinX + ((t.MaxX - t.MinX) / 2), t.MinY + ((t.MaxY - t.MinY) / 2)
            let x, y = cell.Id.X, cell.Id.Y
            let pow x = x * x 
            sqrt (pow(double cx - double x) + pow(double cy - double y))
    member t.CountFood (world: TheWorld) = 
            let t = t in Map.fold (fun s (k: UID) v -> if t.IsInBounds k.X k.Y then s + v.Food else s) 0 world   
    member internal t.CellsWithMaxFood (world: TheWorld) = 
            let t = t in
                Map.filter (fun (k: UID) v -> t.IsInBounds k.X k.Y) world   
                |> Map.toList
                |> List.filter (fun (k,v) -> v.IsFullOfFood)

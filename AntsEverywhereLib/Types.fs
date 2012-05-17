//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//


module AntsEverywhereLib.Types

open System

let xSize = 50
let ySize = 50
let nestSize = 5
let maxTotalFoodPerSquare = 200
let minGeneratedFoodPerSquare = 20
let maxGeneratedFoodPerSquare = 100
let maxFoodAntCanCarry = 5
let chanceOfFood = 0.04

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

type Ant = {
    Color : AntColor
    FoodCarried : int }
    with
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

type Nest(ix, iy, sizex, sizey) =
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
            Map.fold (fun s (k: UID) v -> if t.IsInBounds k.X k.Y then s + v.Food else s) 0 world   
    member internal t.CellsWithMaxFood (world: TheWorld) = 
            Map.filter (fun (k: UID) v -> t.IsInBounds k.X k.Y) world   
            |> Map.toList
            |> List.filter (fun (k,v) -> v.IsFullOfFood)
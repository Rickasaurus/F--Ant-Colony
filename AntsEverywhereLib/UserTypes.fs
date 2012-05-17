module AntsEverywhereLib.UserTypes

open AntsEverywhereLib.Types

/// Represents an ant's view of another ant (or him/her self)
[<Struct>]
type AntView (ant: Ant, viewingAnt: Ant) = 
    /// The amount of food carried by this ant
    member t.FoodCarried = ant.FoodCarried
    /// True if the ant is carrying food, false otherwise
    member t.CarryingFood = ant.FoodCarried > 0
    /// True if the ant is carrying it's maximum amount of food, false otherwise
    member t.CarryingMaxFood = ant.FoodCarried = maxFoodAntCanCarry
    /// True if the ant is wounded, false otherwise
    member t.IsWounded = ant.Wounds > 0
    /// True if the ant is an enemy, false otherwise
    member t.IsEnemy = ant.Color <> viewingAnt.Color

/// Represents an ant's view of a particular cell
[<Struct>] 
type AntCellView (wc: WorldCell, ant: Ant, nest: Nest) =
    /// True when the cell cannot contain any more food, false otherwise
    member t.IsFullOfFood = wc.Food >= maxTotalFoodPerSquare
    /// True when the cell contains some food, false otherwise
    member t.HasFood = wc.Food > 0
    /// The amount of food the cell contains. 0 if none
    member t.FoodContained = wc.Food
    /// True when the cell contains an enemy ant, false otherwise
    member t.ContainsEnemyAnt = match wc.Ant with | Some wcant -> wcant.Color <> ant.Color | None -> false
    /// True the the cell contains any ant, false otherwise
    member t.ContainsAnt = wc.Ant.IsSome
    /// The ant occupying this cell, if any
    member t.Ant = let ant = ant in wc.Ant |> Option.map (fun a -> AntView(a, ant))
    /// True if the cell contains a friendly pheromone signal, false otherwise
    member t.HasFriendlyPheromone = not (wc.Pheromones.[ant.Color] = 0)
    /// Returns the quantity of friendly pheromone in this cell, 0 if none
    member t.FriendlyPheromoneQuantity = wc.Pheromones.[ant.Color]
    /// True if the cell contains an enemy pheromone, false otherwise
    member t.HasEnemyPheromone = not (wc.Pheromones.[ant.Color.Other] = 0)
    /// Returns the quantity of enemy pheromone in this cell, 0 if none
    member t.EnemyPheromoneQuantity = wc.Pheromones.[ant.Color.Other]
    /// Returns the maximum amount of pheromones this cell can contain, 0 is always the minimum
    member t.MaxPheromones = maxCellPheromoneQuantity
    /// Returns the maximum amount of food this cell can contain, 0 is always the minimum
    member t.MaxFood = maxTotalFoodPerSquare    
    /// True if the cell is a friendly nest cell, false otherwise
    member t.IsMyNest = wc.CellType = WorldCellType.NestCell(ant.Color)
    /// True if the cell is an enemy nest cell, false otherwise
    member t.IsEnemyNest = wc.CellType = WorldCellType.NestCell(ant.Color.Other)
    /// Returns the distance in cells from this cell to the your friendly nest
    member t.DistanceToNest = if wc.CellType = WorldCellType.NestCell(ant.Color) then 0.0 else nest.Distance wc
    member internal t.WorldCell = wc

/// Represents an ant's view of surrounding cells.
[<Struct>] 
type AntNearbyView (cells: AntCellView list) = 
    static member internal FromWorldCells worldcells ant nest = worldcells |> List.map (fun c -> AntCellView(c, ant, nest)) |> (fun acvs -> AntNearbyView acvs)
    /// A list of all neighboring cells
    member t.Cells = cells
    /// A list of neighboring cells which do not contain ants
    member t.EmptyCells = cells |> List.filter (fun c -> not c.ContainsAnt)
    /// A list of neighboring cells which contain enemy ants
    member t.EnemyCells = cells |> List.filter (fun c -> c.ContainsEnemyAnt)
    /// A list of neighboring cells which are part of the friendly nest
    member t.MyNestCells = cells |> List.filter (fun c -> c.IsMyNest)
    /// A list of neighboring cells which are part of the enemy nest
    member t.EnemyNestCells = cells |> List.filter (fun c -> c.IsEnemyNest)
    /// A list of neighboring cells which contain food but are not part of the friendly nest 
    member t.FoodCollectionCells = cells |> List.filter (fun c -> not c.IsMyNest && c.HasFood)
    /// A list of neighboring cells which contain friendly pheromones and no ants
    member t.FriendlyPheromoneCells = cells |> List.filter (fun c -> not c.ContainsAnt && c.HasFriendlyPheromone)
    /// A list of neighboring cells which are nest cells and are not full of food
    member t.FoodDropCells = t.MyNestCells |> List.filter (fun c -> not c.IsFullOfFood)

type AntAction =
    | Nothing
    | Move of AntCellView
    | TakeFood of AntCellView
    | DropFood of AntCellView
    | DropPheromone of AntCellView * int
    | Attack of AntCellView


type IAntBehavior =
    abstract member Name : string
    abstract member Behave : AntView -> AntCellView -> AntNearbyView -> AntAction     

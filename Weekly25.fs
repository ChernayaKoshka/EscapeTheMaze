
open System
open System.IO

type Position = {x:int; y:int}
type Direction = UP | DOWN | LEFT | RIGHT
type PlayerState = WIN | LOSE | VALIDPOSITION | INVALIDPOSITION
type Player = {pos:Position; facing:Direction}
type Map = {data:char[,]; width:int; height:int}

let defaultMap = [|"#########################################################################";
                   "#   #               #               #           #                   # * #";
                   "#   #   #########   #   #####   #########   #####   #####   #####   #   #";
                   "#               #       #   #           #           #   #   #       #   #";
                   "#########   #   #########   #########   #####   #   #   #   #########   #";
                   "#       #   #               #           #   #   #   #   #           #   #";
                   "#   #   #############   #   #   #########   #####   #   #########   #   #";
                   "#   #               #   #   #       #           #           #       #   #";
                   "#   #############   #####   #####   #   #####   #########   #   #####   #";
                   "#           #       #   #       #   #       #           #   #           #";
                   "#   #####   #####   #   #####   #   #########   #   #   #   #############";
                   "#       #       #   #   #       #       #       #   #   #       #       #";
                   "#############   #   #   #   #########   #   #####   #   #####   #####   #";
                   "#           #   #           #       #   #       #   #       #           #";
                   "#   #####   #   #########   #####   #   #####   #####   #############   #";
                   "#   #       #           #           #       #   #   #               #   #";
                   "#   #   #########   #   #####   #########   #   #   #############   #   #";
                   "#   #           #   #   #   #   #           #               #   #       #";
                   "#   #########   #   #   #   #####   #########   #########   #   #########";
                   "#   #       #   #   #           #           #   #       #               #";
                   "#   #   #####   #####   #####   #########   #####   #   #########   #   #";
                   "#   #                   #           #               #               #   #";
                   "#XXX#####################################################################"|]
                   
let loadMapFromFile (path:string) = 
    let map = match File.Exists path with
               | true -> File.ReadAllLines path
               | false -> defaultMap

    let rows = map.Length
    let columns = map.[0] |> String.length
    let preMap = [|for line in map -> [|for char in line -> char|]|]
    {data = Array2D.init columns rows (fun y x -> preMap.[x].[y]); width=columns; height=rows}

let showMap (map:Map) =
    Console.Clear()
    for y in 0..map.height-1 do
        for x in 0..map.width-1 do
            printf "%c" map.data.[x,y]
        printfn ""
    map

let getPlayerIcon (player:Player) = 
    match player.facing with
    | UP -> '^'
    | DOWN -> 'v'
    | LEFT -> '<'
    | RIGHT -> '>'

let mergePlayer (player:Player) (map:Map)  = 
    player
    |> getPlayerIcon
    |> Array2D.set map.data player.pos.x player.pos.y
    map

let rec getPlayerMove() =
    match Console.ReadKey(false).Key with
    | ConsoleKey.UpArrow    | ConsoleKey.W -> UP
    | ConsoleKey.DownArrow  | ConsoleKey.S -> DOWN
    | ConsoleKey.LeftArrow  | ConsoleKey.A -> LEFT
    | ConsoleKey.RightArrow | ConsoleKey.D -> RIGHT
    | _ -> getPlayerMove()

let adjustPosition position direction =
    match direction with
    | UP ->    {x=position.x;   y=position.y-1}
    | DOWN ->  {x=position.x;   y=position.y+1}
    | LEFT ->  {x=position.x-1; y=position.y}
    | RIGHT -> {x=position.x+1; y=position.y}

let applyMove (player:Player) (direction:Direction) =
    {pos = adjustPosition player.pos direction; facing=direction}

let posWithinBounds position map =
    match position with
    | x when position.x > map.width-1 || position.x < 0 || position.y > map.height-1 || position.y < 0 -> false
    | _ -> true

let bump position direction map = 
    match map.data.[position.x,position.y] with
    | '#' -> 
            let bumpedTo = adjustPosition position direction
            match posWithinBounds bumpedTo map with
            | true ->
                    match map.data.[bumpedTo.x, bumpedTo.y] with
                    | ' ' -> Array2D.set map.data bumpedTo.x bumpedTo.y '#'
                             Array2D.set map.data position.x position.y ' '
                             true
                    | _ -> false
            | false -> false
    | _ -> false

let getPlayerState (player:Player) (map:Map) =
    match map.data.[player.pos.x, player.pos.y] with
    | '#' -> INVALIDPOSITION
    | ' ' -> VALIDPOSITION
    | 'X' -> WIN
    | 'T' -> LOSE
    | _   -> VALIDPOSITION

let rec runGame (player:Player) (map:Map) = 
    let movedPlayer = 
        let tentativeMove = getPlayerMove()
                            |> applyMove player
        match getPlayerState tentativeMove map with
                    | INVALIDPOSITION -> match bump tentativeMove.pos tentativeMove.facing map with
                                         | true -> Array2D.set map.data player.pos.x player.pos.y ' '
                                                   tentativeMove
                                         | false -> {pos=player.pos; facing=tentativeMove.facing}
                    | _ -> Array2D.set map.data player.pos.x player.pos.y ' '
                           tentativeMove
    
    match getPlayerState movedPlayer map with
    | WIN -> WIN
    | LOSE -> LOSE
    | _ -> mergePlayer movedPlayer map
           |> showMap
           |> runGame movedPlayer 

let rec searchRow map pos item =
    match pos.x with
    | x when pos.x > map.width-1 -> None
    | _ -> match map.data.[pos.x,pos.y] with
           | x when map.data.[pos.x,pos.y] = item -> Some pos
           | _ -> searchRow map {x=pos.x+1; y=pos.y} item

let rec nextRow map pos item =
    match pos.y with
    | x when pos.y > map.height-1 -> None
    | _ -> match searchRow map {x=0; y=pos.y;} item with
           | Some x -> printfn "Located starting point at (%d,%d)" x.x x.y
                       Some x
           | None -> nextRow map {x=0; y=pos.y+1} item

let locateFirstInMap item map =
    nextRow map {x=0;y=0} item

[<EntryPoint>]
let main argv =
    let mapName = match argv.Length with 
                  | 0 -> "default"
                  | _ ->  argv.[0]

    let map = loadMapFromFile mapName

    printfn "Loaded map \"%s\". [%d,%d]" mapName map.width map.height

    let startingPosition = match locateFirstInMap '*' map with
                           | Some x -> x
                           | None -> failwith "No starting position set!"

    let player = {pos = startingPosition; facing = UP}

    printfn @""
    printfn @" _______ _______ _______ _______ _______ _______   _________        _______    _______ _______ _______ _______ "
    printfn @"(  ____ (  ____ (  ____ (  ___  (  ____ (  ____ \  \__   __|\     /(  ____ \  (       (  ___  / ___   (  ____ \"
    printfn @"| (    \| (    \| (    \| (   ) | (    )| (    \/     ) (  | )   ( | (    \/  | () () | (   ) \/   )  | (    \/"
    printfn @"| (__   | (_____| |     | (___) | (____)| (__         | |  | (___) | (__      | || || | (___) |   /   | (__    "
    printfn @"|  __)  (_____  | |     |  ___  |  _____|  __)        | |  |  ___  |  __)     | |(_)| |  ___  |  /   /|  __)   "
    printfn @"| (           ) | |     | (   ) | (     | (           | |  | (   ) | (        | |   | | (   ) | /   / | (      "
    printfn @"| (____//\____) | (____/| )   ( | )     | (____/\     | |  | )   ( | (____/\  | )   ( | )   ( |/   (_/| (____/\"
    printfn @"(_______\_______(_______|/     \|/      (_______/     )_(  |/     \(_______/  |/     \|/     \(_______(_______/"
    printfn @"                                                                                                               "
    printfn @""
    printfn "RULES: You can push walls, but you can't push them into other walls. Reach the end (represented by 'X') to win. Your character looks like one of these: ^ v < >"
    printfn "Press any movement key (WASD/Arrow Keys) to start"
    
    match runGame player map with
    | WIN -> printfn "You win."
    | LOSE -> printfn "You lose."
    | _ -> failwith "ERROR! Unexpected player state at end!"
    0 // return an integer exit code

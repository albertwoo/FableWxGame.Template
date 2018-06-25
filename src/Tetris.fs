module Tetris
open TetrisDomain
open Elmish
open Fable.Import
open Fable.Core.JsInterop

type Model = {
    SquareSize: int
    Boundry: int * int
    AllSquares: Square list
    PreviewBlock: Block option
    MovingBlock: Block option
    PrectionBlock: Block option
    IsOver: bool
    Score: int
    DefaultSpeed: int
    Speed: int
    SpeedCount: int }

type Msg = | Action of Action | ReachBottom | ReachLeft | ReachRight

let private newBlock columns =
    generateRandomBlockType()
    |> generateBlock None
    |> fun block ->
        { block with
            Squares =
                block.Squares
                |> List.map (fun x -> { x with Location = fst x.Location, snd x.Location + columns / 2}) }
let private setBlockToBaseLine block =
    let maxRow = block.Squares |> List.maxBy (fun x -> fst x.Location) |> fun x -> fst x.Location
    { block with 
        Squares = block.Squares |> List.map (fun x -> { x with Location = fst x.Location - maxRow, snd x.Location }) }

let private getPredictionBlock boundry squares block =
    moveUntilBlocked boundry squares Down block
    |> fun x ->
        { x with Squares = x.Squares |> List.map (fun square ->
                            let r, g, b, a = square.Color
                            { square with Color = r, g, b, 0.2 } )}

let getTetrisLevel model = model.DefaultSpeed - model.Speed

let init () =
    let rows, columns = 25, 15
    {
        SquareSize = 18
        Boundry = rows - 1, columns - 1
        AllSquares = []
        PreviewBlock = Some(newBlock columns)
        MovingBlock = Some(newBlock columns |> setBlockToBaseLine)
        PrectionBlock = None
        IsOver = false
        Score = 0
        DefaultSpeed = 10
        Speed = 10
        SpeedCount = 0
    }

let update msg model =
    let reachBottom block =
        let squares, lines = cleanSquares model.Boundry (model.AllSquares @ block.Squares)
        let score = model.Score + lines
        { model with
            PreviewBlock = Some(newBlock (snd model.Boundry + 1))
            MovingBlock = Some(model.PreviewBlock.Value |> setBlockToBaseLine)
            PrectionBlock = None
            AllSquares = squares
            Score = score 
            Speed = calculateSpeed model.DefaultSpeed score
            SpeedCount = 0 }, Cmd.none
    let goLeftOrRight action =
        match model.MovingBlock with
        | Some mb ->
            let block = moveUntilBlocked model.Boundry model.AllSquares action mb
            { model with
                MovingBlock = Some(block)
                PrectionBlock = Some(moveUntilBlocked model.Boundry model.AllSquares Down block)}, Cmd.none
        | _ -> model, Cmd.none
    match msg with
    | Action action ->
        let mutable m = model, Cmd.none
        model.MovingBlock
        |> Option.bind (fun mb -> // Check if game is over
            if action = Down &&
                mb.Squares |> List.exists (fun x -> fst x.Location = 0) &&
                isBlocked model.Boundry model.AllSquares mb then
                m <- { model with IsOver = true }, Cmd.none
                None
            else Some(mb))
        |> Option.bind (fun mb -> // Check if other actions except Down will lead to blocked
            let pb = transformBlock action mb
            if isBlocked model.Boundry model.AllSquares pb then
                if action <> Down then m <- model, Cmd.none
                else m <- reachBottom mb
                None
            else Some(mb, pb))
        |> Option.bind (fun (mb, pb) -> // Action is executed
            m <- { model with
                    MovingBlock = Some(pb)
                    PrectionBlock = 
                        if model.PrectionBlock.IsSome && action = Down then model.PrectionBlock
                        else Some(getPredictionBlock model.Boundry model.AllSquares pb) }, Cmd.none
            None)
        |> ignore
        m
    | ReachBottom ->
        match model.PrectionBlock with
        | Some block -> reachBottom block
        | _ -> model, Cmd.none
    | ReachLeft -> goLeftOrRight Action.Left
    | ReachRight -> goLeftOrRight Action.Right

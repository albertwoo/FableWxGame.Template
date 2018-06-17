module FableWxGame

open System
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open TetrisDomain

let defaultSpeed = 10
let calculatSpeed score = 
    let s = defaultSpeed - score / 6
    if s < 1 then 2 else s

type Model = {
    Score: int
    AllSquares: Square list
    Boundry: int * int
    IsOver: bool
    PreviewBlock: Block option
    MovingBlock: Block option
    PrectionBlock: Block option
    TimeCost: int
    Speed: int
    SpeedCount: int
    TouchStartPos: (float * float) option
    IsPaused: bool
    IsRestarting: bool
    HideDetail: bool
}

type Msg =
    | Action of Action
    | BeginRestart | CancelRestart | Restart
    | Tick
    | TouchStart of float * float | TouchEnd of float * float
    | Pause | Continue
    | HideDetail

let init () =
    let rows, columns = 28, 18
    { 
        Score = 0
        Boundry = rows - 1, columns - 1
        AllSquares = []
        IsOver = false
        PreviewBlock = Some(generateBlock (getRandomBlockType()) None |> setBlockToMid columns)
        MovingBlock = Some(generateBlock (getRandomBlockType()) None |> setBlockToMid columns)
        PrectionBlock = None
        TimeCost = 0
        Speed = defaultSpeed
        SpeedCount = 0
        TouchStartPos = None
        IsPaused = false
        IsRestarting = false
        HideDetail = true
    }, Cmd.none

let update msg model =
    match msg with
    | BeginRestart -> { model with IsRestarting = true }, Cmd.none
    | CancelRestart -> { model with IsRestarting = false }, Cmd.none
    | Restart -> init ()
    | Action action ->
        match model.MovingBlock with
        | Some mb ->
            if model.IsPaused || model.IsOver then model, Cmd.none
            elif action = Down && isNewBornBlock mb && isBlocked mb model.Boundry model.AllSquares then
                { model with IsOver = true }, Cmd.none
            else
                let processedMB = transformBlock action mb 1
                if isOutOfBoundry processedMB model.Boundry then model, Cmd.none
                else
                    let blocked = isBlocked processedMB model.Boundry model.AllSquares
                    if action = Down && blocked then
                        let squares, lines = getScore model.Boundry (model.AllSquares @ mb.Squares)
                        let score = model.Score + lines
                        { model with PreviewBlock = Some(generateBlock (getRandomBlockType()) None |> setBlockToMid (snd model.Boundry + 1))
                                     MovingBlock = model.PreviewBlock
                                     PrectionBlock = Some(getPredictionBlock model.PreviewBlock.Value model.Boundry squares)
                                     AllSquares = squares
                                     Score = score 
                                     Speed = calculatSpeed score
                                     SpeedCount = 0
                                     TouchStartPos = None }, Cmd.none
                    elif not blocked then { model with MovingBlock = Some(processedMB)
                                                       PrectionBlock = 
                                                            if model.PrectionBlock.IsSome && action = Down then model.PrectionBlock
                                                            else Some(getPredictionBlock processedMB model.Boundry model.AllSquares) }, Cmd.none
                    else model, Cmd.none
        | _ -> model, Cmd.none
    | Tick ->
        if model.Speed <= model.SpeedCount && not model.IsPaused && not model.IsOver then
            { model with TimeCost = model.TimeCost + 1
                         SpeedCount = 0 }, Cmd.ofMsg (Down |> Msg.Action)
        else
            { model with TimeCost = model.TimeCost + 1
                         SpeedCount = model.SpeedCount + 1 }, Cmd.none
    | TouchStart (p1, p2) -> { model with TouchStartPos = Some(p1, p2) }, Cmd.none
    | TouchEnd (x, y) ->
        match model.TouchStartPos with
        | Some (x0, y0) -> 
            let dx, dy = x - x0, y - y0
            let threshhold = 40.
            let extraSteps (d: float) =
                (Math.Abs d) - threshhold * 3.
                |> fun x -> if x > 0. then int (x / 6.) else 0
                |> fun x -> if x > 5 then 5 else x
            if Math.Abs dx > Math.Abs dy && Math.Abs dx > threshhold then
                if dx > 0. then { model with TouchStartPos = None }, Cmd.batch [for _ in 0..(extraSteps dx) -> Cmd.ofMsg (Right |> Msg.Action)]
                else { model with TouchStartPos = None }, Cmd.batch [for _ in 0..(extraSteps dx) -> Cmd.ofMsg (Left |> Msg.Action)]
            elif Math.Abs dx < Math.Abs dy && dy > threshhold then
                { model with TouchStartPos = None }, Cmd.batch [for _ in 0..(extraSteps dy) -> Cmd.ofMsg (Down |> Msg.Action)]
            else { model with TouchStartPos = None }, Cmd.none
        | _ -> { model with TouchStartPos = None }, Cmd.none
    | Pause -> { model with IsPaused = true }, Cmd.none
    | Continue -> { model with IsPaused = false }, Cmd.none
    | HideDetail -> { model with HideDetail = not model.HideDetail }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let squareSize = 15
    let squareBorder = 1
    let backgroundColor = "#fdefd2"
    let borderColor = "rgba(0, 0, 0, 0.3)"
    let primaryColor = "red"
    let generateColorString color =
        let r, g, b, a = color
        sprintf "rgba(%d, %d, %d, %f)" r g b a
    let rows, columns = fst model.Boundry, snd model.Boundry
    let screenWidth, screenHeight = int window.innerWidth, int window.innerHeight
    let width, height = squareSize * columns, squareSize * rows + 200
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.tabIndex <- 1.
    canvas.focus()
    canvas.width <- float screenWidth
    canvas.height <- float screenHeight
    let context = canvas.getContext_2d()
    let tetrixMarginX, tetrisMarginY = (screenWidth - columns * squareSize) / 2, (screenHeight - height) / 2
    let drawSquare location color (context: CanvasRenderingContext2D) =
        let row, column = location
        let x, y = tetrixMarginX + squareSize * (column - 1), tetrisMarginY + squareSize * (row - 1)
        context.fillStyle <- !^borderColor
        context.fillRect(float x, float y, float squareSize, float squareSize)
        context.fillStyle <- !^color
        context.fillRect(float (x + squareBorder), float (y + squareBorder), float (squareSize - squareBorder * 2), float (squareSize - squareBorder * 2))
    let drawGrid targetContext =
        for r in 0..rows do
            for c in 0..columns do
                drawSquare (r, c) backgroundColor targetContext
        targetContext
    let drawBlock block targetContext =
        match block with
        | Some mb ->
            for square in mb.Squares do drawSquare square.Location (generateColorString square.Color) targetContext
            targetContext
        | _ -> targetContext
    let updateInfo model (targetContext: CanvasRenderingContext2D) =
        let x, y = screenWidth / 2 - width / 2 + 70, tetrisMarginY + squareSize * rows + 25
        targetContext.font <- "16px fantasy"
        targetContext.fillStyle <- !^primaryColor
        targetContext.fillText(sprintf "分数：%d 🙏 %d" (model.Score) (model.TimeCost / 10), float x, float y)
        targetContext

    context.fillStyle <- !^backgroundColor
    context.fillRect(0., 0., float screenWidth, float screenHeight)

    if not model.IsOver && not model.IsPaused && not model.IsRestarting then
        context
        |> drawGrid
        |> drawBlock model.MovingBlock
        |> drawBlock model.PrectionBlock
        |> drawBlock model.PreviewBlock
        |> updateInfo model
        |> fun x ->
            model.AllSquares |> List.iter (fun s -> drawSquare s.Location "rgba(0,0,0,0.8)" x)

let timerTick dispatch = 
    let timer = new System.Timers.Timer(100.0)
    timer.Elapsed.Subscribe (fun _ -> dispatch Tick) |> ignore
    timer.Enabled <- true
    timer.Start()

let keyDown dispatch =
    Browser.document.getElementsByTagName_canvas().[0].addEventListener_keydown (fun e ->
        if e.keyCode = 37. then Left |> Msg.Action |> dispatch
        elif e.keyCode = 38. then Rotate |> Msg.Action |> dispatch
        elif e.keyCode = 39. then Right |> Msg.Action |> dispatch
        elif e.keyCode = 40. then Down |> Msg.Action |> dispatch
    )
let click dispatch =   
    Browser.document.getElementsByTagName_canvas().[0].addEventListener_click (fun _ ->
        Browser.document.getElementsByTagName_canvas().[0].focus()
        Rotate |> Msg.Action |> dispatch
    )
let touch dispatch =
    Browser.document.getElementsByTagName_canvas().[0].addEventListener_touchstart (fun e ->
        (e.changedTouches.[0].clientX, e.changedTouches.[0].clientY) |> TouchStart |> dispatch)
    Browser.document.getElementsByTagName_canvas().[0].addEventListener_touchend (fun e ->
        (e.changedTouches.[0].clientX, e.changedTouches.[0].clientY) |> TouchEnd |> dispatch)

let subscriptions model =
    [
        Cmd.ofSub timerTick
        Cmd.ofSub keyDown
        Cmd.ofSub click
        Cmd.ofSub touch
    ]|> Cmd.batch 

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription subscriptions
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
//|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
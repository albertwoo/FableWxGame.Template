module FableWxGame

open System
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open TetrisDomain
open WechatAPI

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
    TouchStartPoint: (float * float) option
    TouchMovingPoint: (float * float) option
    TouchTime: DateTime option
    IsPaused: bool
    IsRestarting: bool
    HideDetail: bool }

type Msg =
    | Action of Action | GoBottom
    | BeginRestart | CancelRestart | Restart
    | Tick
    | TouchStart of float * float | TouchMove of float * float | TouchEnd of float * float
    | Pause | Continue
    | HideDetail

let init () =
    let rows, columns = 25, 15
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
        TouchStartPoint = None
        TouchMovingPoint = None
        TouchTime = None
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
                                     TouchMovingPoint = None }, Cmd.none
                    elif not blocked then { model with MovingBlock = Some(processedMB)
                                                       PrectionBlock = 
                                                            if model.PrectionBlock.IsSome && action = Down then model.PrectionBlock
                                                            else Some(getPredictionBlock processedMB model.Boundry model.AllSquares) }, Cmd.none
                    else model, Cmd.none
        | _ -> model, Cmd.none
    | GoBottom ->
        match model.PrectionBlock with
        | Some mb ->
            let squares, lines = getScore model.Boundry (model.AllSquares @ mb.Squares)
            let score = model.Score + lines
            { model with PreviewBlock = Some(generateBlock (getRandomBlockType()) None |> setBlockToMid (snd model.Boundry + 1))
                         MovingBlock = model.PreviewBlock
                         PrectionBlock = Some(getPredictionBlock model.PreviewBlock.Value model.Boundry squares)
                         AllSquares = squares
                         Score = score 
                         Speed = calculatSpeed score
                         SpeedCount = 0
                         TouchMovingPoint = None }, Cmd.none
        | _ -> model, Cmd.none
    | Tick ->
        if model.IsOver || model.IsPaused || model.IsRestarting then
            model, Cmd.none
        elif model.Speed <= model.SpeedCount then
            { model with TimeCost = model.TimeCost + 1
                         SpeedCount = 0 }, Cmd.ofMsg (Down |> Msg.Action)
        else
            { model with TimeCost = model.TimeCost + 1
                         SpeedCount = model.SpeedCount + 1 }, Cmd.none
    | TouchStart (x, y) -> { model with TouchStartPoint = Some(x, y)
                                        TouchMovingPoint = Some(x, y)
                                        TouchTime = Some(DateTime.Now) }, Cmd.none
    | TouchMove (x, y) ->
        match model.TouchMovingPoint with
        | Some (x0, y0) -> 
            let dx, dy = x - x0, y - y0
            let threshhold = 20.
            if Math.Abs dx > Math.Abs dy && Math.Abs dx > threshhold then
                if dx > 0. then { model with TouchMovingPoint = Some(x ,y) }, Cmd.ofMsg (Right |> Msg.Action)
                else { model with TouchMovingPoint = Some(x ,y) },  Cmd.ofMsg (Left |> Msg.Action)
            elif Math.Abs dx < Math.Abs dy then
                if dy > threshhold && dy < threshhold * 2. then
                    { model with TouchMovingPoint = Some(x ,y) }, Cmd.ofMsg (Down |> Msg.Action)
                elif dy >= threshhold * 2. then
                    { model with TouchMovingPoint = None }, Cmd.ofMsg GoBottom
                else model, Cmd.none
            else model, Cmd.none
        | _ -> model, Cmd.none
    | TouchEnd (x, y) ->
        match model.TouchStartPoint with
        | Some (x0, y0) -> 
            let dx, dy = x - x0, y - y0
            if Math.Abs dx < 10. && Math.Abs dy < 10. && 
               (DateTime.Now - model.TouchTime.Value).Ticks < TimeSpan.FromMilliseconds(500.).Ticks then
                { model with IsOver = if model.IsOver then false else false
                             TouchStartPoint = None
                             TouchMovingPoint = None
                             TouchTime = None }, Cmd.ofMsg (Rotate |> Msg.Action)
            else { model with TouchStartPoint = None
                              TouchMovingPoint = None
                              TouchTime = None }, Cmd.none
        | _ -> { model with TouchMovingPoint = None
                            TouchTime = None }, Cmd.none
    | Pause -> { model with IsPaused = true; }, Cmd.none
    | Continue -> { model with IsPaused = false }, Cmd.none
    | HideDetail -> { model with HideDetail = not model.HideDetail }, Cmd.none

let squareSize = 20
let squareBorder = 1
let backgroundColor = "#fdefd2"
let borderColor = "rgba(0, 0, 0, 0.3)"
let primaryColor = "red"
let generateColorString color =
    let r, g, b, a = color
    sprintf "rgba(%d, %d, %d, %f)" r g b a
let screenWidth, screenHeight = int window.innerWidth, int window.innerHeight
let canvas = Browser.document.getElementsByTagName_canvas().[0]
canvas.width <- float screenWidth
canvas.height <- float screenHeight

let view (model : Model) (dispatch : Msg -> unit) =
    let rows, columns = fst model.Boundry, snd model.Boundry
    let width, height = squareSize * columns, squareSize * rows + 30

    let context = canvas.getContext_2d()
    context.fillStyle <- !^backgroundColor
    context.fillRect(0., 0., float screenWidth, float screenHeight)

    let tetrisMarginX, tetrisMarginY = (screenWidth - (columns - 1) * squareSize) / 2, (screenHeight - height) / 2
    let drawSquare location color (context: CanvasRenderingContext2D) =
        let row, column = location
        let x, y = tetrisMarginX + squareSize * (column - 1), tetrisMarginY + squareSize * (row - 1)
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
        let x, y = tetrisMarginX - squareSize, tetrisMarginY + squareSize * rows + 30
        targetContext.font <- "14px fantasy"
        targetContext.fillStyle <- !^primaryColor
        targetContext.fillText(sprintf "分数：%d 🙏 %d秒 %d级 @slaveoftime🖖" (model.Score) (model.TimeCost / 10) (defaultSpeed - model.Speed), float x, float y)
        targetContext
    let drawGameOver (targetContext: CanvasRenderingContext2D) =
        let x, y = screenWidth / 2 - 120, screenHeight / 2 - 25
        targetContext.font <- "40px fantasy"
        targetContext.fillStyle <- !^"rgba(0, 0, 0, 0.8)"
        targetContext.fillText("GAME OVER  ", float x - 6., float y - 6.)
        targetContext.font <- "40px fantasy"
        targetContext.fillStyle <- !^primaryColor
        targetContext.fillText("GAME OVER 😝", float x, float y)
        targetContext
    let drawControls (targetContext: CanvasRenderingContext2D) =
        let x, y = screenWidth / 2 - 150, tetrisMarginY + squareSize * rows + 35
        let image = Image.Create()
        image.src <- "./images/controls.png"
        targetContext.drawImage(!^image, float x, float y, 300., 32.)
        targetContext
    
    context
    |> drawGrid
    |> drawBlock model.PreviewBlock
    |> drawBlock model.MovingBlock
    |> drawBlock model.PrectionBlock
    // |> drawControls
    |> updateInfo model
    |> fun x ->
        model.AllSquares |> List.iter (fun s -> drawSquare s.Location "rgba(0,0,0,0.8)" x)
        x
    |> fun x -> if model.IsOver then drawGameOver x else x

let timeTick dispatch = 
    let timer = new System.Timers.Timer(100.0)
    timer.Elapsed.Subscribe (fun _ -> dispatch Tick) |> ignore
    timer.Enabled <- true
    timer.Start()
let keyDownEvent dispatch =
    canvas.addEventListener_keydown (fun e ->
        if e.keyCode = 37. then Left |> Msg.Action |> dispatch
        elif e.keyCode = 38. then Rotate |> Msg.Action |> dispatch
        elif e.keyCode = 39. then Right |> Msg.Action |> dispatch
        elif e.keyCode = 40. then Down |> Msg.Action |> dispatch)
let clickEvent dispatch =   
    canvas.addEventListener_click (fun _ ->
        console.log("clicked")
        Browser.document.getElementsByTagName_canvas().[0].focus()
        Rotate |> Msg.Action |> dispatch)
let touchEvent dispatch =
    canvas.addEventListener_touchstart (fun e ->
        (e.changedTouches.[0].clientX, e.changedTouches.[0].clientY) |> TouchStart |> dispatch)
    canvas.addEventListener_touchmove (fun e ->
        (e.changedTouches.[0].clientX, e.changedTouches.[0].clientY) |> TouchMove |> dispatch)
    canvas.addEventListener_touchend (fun e ->
        (e.changedTouches.[0].clientX, e.changedTouches.[0].clientY) |> TouchEnd |> dispatch)

let subscriptions model =
    [
        Cmd.ofSub timeTick
        Cmd.ofSub keyDownEvent
        Cmd.ofSub clickEvent
        Cmd.ofSub touchEvent
    ]|> Cmd.batch 

// wx.setPreferredFramesPerSecond 20

Program.mkProgram init update view
|> Program.withSubscription subscriptions
#if DEBUG
// |> Program.withConsoleTrace
#endif
|> Program.run
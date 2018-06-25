module FableWxGame

open System
open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open TetrisDomain
open WechatAPI

type Model = {
    Tetris: Tetris.Model
    TimeCost: int
    TouchStartPoint: (float * float) option
    TouchMovingPoint: (float * float) option
    TouchTime: DateTime option
    IsPaused: bool
    IsRestarting: bool
    HideDetail: bool }

type Msg =
    | TetrisMsg of Tetris.Msg
    | BeginRestart | CancelRestart | Restart
    | Tick
    | TouchStart of float * float | TouchMove of float * float | TouchEnd of float * float
    | Pause | Continue
    | HideDetail

let init () =
    {
        Tetris = Tetris.init()
        TimeCost = 0
        TouchStartPoint = None
        TouchMovingPoint = None
        TouchTime = None
        IsPaused = false
        IsRestarting = false
        HideDetail = true
    }, Cmd.none

let update msg model: Model * Cmd<Msg> =
    match msg with
    | BeginRestart -> { model with IsRestarting = true }, Cmd.none
    | CancelRestart -> { model with IsRestarting = false }, Cmd.none
    | Restart -> init ()
    | TetrisMsg msg' ->
        if model.Tetris.IsOver || model.IsPaused || model.IsRestarting then
            model, Cmd.none
        else
            let tetrisModel, cmd = Tetris.update msg' model.Tetris
            { model with Tetris = tetrisModel }, cmd
    | Tick ->
        if model.Tetris.IsOver || model.IsPaused || model.IsRestarting then
            model, Cmd.none
        elif model.Tetris.Speed <= model.Tetris.SpeedCount then
            { model with TimeCost = model.TimeCost + 1
                         Tetris = { model.Tetris with SpeedCount = 0 } }, Cmd.ofMsg (Down |> Tetris.Action |> Msg.TetrisMsg)
        else
            { model with TimeCost = model.TimeCost + 1
                         Tetris = { model.Tetris with SpeedCount = model.Tetris.SpeedCount + 1 } }, Cmd.none
    | TouchStart (x, y) ->
        { model with
            TouchStartPoint = Some(x, y)
            TouchMovingPoint = Some(x, y)
            TouchTime = Some(DateTime.Now) }, Cmd.none
    | TouchMove (x, y) ->
        match model.TouchMovingPoint with
        | Some (x0, y0) ->
            let model1 = { model with TouchMovingPoint = Some(x ,y) }
            let dx, dy = x - x0, y - y0
            let threshhold = 30.
            if Math.Abs dx > Math.Abs dy && Math.Abs dx > threshhold  then
                if dx > 0. then
                    model1, Cmd.ofMsg (Right |> Tetris.Action |> TetrisMsg)
                else
                    model1, Cmd.ofMsg (Left |> Tetris.Action |> TetrisMsg)
            elif Math.Abs dx < Math.Abs dy && dy > threshhold then
                model1, Cmd.ofMsg (Down |> Tetris.Action |> TetrisMsg)
            else model, Cmd.none
        | _ -> model, Cmd.none
    | TouchEnd (x, y) ->
        match model.TouchStartPoint with
        | Some (x0, y0) -> 
            let model' = { model with
                            TouchStartPoint = None
                            TouchMovingPoint = None
                            TouchTime = None }
            let dx, dy = x - x0, y - y0
            let threshhold = 80.
            let isQuickTouch = (DateTime.Now -  model.TouchTime.Value).Ticks < TimeSpan.FromMilliseconds(200.).Ticks
            // if model.Tetris.IsOver then model', Cmd.ofMsg Restart
            if isQuickTouch && Math.Abs dx < 10. && Math.Abs dy < 10. then
                model', Cmd.ofMsg (Rotate |> Tetris.Action |> TetrisMsg)
            elif isQuickTouch && Math.Abs dx > Math.Abs dy && Math.Abs dx > threshhold then
                if dx > 0. then model', Cmd.ofMsg (Tetris.ReachRight |> TetrisMsg)
                else model', Cmd.ofMsg (Tetris.ReachLeft |> TetrisMsg)
            elif isQuickTouch && Math.Abs dx < Math.Abs dy && Math.Abs dy > threshhold then
                model', Cmd.ofMsg (Tetris.ReachBottom |> TetrisMsg) 
            else model', Cmd.none
        | _ -> model, Cmd.none
    | Pause -> { model with IsPaused = true; }, Cmd.none
    | Continue -> { model with IsPaused = false }, Cmd.none
    | HideDetail -> { model with HideDetail = not model.HideDetail }, Cmd.none
   

let screenWidth, screenHeight = int window.innerWidth, int window.innerHeight
let canvas = Browser.document.getElementsByTagName_canvas().[0]
canvas.width <- float screenWidth
canvas.height <- float screenHeight

let view (model : Model) (dispatch : Msg -> unit) =
    let squareSize = model.Tetris.SquareSize
    let squareBorder = 1
    let backgroundColor = "#fdefd2"
    let borderColor = "rgba(0, 0, 0, 0.3)"
    let primaryColor = "red"
    let generateColorString color =
        let r, g, b, a = color
        sprintf "rgba(%d, %d, %d, %f)" r g b a
    let rows, columns = fst model.Tetris.Boundry, snd model.Tetris.Boundry
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
        targetContext.fillText(sprintf "分数：%d 🙏 %d秒 %d级 @slaveoftime🖖" (model.Tetris.Score) (model.TimeCost / 10) (Tetris.getTetrisLevel model.Tetris), float x, float y)
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
    |> fun ctx ->
        match model.Tetris.MovingBlock, model.Tetris.PreviewBlock with
        | Some mb, Some pb -> if not (isSquaresCollided mb.Squares pb.Squares) then drawBlock model.Tetris.PreviewBlock ctx else ctx
        | _ -> ctx
    |> drawBlock model.Tetris.MovingBlock
    |> drawBlock model.Tetris.PrectionBlock
    // |> drawControls
    |> updateInfo model
    |> fun x ->
        model.Tetris.AllSquares |> List.iter (fun s -> drawSquare s.Location "rgba(0,0,0,0.8)" x)
        x
    |> fun x -> if model.Tetris.IsOver then drawGameOver x else x

let timeTick dispatch = 
    let timer = new System.Timers.Timer(100.0)
    timer.Elapsed.Subscribe (fun _ -> dispatch Tick) |> ignore
    timer.Enabled <- true
    timer.Start()
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
        Cmd.ofSub touchEvent
    ]|> Cmd.batch 

wx.setPreferredFramesPerSecond(20)

Program.mkProgram init update view
|> Program.withSubscription subscriptions
#if DEBUG
// |> Program.withConsoleTrace
#endif
|> Program.run
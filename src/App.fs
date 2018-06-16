module FableWxGame

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open System


let drawTetris model (context: CanvasRenderingContext2D) border =
    let w, h = border
    let squareSize = 15.
    let row, column = 30, 20
    let margin = (w - (float column) * squareSize) / 2.
    context.fillStyle <- !^"green"
    context.fillRect(margin, margin, w - margin * 2., (float row) * squareSize)
    let rand = new Random()
    for i in 0..rand.Next(0, 50) do
        context.fillStyle <- !^(sprintf "rgba(%d, %d, %d, %f)" (rand.Next(0, 180)) (rand.Next(0, 180)) (rand.Next(0, 180)) (rand.NextDouble()))
        context.fillRect(margin + squareSize * (rand.Next(0, column) |> float), squareSize * margin + (rand.Next(0, row) |> float), squareSize, squareSize)

let app () =
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- window.innerWidth
    canvas.height <- window.innerHeight
    let context = canvas.getContext_2d()
    // The (!^) operator checks and casts a value to an Erased Union type
    // See http://fable.io/docs/interacting.html#Erase-attribute
    context.fillStyle <- !^"rgba(50,0,0,0.5)"
    context.fillRect (0., 0., window.innerWidth, window.innerHeight)
    let timer = new System.Timers.Timer(100.0)
    timer.Elapsed.Subscribe (fun _ -> drawTetris (DateTime.Now.ToLongTimeString()) context (window.innerWidth, window.innerHeight) ) |> ignore
    timer.Enabled <- true
    timer.Start()

    //let audio = Audio.Create()
    //audio.loop <- true
    //audio.src <- "audio/bgm.mp3"
    //audio.play()

app ()
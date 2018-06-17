module WechatAPI

open Fable.Core
open Fable.Import.Browser
open Fable.Core.JsInterop

type [<AllowNullLiteral>] WX =
    [<Emit("$0($1...)")>] abstract createCanvas: unit -> HTMLCanvasElement
    [<Emit("$0($1...)")>] abstract setPreferredFramesPerSecond: int -> unit
let [<Global>] wx: WX = createEmpty

type [<AllowNullLiteral>] Audio = inherit HTMLAudioElement
type [<AllowNullLiteral>] Image = inherit HTMLImageElement
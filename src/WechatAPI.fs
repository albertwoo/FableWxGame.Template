module WechatAPI

open Fable.Core
open Fable.Import.Browser

type [<AllowNullLiteral>] WX =
    [<Emit("$0($1...)")>] abstract createCanvas: unit -> HTMLCanvasElement
let [<Global>] wx: WX = jsNative

//type [<AllowNullLiteral>] Audio = inherit HTMLAudioElement
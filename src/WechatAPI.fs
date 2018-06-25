module WechatAPI

open Fable.Core

type [<AllowNullLiteral>] IWxApi =
    abstract setPreferredFramesPerSecond: int -> unit
let [<Global>] wx: IWxApi = jsNative

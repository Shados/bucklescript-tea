class type _formdata = object method  append : string -> string -> unit end
type t = _formdata Js.t
external create : unit -> t = "FormData"[@@bs.new ]
let append key value f = (f ## append) key value
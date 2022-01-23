type unresolved
type xmlHttpRequestUpload
type event_readystatechange = Web_json.t
type event_abort = Web_json.t
type event_error = Web_json.t
type event_load = Web_json.t
type event_loadstart = Web_json.t
type event_progress = Web_json.t
type event_timeout = Web_json.t
type event_loadend = Web_json.t
class type _xmlhttprequest =
  object
    method  abort : unit -> unit
    method  getAllResponseHeaders : unit -> string Js.null
    method  getResponseHeader : string -> string Js.null
    method  _open : string -> string -> bool -> string -> string -> unit
    method  overrideMimeType : string -> unit
    method  send : unit -> unit
    method  send__string : string Js.null -> unit
    method  send__formdata : Web_formdata.t -> unit
    method  send__document : Web_document.t -> unit
    method  setRequestHeader : string -> string -> unit
    method  onreadystatechange : event_readystatechange -> unit[@@bs.get ]
    [@@bs.set ]
    method  readyState : int[@@bs.get ]
    method  responseType : string[@@bs.get ][@@bs.set ]
    method  response : unresolved Js.null[@@bs.get ]
    method  responseText : string[@@bs.get ]
    method  responseURL : string[@@bs.get ]
    method  responseXML : Web_document.t Js.null[@@bs.get ]
    method  status : int[@@bs.get ]
    method  statusText : string[@@bs.get ]
    method  timeout : float[@@bs.get ][@@bs.set ]
    method  upload : xmlHttpRequestUpload[@@bs.get ]
    method  withCredentials : bool[@@bs.get ][@@bs.set ]
    method  onabort : event_abort -> unit[@@bs.get ][@@bs.set ]
    method  onerror : event_error -> unit[@@bs.get ][@@bs.set ]
    method  onload : event_load -> unit[@@bs.get ][@@bs.set ]
    method  onloadstart : event_loadstart -> unit[@@bs.get ][@@bs.set ]
    method  onprogress : event_loadstart -> unit[@@bs.get ][@@bs.set ]
    method  ontimeout : event_timeout -> unit[@@bs.get ][@@bs.set ]
    method  onloadend : event_loadend -> unit[@@bs.get ][@@bs.set ]
  end
type t = _xmlhttprequest Js.t
external create : unit -> t = "XMLHttpRequest"[@@bs.new ]
type errors =
  | IncompleteResponse 
  | NetworkError 
type body =
  | EmptyBody 
  | EmptyStringBody 
  | StringBody of string 
  | FormDataBody of Web_formdata.t 
  | FormListBody of (string * string) list 
  | DocumentBody of Web_document.t 
let abort (x : t) = ((x ## abort) () : unit)
let getAllResponseHeaders (x : t) =
  (let open Tea_result in
     match Js.Null.toOption ((x ## getAllResponseHeaders) ()) with
     | None -> ((Error (IncompleteResponse))[@explicit_arity ])
     | ((Some (""))[@explicit_arity ]) -> ((Error (NetworkError))
         [@explicit_arity ])
     | ((Some (s))[@explicit_arity ]) -> ((Ok (s))[@explicit_arity ]) : 
  (string, errors) Tea_result.t)
let getAllResponseHeadersAsList (x : t) =
  (let open Tea_result in
     match getAllResponseHeaders x with
     | Error _ as err -> err
     | ((Ok (s))[@explicit_arity ]) ->
         ((Ok
             ((((((s |> (Js.String.split "\r\n")) |>
                    (Array.map (Js.String.splitAtMost ": " ~limit:2)))
                   |> Array.to_list)
                  |> (List.filter (fun a -> (Array.length a) == 2)))
                 |>
                 (List.map
                    (function
                     | [|key;value|] -> (key, value)
                     | _ -> failwith "Cannot happen, already checked length")))))
         [@explicit_arity ]) : ((string * string) list, errors) Tea_result.t)
let getAllResponseHeadersAsDict (x : t) =
  (let module StringMap = (Map.Make)(String) in
     match getAllResponseHeadersAsList x with
     | Tea_result.Error _ as err -> err
     | ((Tea_result.Ok (l))[@explicit_arity ]) ->
         let insert d (k, v) = StringMap.add k v d in
         ((Tea_result.Ok ((List.fold_left insert StringMap.empty l)))
           [@explicit_arity ]) : (string Map.Make(String).t, errors)
                                   Tea_result.t)
let getResponseHeader key x = Js.Null.toOption ((x ## getResponse) key)
let open_ (method' : string) (url : string) ?(async= true)  ?(user= "") 
  ?(password= "")  x = (x ## _open) method' url async user password
let overrideMimeType (mimetype : string) (x : t) =
  ((x ## overrideMimeType) mimetype : unit)
let send (body : body) (x : t) =
  (match body with
   | EmptyBody -> (x ## send) ()
   | EmptyStringBody -> (x ## send__string) Js.Null.empty
   | ((StringBody (s))[@explicit_arity ]) ->
       (x ## send__string) (Js.Null.return s)
   | ((FormDataBody (f))[@explicit_arity ]) -> (x ## send__formdata) f
   | ((FormListBody (l))[@explicit_arity ]) ->
       let form =
         List.fold_left
           (fun f ->
              fun (key, value) ->
                let () = Web_formdata.append key value f in f)
           (Web_formdata.create ()) l in
       (x ## send__formdata) form
   | ((DocumentBody (d))[@explicit_arity ]) -> (x ## send__document) d : 
  unit)
let setRequestHeader (header : string) (value : string) (x : t) =
  (x ## setRequestHeader) header value
type state =
  | Unsent 
  | Opened 
  | HeadersReceived 
  | Loading 
  | Done 
type responseType =
  | StringResponseType 
  | ArrayBufferResponseType 
  | BlobResponseType 
  | DocumentResponseType 
  | JsonResponseType 
  | TextResponseType 
  | RawResponseType of string 
type responseBody =
  | NoResponse 
  | StringResponse of string 
  | ArrayBufferResponse of unit 
  | BlobResponse of unit 
  | DocumentResponse of Web_document.t 
  | JsonResponse of Web_json.t 
  | TextResponse of string 
  | RawResponse of string * unit 
let set_onreadystatechange (cb : event_readystatechange -> unit) (x : t) =
  ((x ## onreadystatechange) #= cb : unit)
let get_onreadystatechange (x : t) =
  (x ## onreadystatechange : event_readystatechange -> unit)
let readyState (x : t) =
  (match x ## readyState with
   | 0 -> Unsent
   | 1 -> Opened
   | 2 -> HeadersReceived
   | 3 -> Loading
   | 4 -> Done
   | i ->
       failwith ("Invalid return from 'readystate' of: " ^ (string_of_int i)) : 
  state)
let set_responseType (typ : responseType) (x : t) =
  (match typ with
   | StringResponseType -> (x ## responseType) #= ""
   | ArrayBufferResponseType -> (x ## responseType) #= "arraybuffer"
   | BlobResponseType -> (x ## responseType) #= "blob"
   | DocumentResponseType -> (x ## responseType) #= "document"
   | JsonResponseType -> (x ## responseType) #= "json"
   | TextResponseType -> (x ## responseType) #= "text"
   | ((RawResponseType (s))[@explicit_arity ]) -> (x ## responseType) #= s : 
  unit)
let get_responseType (x : t) =
  (match x ## responseType with
   | "" -> StringResponseType
   | "arraybuffer" -> ArrayBufferResponseType
   | "blob" -> BlobResponseType
   | "document" -> DocumentResponseType
   | "json" -> JsonResponseType
   | "text" -> TextResponseType
   | s -> ((RawResponseType (s))[@explicit_arity ]) : responseType)
let get_response (x : t) =
  (match Js.Null.toOption (x ## response) with
   | None -> NoResponse
   | ((Some (resp))[@explicit_arity ]) ->
       (match get_responseType x with
        | StringResponseType -> ((StringResponse ((Obj.magic resp)))
            [@explicit_arity ])
        | ArrayBufferResponseType ->
            ((ArrayBufferResponse ((Obj.magic resp)))[@explicit_arity ])
        | BlobResponseType -> ((BlobResponse ((Obj.magic resp)))
            [@explicit_arity ])
        | DocumentResponseType -> ((DocumentResponse ((Obj.magic resp)))
            [@explicit_arity ])
        | JsonResponseType -> ((JsonResponse ((Obj.magic resp)))
            [@explicit_arity ])
        | TextResponseType -> ((TextResponse ((Obj.magic resp)))
            [@explicit_arity ])
        | ((RawResponseType (s))[@explicit_arity ]) ->
            ((RawResponse (s, (Obj.magic resp)))[@implicit_arity ])) : 
  responseBody)
let get_responseText (x : t) = (x ## responseText : string)
let get_responseURL (x : t) = (x ## responseURL : string)
let get_responseXML (x : t) =
  (Js.Null.toOption (x ## responseXML) : Web_document.t option)
let get_status (x : t) = (x ## status : int)
let get_statusText (x : t) = (x ## statusText : string)
let set_timeout (t : float) (x : t) = ((x ## timeout) #= t : unit)
let get_timeout (x : t) = (x ## timeout : float)
let set_withCredentials (b : bool) (x : t) =
  ((x ## withCredentials) #= b : unit)
let get_withCredentials (x : t) = (x ## withCredentials : bool)
let set_onabort (cb : event_abort -> unit) (x : t) =
  ((x ## onabort) #= cb : unit)
let get_onabort (x : t) = (x ## onabort : event_abort -> unit)
let set_onerror (cb : event_error -> unit) (x : t) =
  ((x ## onerror) #= cb : unit)
let get_onerror (x : t) = (x ## onerror : event_error -> unit)
let set_onload (cb : event_load -> unit) (x : t) =
  ((x ## onload) #= cb : unit)
let get_onload (x : t) = (x ## onload : event_load -> unit)
let set_onloadstart (cb : event_loadstart -> unit) (x : t) =
  ((x ## onloadstart) #= cb : unit)
let get_onloadstart (x : t) = (x ## onloadstart : event_loadstart -> unit)
let set_onprogress (cb : event_loadstart -> unit) (x : t) =
  ((x ## onprogress) #= cb : unit)
let get_onprogress (x : t) = (x ## onprogress : event_loadstart -> unit)
let set_ontimeout (cb : event_timeout -> unit) (x : t) =
  ((x ## ontimeout) #= cb : unit)
let get_ontimeout (x : t) = (x ## ontimeout : event_timeout -> unit)
let set_onloadend (cb : event_loadend -> unit) (x : t) =
  ((x ## onloadend) #= cb : unit)
let get_onloadend (x : t) = (x ## onloadend : event_loadend -> unit)
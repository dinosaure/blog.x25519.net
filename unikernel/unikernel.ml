module Make
    (Console : Mirage_console.S)
    (StackV4 : Mirage_stack.V4)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S) = struct
  module Paf = Paf.Make(StackV4)
  module TCP = Paf.TCP

  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  open Lwt.Infix
  open Httpaf

  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let _html = Fpath.v "html"
  let _index = Fpath.(_html / "index.html")
  let _error = "Internal server error."

  exception Conflict

  let connect_store resolver conduit =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repository ->
    repository, Store.remote ~conduit ~resolver (Key_gen.remote ())

  let respond_404 key reqd =
    let contents = Fmt.strf "Resource %a not found." Fpath.pp key in
    let headers = Headers.of_list
                    [ "content-length", string_of_int (String.length contents)
                    ; "connection", "close" ] in 
    let response = Response.create ~headers `Not_found in
    Reqd.respond_with_string reqd response contents

  let display console store reqd key =
    Store.find store (Fpath.segs key) >>= fun contents -> match contents, Fpath.get_ext key with
    | None, _ ->
      respond_404 key reqd ;
      log console "Unavailable %a ressource." Fpath.pp key
    | Some contents, "html" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "text/html"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as text/html) delivered!" Fpath.pp key
    | Some contents, "css" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "text/css"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as text/css) delivered!" Fpath.pp key
    | Some _, _ ->
      respond_404 key reqd ;
      log console "Invalid %a ressource." Fpath.pp key

  let reload console store remote =
    Sync.pull store remote `Set >>= function
    | Ok `Empty | Ok (`Head _) ->
      log console "Synchronization done."
    | Error (`Msg err) ->
      log console "Synchronization error: %s" err >>= fun () ->
      Lwt.fail (Failure err)
    | Error (`Conflict err) ->
      log console "Conflict while synchronization: %s" err >>= fun () ->
      Lwt.fail Conflict

  let dispatch console remote store reqd =
    let request = Reqd.request reqd in
    let target = Uri.of_string request.Request.target in
    let target = Astring.String.trim ~drop:(Char.equal '/') (Uri.path target) in
    let target = Astring.String.cuts ~sep:"/" target in

    match target with
    | [] -> display console store reqd _index
    | [ "_update" ] -> reload console store remote
    | _ -> match Fpath.of_string request.Request.target with
      | Ok key -> display console store reqd key
      | Error _ ->
        log console "Invalid ressource %S." request.Request.target >>= fun () ->
        Lwt.fail Not_found

  let request_handler console remote store edn reqd =
    let res () =
      Lwt.catch
        (fun () -> dispatch console remote store reqd)
        (fun exn ->
           let exn = Printexc.to_string exn in
           log console "Got an error: %s." exn >>= fun () ->
           let headers = Headers.of_list [ "content-length", string_of_int (String.length _error)
                                         ; "connection", "close" ] in
           let response = Response.create ~headers `Internal_server_error in
           Reqd.respond_with_string reqd response _error ;
           Lwt.return ()) in
    Lwt.async res

  let error_handler _ ?request:_ _ _ = ()

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let start console stack resolver conduit =
    connect_store resolver conduit >>= fun (store, remote) ->
    Sync.pull store remote `Set >>= function
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
    | Ok `Empty | Ok (`Head _) ->
      let config =
        { Tuyau_mirage_tcp.port= Key_gen.port ()
        ; Tuyau_mirage_tcp.keepalive= None
        ; Tuyau_mirage_tcp.stack } in
      let request_handler = request_handler console remote store in
      let fiber () =
        Tuyau_mirage.serve ~key:TCP.configuration config ~service:TCP.service >>? fun (master, _) ->
        Paf.http ~request_handler ~error_handler master in
      fiber () >>= function
      | Ok () -> (* TODO(dinosaure): properly close [master]. *) Lwt.return ()
      | Error err ->
        log console "Got an error while initialization: %a." Tuyau_mirage.pp_error err
end

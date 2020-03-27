let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Make
    (Console : Mirage_console.S)
    (Time : Mirage_time.S)
    (StackV4 : Mirage_stack.V4)
    (Certificate : Mirage_kv.RO)
    (Resolver : Resolver_lwt.S)
    (Conduit : Conduit_mirage.S) = struct
  module Key = Mirage_kv.Key
  module Paf = Paf.Make(Time)(StackV4)
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
    log console "Deliver %a." Fpath.pp key >>= fun () ->
    Store.find store (Fpath.segs key) >>= fun contents -> match contents, Fpath.get_ext key with
    | None, _ ->
      respond_404 key reqd ;
      log console "Unavailable %a resource." Fpath.pp key
    | Some contents, ".html" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "text/html"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as text/html) delivered!" Fpath.pp key
    | Some contents, ".css" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "text/css"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as text/css) delivered!" Fpath.pp key
    | Some contents, ".xml" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "application/rss+xml"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as application/rss+xml) delivered!" Fpath.pp key
    | Some contents, ".png" ->
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "content-type", "image/png"
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      log console "%a (as image/png) delivered!" Fpath.pp key
    | Some _, _ ->
      respond_404 key reqd ;
      log console "Invalid %a resource (extension: %s)." Fpath.pp key (Fpath.get_ext key)

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
    | [ "" ] -> display console store reqd _index
    | [ "_update" ] ->
      reload console store remote >>= fun () ->
      let contents = "Blog updated!" in
      let headers = Headers.of_list
                      [ "content-length", string_of_int (String.length contents)
                      ; "connection", "close" ] in
      let response = Response.create ~headers `OK in
      Reqd.respond_with_string reqd response contents ;
      Lwt.return ()
    | _ ->
      let target = Astring.String.trim ~drop:(Char.equal '/') request.Request.target in
      match Fpath.of_string target with
      | Ok key -> display console store reqd Fpath.(_html // key)
      | Error _ ->
        log console "Invalid resource %S." request.Request.target >>= fun () ->
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

  let null ~host:_ _ = Ok None

  let start console time stack local resolver conduit =
    connect_store resolver conduit >>= fun (store, remote) ->
    let get_tls_configuration () =
      Certificate.get local Key.(v (Key_gen.private_key ())) >>= fun private_key ->
      Certificate.get local Key.(v (Key_gen.pem ())) >>= fun cert ->
      match private_key, cert with
      | Ok private_key, Ok cert ->
        ( match X509.Private_key.decode_pem (Cstruct.of_string private_key),
                X509.Certificate.decode_pem_multiple (Cstruct.of_string cert) with
        | Ok (`RSA private_key), Ok certs -> Lwt.return (private_key, certs)
        | Ok _, _ -> invalid_arg "Invalid PEM certificate %S" cert
        | _, Ok _ -> invalid_arg "Invalid private key" )
      | _, _ -> invalid_arg "The blog needs a certificate" in
    get_tls_configuration () >>= fun (private_key, certs) ->
    Sync.pull store remote `Set >>= function
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
    | Ok `Empty | Ok (`Head _) ->
      let tcp_config ~port =
        { Tuyau_mirage_tcp.port= port
        ; Tuyau_mirage_tcp.keepalive= None
        ; Tuyau_mirage_tcp.nodelay= false
        ; Tuyau_mirage_tcp.stack } in
      let tls_config =
        Tls.Config.server
          ~certificates:(`Single (certs, private_key))
          ~authenticator:null () in
      let request_handler = request_handler console remote store in
      let http_fiber () =
        (Tuyau_mirage.serve
           ~key:TCP.configuration
           (tcp_config ~port:(Key_gen.http_port ()))
           ~service:TCP.service >>? fun (master, _) ->
         Paf.http ~request_handler ~error_handler master) >>= fun _ ->
        Lwt.return () in
      let https_fiber () =
        (Tuyau_mirage.serve
           ~key:Paf.tls_configuration
           (tcp_config ~port:(Key_gen.https_port ()), tls_config)
           ~service:Paf.tls_service >>? fun (master, _) ->
         Paf.https ~request_handler ~error_handler master) >>= fun _ ->
        Lwt.return () in
      Lwt.join [ http_fiber (); https_fiber () ]
end

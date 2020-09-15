let () = Printexc.record_backtrace true

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

module Make
    (Console : Mirage_console.S)
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (StackV4 : Mirage_stack.V4) = struct
  module Key = Mirage_kv.Key
  module Paf = Paf.Make(Time)(StackV4)
  module TCP = Paf.TCP
  module DNS = Conduit_mirage_dns.Make(Random)(Time)(Mclock)(StackV4)
  module SSH = Awa_conduit.Make(Lwt)(Conduit_mirage)(Mclock)
  module Certify = Dns_certify_mirage.Make(Random)(Pclock)(Time)(StackV4)
  module Store = Irmin_mirage_git.Mem.KV(Irmin.Contents.String)
  module Sync = Irmin.Sync(Store)

  let ssh_protocol = SSH.protocol_with_ssh TCP.protocol

  open Lwt.Infix
  open Httpaf

  let log console fmt = Fmt.kstrf (Console.log console) fmt

  let _html = Fpath.v "html"
  let _index = Fpath.(_html / "index.html")
  let _error = "Internal server error."

  exception Conflict

  let git_edn edn =
    match Smart_git.endpoint_of_string edn with
    | Ok edn -> edn
    | Error (`Msg err) -> Fmt.invalid_arg "Invalid Git endpoint (%s): %s." edn err

  let connect_store ~resolvers =
    let config = Irmin_mem.config () in
    Store.Repo.v config >>= Store.master >|= fun repository ->
    repository, Store.remote ~resolvers (Key_gen.remote ())

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

  let exit_expired = 80

  let quit_before_expire = function
    | `Single (server :: _, _) ->
      let expiry = snd (X509.Certificate.validity server) in
      let diff = Ptime.diff expiry (Ptime.v (Pclock.now_d_ps ())) in
      ( match Ptime.Span.to_int_s diff with
        | None -> invalid_arg "couldn't convert span to seconds"
        | Some x when x < 0 -> invalid_arg "diff is negative"
        | Some x ->
          Lwt.async @@ fun () ->
          Time.sleep_ns Int64.(sub (Duration.of_sec x) (Duration.of_day 1)) >|= fun () ->
          exit exit_expired )
    | _ -> ()

  let tls stack hostname =
    Certify.retrieve_certificate stack ~dns_key:(Key_gen.dns_key ())
      ~hostname (Key_gen.dns_server ()) (Key_gen.dns_port ()) >>= function
    | Error (`Msg err) -> Lwt.fail (Failure err)
    | Ok certificates ->
      quit_before_expire certificates ;
      let conf = Tls.Config.server ~certificates () in
      Lwt.return conf

  let ssh_cfg edn =
    match edn, Key_gen.ssh_seed (), Key_gen.ssh_auth () with
    | { Smart_git.scheme= `SSH user; path; _ }, Some seed, Some auth ->
      let authenticator = match Awa.Keys.authenticator_of_string auth with
        | Ok v -> Some v
        | Error err -> None in
      let seed = Awa.Keys.of_seed seed in
      let req = Awa.Ssh.Exec (Fmt.strf "git-upload-pack '%s'" path) in
      Some { Awa_conduit.user; key= seed; req
           ; authenticator }
    | _ -> None

  let start console random mclock pclock time stack =
    let dns = DNS.create stack in
    let ssh_cfg = ssh_cfg (git_edn (Key_gen.remote ())) in
    let irmin_resolvers =
      let tcp_resolve ~port = DNS.resolv stack ?nameserver:None dns ~port in
      match ssh_cfg with
      | Some ssh_cfg ->
        let ssh_resolve domain_name =
          tcp_resolve ~port:22 domain_name >>= function
          | Some edn -> Lwt.return_some (edn, ssh_cfg)
          | None -> Lwt.return_none in
        Conduit_mirage.empty
        |> Conduit_mirage.add
            ~priority:10 ssh_protocol ssh_resolve
        |> Conduit_mirage.add
             TCP.protocol (tcp_resolve ~port:9418)
      | None ->
        Conduit_mirage.add
          TCP.protocol (tcp_resolve ~port:9418)
          Conduit_mirage.empty in
    connect_store ~resolvers:irmin_resolvers >>= fun (store, remote) ->
    tls stack Domain_name.(host_exn (of_string_exn (Key_gen.hostname ())))
    >>= fun tls_config ->
    Sync.pull store remote `Set >>= fun res ->
    match res with
    | Error (`Msg err) -> failwith err
    | Error (`Conflict err) -> failwith err
    | Ok `Empty | Ok (`Head _) ->
      let tcp_config ~port =
        { Conduit_mirage_tcp.port= port
        ; Conduit_mirage_tcp.keepalive= None
        ; Conduit_mirage_tcp.nodelay= false
        ; Conduit_mirage_tcp.stack } in
      let request_handler = request_handler console remote store in
      let http_fiber () =
        (Conduit_mirage.Service.init
           (tcp_config ~port:(Key_gen.http_port ()))
           ~service:TCP.service >>? fun master ->
         Paf.http ~request_handler ~error_handler master) >>= fun _ ->
        Lwt.return () in
      let https_fiber () =
        (Conduit_mirage.Service.init
           (tcp_config ~port:(Key_gen.https_port ()), tls_config)
           ~service:Paf.tls_service >>? fun master ->
         Paf.https ~request_handler ~error_handler master) >>= fun _ ->
        Lwt.return () in
      Lwt.join [ http_fiber (); https_fiber () ]
end

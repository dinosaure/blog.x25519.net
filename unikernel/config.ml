open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/__blog" doc))

let http_port =
  let doc = Key.Arg.info ~doc:"port of HTTP service" [ "p"; "port" ] in
  Key.(create "http_port" Arg.(opt int 80 doc))

let https_port =
  let doc = Key.Arg.info ~doc:"port of HTTPS service" [ "https-port" ] in
  Key.(create "https_port" Arg.(opt int 443 doc))

let private_key =
  let doc = Key.Arg.info ~doc:"Private key." [ "k"; "private-key" ] in
  Key.(create "private_key" Arg.(required string doc))

let pem =
  let doc = Key.Arg.info ~doc:"Certificate." [ "c"; "certificate" ] in
  Key.(create "pem" Arg.(required string doc))

let __blog =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote
          ; Key.abstract http_port
          ; Key.abstract https_port
          ; Key.abstract private_key
          ; Key.abstract pem ]
    (console @-> time @-> stackv4 @-> kv_ro @-> resolver @-> conduit @-> job)

let stack = generic_stackv4 default_network
let conduit = conduit_direct stack
let resolver = resolver_dns stack
let console = default_console
let time = default_time
let certificate = generic_kv_ro "cert"

let packages =
  let tuyau = "git+https://github.com/dinosaure/tuyau.git" in
  let paf = "git+https://github.com/dinosaure/paf-le-chien.git" in
  [ package "httpaf"
  ; package "irmin-mirage-git"

  ; package ~pin:tuyau "tuyau"
  ; package ~pin:tuyau "tuyau-tls"
  ; package ~pin:tuyau ~sublibs:["tcp"; "tls"] "tuyau-mirage"
  ; package ~pin:paf "paf" ]

let () =
  register "__blog"
    ~packages
    [ __blog $ console $ time $ stack $ certificate $ resolver $ conduit ]

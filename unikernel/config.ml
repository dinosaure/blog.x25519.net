open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/__blog" doc))

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service" [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

let __blog =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote; Key.abstract port ]
    (console @-> time @-> stackv4 @-> resolver @-> conduit @-> job)

let stack = generic_stackv4 default_network
let conduit = conduit_direct stack
let resolver = resolver_dns stack
let console = default_console
let time = default_time

let packages =
  let tuyau = "git+https://github.com/dinosaure/tuyau.git#fix-blog" in
  let paf = "git+https://github.com/dinosaure/paf-le-chien.git#fix-blog" in
  [ package "httpaf"
  ; package "irmin-mirage-git"

  ; package ~pin:tuyau "tuyau"
  ; package ~pin:tuyau "tuyau-tls"
  ; package ~pin:tuyau ~sublibs:["tcp"; "tls"] "tuyau-mirage"
  ; package ~pin:paf "paf" ]

let () =
  register "__blog"
    ~packages
    [ __blog $ console $ time $ stack $ resolver $ conduit ]

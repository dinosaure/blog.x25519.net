open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/__blog" doc))

let http_port =
  let doc = Key.Arg.info ~doc:"port of HTTP service" [ "p"; "port" ] in
  Key.(create "http-port" Arg.(opt int 80 doc))

let https_port =
  let doc = Key.Arg.info ~doc:"port of HTTPS service" [ "https-port" ] in
  Key.(create "https-port" Arg.(opt int 443 doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"DNS key" [ "dns-key" ] in
  Key.(create "dns_key" Arg.(required string doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"DNS server port" [ "dns-port" ] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"DNS server" [ "dns-server" ] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname of the unikernel" [ "hostname" ] in
  Key.(create "hostname" Arg.(required string doc))

let ssh_seed =
  let doc = Key.Arg.info ~doc:"Seed of the private SSH key" [ "ssh-seed" ] in
  Key.(create "ssh-seed" Arg.(opt (some string) None doc))

let ssh_auth =
  let doc = Key.Arg.info ~doc:"SSH Public key of the remote Git endpoint" [ "ssh-auth" ] in
  Key.(create "ssh-auth" Arg.(opt (some string) None doc))

let __blog =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote
          ; Key.abstract http_port
          ; Key.abstract https_port
          ; Key.abstract dns_key
          ; Key.abstract dns_server
          ; Key.abstract dns_port
          ; Key.abstract ssh_seed
          ; Key.abstract ssh_auth
          ; Key.abstract hostname ]
    (console @-> random @-> mclock @-> pclock @-> time @-> stackv4 @-> job)

let stack = generic_stackv4 default_network
let console = default_console
let random = default_random
let time = default_time
let mclock = default_monotonic_clock
let pclock = default_posix_clock

let packages =
  let paf = "git+https://github.com/dinosaure/paf-le-chien.git" in
  [ package "httpaf"
  ; package "irmin-mirage-git"
  ; package ~sublibs:[ "dns" ] "conduit-mirage"
  ; package ~sublibs:[ "mirage" ] "dns-certify"
  ; package "awa-conduit"
  ; package ~pin:paf "paf" ]

let () =
  register "__blog"
    ~packages
    [ __blog $ console $ random $ mclock $ pclock $ time $ stack ]

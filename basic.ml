open Now

type eval_result = Success | Error of string
type syntax = Reason | OCaml

module Repl = struct
  let std_fmt = Format.std_formatter

    let noop_fmt = Format.make_formatter (fun _ _ _ -> ()) ignore

    let init_toploop () =
      (* Topfind.add_predicates ["byte"; "toploop"] ; *)
      (* Add findlib path so Topfind is available and it won't be
           initialized twice if the user does [#use "topfind"]. *)
      (* Topdirs.dir_directory (Findlib.package_directory "findlib") ; *)
      Toploop.initialize_toplevel_env ()

    let eval ?(fmt = std_fmt) ~syntax str =
      try
        let lex = Lexing.from_string str in
        let tpl_phrases =
          match syntax with
          | OCaml | Reason -> Parse.use_file lex
          (*| Reason ->*)
              (*List.map Reason_toolchain.To_current.copy_toplevel_phrase*)
                (*(Reason_toolchain.RE.use_file lex)*)
        in
        let exec phr =
          if Toploop.execute_phrase true fmt phr then Success
          else Error "No result"
        in
        let rec execAll phrases =
          match phrases with
          | [] -> Error "No result"
          | [phr] -> exec phr
          | phr :: next -> (
            let ret = exec phr in
            match ret with Error _ -> ret | _ -> execAll next )
            in
        execAll tpl_phrases
            with
      | Syntaxerr.Error _ -> Error "Syntax Error occurred"
      (*| Reason_syntax_util.Error _ -> Error "Reason Parsing Error"*)
      | _ -> Error ("Error while exec: " ^ str)
end

let eval_and_check_result body =
  let buf = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buf in
  begin match Repl.eval ~fmt:formatter ~syntax:OCaml body with
  | Success ->
    Buffer.contents buf
  | Error err -> err
  end

let my_handler reqd _context =
  let body = Reqd.request_body reqd in
  let result =
    match body with
    | Some body -> eval_and_check_result body
    | None ->
      "Error: HTTP body not present. Pass OCaml code to evaluate in the HTTP body"
  in
  let response =
    Response.create
      ~headers:(Headers.of_list [ "Content-Type", "application/json" ])
      `OK
  in
  Reqd.respond_with_string reqd response result

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_log (Some Logs.Debug);
  Repl.init_toploop ();
  Now.lambda my_handler

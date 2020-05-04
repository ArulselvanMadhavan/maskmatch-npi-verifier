open Lwt
open Cohttp
open Cohttp_lwt_unix
open Npi_response
open Yojson.Safe

let body =
  Client.get (Uri.of_string "https://npiregistry.cms.hhs.gov/api/?version=2.1&number=1528427689&pretty=true") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  let npi_resp = Npi_j.npi_response_of_string body in
  prettify @@ Npi_j.string_of_npi_response npi_resp

let () =
  let body = Lwt_main.run body in
  let npi_emacs_re = Re.Perl.re "\\b[0-9]{10}?" in
  let npi_re = Re.Perl.compile npi_emacs_re in
  let all_matches = Re.matches npi_re "some 0123456789 work1231231231" in
  List.iter (print_endline) all_matches;
  print_endline ("Received body\n" ^ body)

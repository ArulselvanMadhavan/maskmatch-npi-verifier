open Lwt
open Cohttp
open Cohttp_lwt_unix
open Npi_response
open Cmdliner

let validate_npi npi_str =
  let uri_str = Printf.sprintf "https://npiregistry.cms.hhs.gov/api/?version=2.1&number=%s&pretty=true" npi_str in
  Client.get (Uri.of_string uri_str) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  print_endline @@ Printf.sprintf "NPI - Str %s \n body %s" npi_str body;
  let npi_resp = Npi_j.npi_response_of_string body in
  npi_resp


let extract_npi_from_string notes =
  let npi_emacs_re = Re.Perl.re "\\b[0-9]{10}?" in
  let npi_re = Re.Perl.compile npi_emacs_re in
  let all_matches = Re.matches npi_re notes in
  all_matches

let select_first_npi matches =
  Base.List.hd matches

let validate_row (npi:string) (row:Csv.Row.t) (npi_resp:Npi_response.Npi_t.npi_response) =
  let row_name = Csv.Row.get row 4 in
  let open Base.Option in
  Base.List.hd npi_resp.results >>| fun result ->
  let result_name = result.basic.name in
  let result_npi = result.number in
  let input_npi = Base.Int.of_string npi in
  row_name = result_name && result_npi = input_npi

let process_row row =
  let validation_data_field = (Csv.Row.get row 7) in
  let npi_opt = select_first_npi @@ extract_npi_from_string validation_data_field in
  let validation_result = Base.Option.(npi_opt >>= (fun npi ->
                                let npi_result_lwt = validate_npi npi in
                                let npi_result = Lwt_main.run npi_result_lwt in
                                let validation_result = validate_row npi row npi_result in
                                validation_result)) in
  let final_result = Base.Option.fold validation_result ~init:false ~f:(fun acc a -> acc || a) in
  if final_result then
    print_endline @@ Printf.sprintf "Validation Succeeded for %s" @@ validation_data_field
  else
    print_endline @@ Printf.sprintf "Validation failed for %s" @@ validation_data_field

let parse_csv file_path =
  let in_c = open_in file_path in
  let csv_inc = Csv.of_channel in_c in
  Csv.Rows.iter ~f:process_row csv_inc

let file_path =
  let doc = "csv file to parse" in
  let env = Arg.env_var "CSV_FILE_PATH" ~doc in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

let csv_t =
  Term.(const parse_csv $ file_path)

let () =
  (* let body = Lwt_main.run body in *)
  (* print_endline ("Received body\n" ^ body); *)
  Term.exit @@ Term.eval (csv_t, Term.info "npi-validator")

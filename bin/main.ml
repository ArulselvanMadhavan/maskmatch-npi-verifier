open Lwt
open Cohttp
open Cohttp_lwt_unix
open Npi_response
open Cmdliner

let validate_npi npi_str =
  let uri_str =
    Printf.sprintf
      "https://npiregistry.cms.hhs.gov/api/?version=2.1&number=%s&pretty=true"
      npi_str
  in
  Client.get (Uri.of_string uri_str)
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "NPI: %s Response code: %d\n" npi_str code;
  if code = 200
  then
    body
    |> Cohttp_lwt.Body.to_string
    >|= fun body ->
    let npi_resp = Npi_j.npi_response_of_string body in
    Some npi_resp
  else Lwt.return_none


let extract_npi_from_string notes =
  let npi_emacs_re = Re.Perl.re "\\b[0-9]{10}?" in
  let npi_re = Re.Perl.compile npi_emacs_re in
  let all_matches = Re.matches npi_re notes in
  all_matches


let select_first_npi matches = Base.List.hd matches

let extract_name (basic : Npi_t.basic option) =
  let open Base.Option in
  basic
  >>= fun b ->
  Base.Option.first_some b.first_name b.authorized_official_first_name
  >>= fun fn ->
  Base.Option.first_some b.last_name b.authorized_official_last_name
  >>| fun ln -> Base.String.lowercase @@ Printf.sprintf "%s %s" fn ln


let validate_row
    (npi : string)
    (row : Csv.Row.t)
    (npi_resp : Npi_response.Npi_t.npi_response)
  =
  let row_name = Base.String.lowercase @@ Csv.Row.get row 4 in
  let org_name = Base.String.lowercase @@ Csv.Row.get row 6 in
  let resp_orig = Base.Int.of_string @@ Csv.Row.get row 12 in
  let surg_orig = Base.Int.of_string @@ Csv.Row.get row 13 in
  let open Base.Option in
  Base.List.hd npi_resp.results
  >>| fun result ->
  let result_name = extract_name result.basic in
  let result_npi = result.number in
  let input_npi = Base.Int.of_string npi in
  let row_name_equal =
    Base.Option.fold result_name ~init:false ~f:(fun acc s -> row_name = s || acc)
  in
  let org_name_equal = row_name = org_name in
  let count_limit = resp_orig + surg_orig < 500 in
  let npi_equal = result_npi = input_npi in
  if row_name_equal = false && org_name_equal = false
  then
    print_endline
    @@ Printf.sprintf
         "given_name:%s\tresult_name:%s\torg_name:%s"
         row_name
         (Base.Option.fold result_name ~init:"NOTHING IN RESULT" ~f:(fun _ s -> s))
         org_name;
  if count_limit = false
  then
    print_endline
    @@ Printf.sprintf
         "Count_limit Invalidation resp_orig: %d\t surg_orig: %d"
         resp_orig
         surg_orig;
  if npi_equal = false
  then print_endline @@ Printf.sprintf "NPI MISMATCH: %d\t%d" result_npi input_npi;
  (row_name_equal || org_name_equal) && count_limit && npi_equal


let process_row (csv_outc : Csv.out_channel) row =
  let validation_data_field = Csv.Row.get row 7 in
  let npi_opt = select_first_npi @@ extract_npi_from_string validation_data_field in
  let validation_result =
    Base.Option.(
      npi_opt
      >>= fun npi ->
      let npi_result_lwt = validate_npi npi in
      let npi_result_opt = Lwt_main.run npi_result_lwt in
      let validation_result =
        npi_result_opt >>= fun npi_result -> validate_row npi row npi_result
      in
      validation_result)
  in
  let final_result = Base.Option.fold validation_result ~init:false ~f:(fun _ a -> a) in
  if final_result
  then (
    let _ = Csv.output_record csv_outc (Csv.Row.to_list row) in
    print_endline (Printf.sprintf "Validation Succeeded for %s" @@ validation_data_field))
  else if Base.Option.is_some npi_opt
  then
    Base.Option.iter npi_opt ~f:(fun npi_str ->
        print_endline @@ Printf.sprintf "!!!Validation failed: %s" npi_str)
  else ()


let parse_csv input_file_path output_file_path =
  let in_c = open_in input_file_path in
  let out_c = open_out_gen [ Open_wronly; Open_creat ] 0o666 output_file_path in
  let csv_inc = Csv.of_channel ~has_header:true in_c in
  let csv_outc = Csv.to_channel out_c in
  Csv.Rows.iter ~f:(process_row csv_outc) csv_inc


let input_file_path =
  let doc = "csv file to parse" in
  let env = Arg.env_var "CSV_INPUT_FILE_PATH" ~doc in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"INPUT_CSV_FILE_PATH" ~doc)


let output_file_path =
  let doc = "output file to write to" in
  let env = Arg.env_var "CSV_OUTPUT_FILE_PATH" ~doc in
  let doc = "The message to print." in
  Arg.(value & pos 1 string "Revolt!" & info [] ~env ~docv:"OUTPUT_CSV_FILE_PATH" ~doc)


let csv_t = Term.(const parse_csv $ input_file_path $ output_file_path)

let () =
  Term.exit @@ Term.eval (csv_t, Term.info "maskmatch-npi-validator")


let input_char_opt ic =
  try Some(input_char ic)
  with End_of_file -> None

let input_file filename =
  let ic = open_in filename in
  let rec aux cc =
    match input_char_opt ic with
    | Some c -> aux (c::cc)
    | None -> close_in ic; String.concat "" (List.map (String.make 1) (List.rev cc))
  in
  aux []

let split_langs s =
  if String.contains s '-' then
  begin
    match String.split_on_char '-' s with
    | lng1::lng2::[] -> (lng1, lng2)
    | _ -> failwith "lang-split"
  end
  else failwith "lang-split"

(*
# String.escaped "—";;
- : string = "\226\128\148"
*)

let is_ascii_txt str =
  let n = String.length str in
  let rec aux c =
    if c >= n then true else
    let c1 = String.get str c in
    match c1 with
    | 'A'..'Z'
    | 'a'..'z' -> aux (c+1)
    | '-' | ';'
    | '.' | ',' | '\r' | '\n' | '\t'
    | ' ' -> aux (c+1)
    | _ -> false
  in
  aux 0

(*
let opt_prev c1 =
  match c1 with
  | Some c1 -> (Printf.sprintf "[some:%s]" (String.escaped (String.make 1 c1)))
  | None -> "[none]"
*)

let is_u8_txt txt =
  (*
  Printf.printf "# '%s'\n%!" (String.escaped txt);
  *)
  let n = String.length txt in
  let rec aux c_prev c =
    if c >= n then true else
    let c1 = String.get txt c in
    (*
    Printf.printf "# %d: prev:%s '%s'\n%!" c (opt_prev c_prev) (String.escaped (String.make 1 c1));
    *)
    match c_prev, c1 with
    | None, '\197'
    | None, '\196'
    | None, '\195'
    | None, '\204'
    | None, '\208'
    | None, '\209' -> aux None (c+2)
    | (Some '\197'), _
    | (Some '\196'), _
    | (Some '\195'), _
    | (Some '\204'), _
    | (Some '\208'), _
    | (Some '\209'), _ -> aux None (c+2)
    | (_, ('A'..'Z'))
    | (_, ('a'..'z')) -> aux None (c+1)
    | (_, '-')
    | (_, '.')
    | (_, ',')
    | (_, ';')
    | (_, '\t')
    | (_, '\r')
    | (_, '\n')
    | (_, ' ') -> aux None (c+1)
    | _ -> false
  in
  aux None 0

let c_esc c =
  match c with
  | Some c1 -> c1
  | None -> '\000'

let txt_to_sent_u txt_in =
  let n = String.length txt_in in
  let rec aux c_prev c cc_wrd cc_wrds =
    if c >= n then
    begin
      if cc_wrd <> []
      then
        let word = String.concat "" (List.map (String.make 1) (List.rev cc_wrd)) in
        List.rev(word::cc_wrds)
      else List.rev(cc_wrds)
    end else
    let c1 = String.get txt_in c in
    (*
    Printf.printf "# %d: %s %s\n%!" c (opt_prev c_prev) (String.escaped (String.make 1 c1));
    *)
    match c_prev, c1 with
    | None, 'A'..'Z' ->
        let c0 = char_of_int ((int_of_char c1) lor 0b00100000) in
        aux None (c+1) (c0::cc_wrd) cc_wrds

    | None, 'a'..'z' -> aux None (c+1) (c1::cc_wrd) cc_wrds

    | (Some '\197'), _
    | (Some '\196'), _
    | (Some '\195'), _
    | (Some '\204'), _
    | (Some '\208'), _
    | (Some '\209'), _ ->
        aux None (c+1) (c1::(c_esc c_prev)::cc_wrd) cc_wrds

    | None, ' ' ->
        let word = String.concat "" (List.map (String.make 1) (List.rev cc_wrd)) in
        aux None (c+1) [] (word::cc_wrds)

    | None, '\197'
    | None, '\196'
    | None, '\195'
    | None, '\204'
    | None, '\208'
    | None, '\209' -> aux (Some c1) (c+1) cc_wrd cc_wrds

    | (Some _), _
    | None, _ -> aux None (c+1) cc_wrd cc_wrds
  in
  aux None 0 [] []

let txt_to_sent txt_in =
  let n = String.length txt_in in
  let rec aux c cc_wrd cc_wrds =
    if c >= n then
    begin
      if cc_wrd <> []
      then
        let word = String.concat "" (List.map (String.make 1) (List.rev cc_wrd)) in
        List.rev(word::cc_wrds)
      else List.rev(cc_wrds)
    end else
    let c1 = String.get txt_in c in
    match c1 with
    | 'A'..'Z' ->
        let c0 = char_of_int ((int_of_char c1) lor 0b00100000) in
        aux (c+1) (c0::cc_wrd) cc_wrds

    | 'a'..'z' -> aux (c+1) (c1::cc_wrd) cc_wrds
    | ' ' ->
        let word = String.concat "" (List.map (String.make 1) (List.rev cc_wrd)) in
        aux (c+1) [] (word::cc_wrds)
    | _ -> aux (c+1) cc_wrd cc_wrds
  in
  aux 0 [] []

let m () =
  let txt_in = {|
Pewnego bardzo ciepłego wieczoru, o godzinie siódmej, zbudził się Ojciec Wilk,
zażywający całodziennego wypoczynku wśród wzgórz Seeonee. Podrapał się, ziewnął
i zaczął wyciągać jedną łapę po drugiej, chcąc pozbyć się sennego odrętwienia,
jakie wyczuwał jeszcze w koniuszkach pazurów. Mama-Wilczyca leżała na ziemi,
zwiesiwszy wielki, szary nochal ponad czwórką szamoczących się z sobą i
popiskujących wilczątek, a blask księżyca zaglądał w otwór jaskini, która była
mieszkaniem całej tej gromadki.
  |} in
  Printf.printf "# s:%s\n%!" ( txt_in);
  Printf.printf "# is_ascii:%b\n%!" (is_ascii_txt txt_in);
  Printf.printf "# is_u8_txt:%b\n%!" (is_u8_txt txt_in);
;;

let m () =
  let txt_in = "тихо сказал князь Андрей" in
  Printf.printf "# s:%s\n%!" ( txt_in);
  Printf.printf "# is_ascii:%b\n%!" (is_ascii_txt txt_in);
  Printf.printf "# is_u8_txt:%b\n%!" (is_u8_txt txt_in);
;;

let m () =
  let txt_in = "said prince andrew gently" in
  Printf.printf "# s:%s\n%!" ( txt_in);
  Printf.printf "# is_ascii:%b\n%!" (is_ascii_txt txt_in);
;;

let m () =
  let txt_in = "тихо сказал князь Андрей" in
  print_endline txt_in;
  (*
  Printf.printf "%s\n%!" (String.escaped txt_in);
  *)
  let sent =
    if is_ascii_txt txt_in
    then txt_to_sent txt_in
    else txt_to_sent_u txt_in
  in
  List.iter (fun word ->
    Printf.printf "#w:%s\n%!" word;
  ) sent;
;;

let m () =
  let txt_in = "тихо сказал князь Андрей" in
  let sent = txt_to_sent txt_in in
  List.iter (fun word ->
    Printf.printf "#w:%s\n%!" word;
  ) sent;
;;

let m () =
  let txt_in = "said prince andrew gently" in
  let sent = txt_to_sent txt_in in
  List.iter (fun word ->
    Printf.printf "#w:%s\n%!" word;
  ) sent;
;;

let () =
  let dict_file = "./test5.se" in
  let se = SExpr.parse_file dict_file in
  let lng1, lng2 = split_langs "ru-en" in
  Printf.printf "= %s %s\n%!" lng1 lng2;
  Printf.printf "\n%!";

  (*
  let txt_in = input_file "./tr5.rev" in
  let sent = txt_to_sent txt_in in
  *)

  let txt_in = input_file "./tr5.rev" in
  let sent =
    if is_ascii_txt txt_in
    then txt_to_sent txt_in
    else txt_to_sent_u txt_in
  in

  (*
  let sent = "said"::"prince"::"andrew"::"gently"::[] in
  *)

  let aux_word_2 se =
    match se with
    | SExpr.Expr ((SExpr.Atom lg)::(SExpr.Atom word_lg)::[]) -> Some(lg, word_lg)
    | SExpr.Expr _ | SExpr.Atom _ -> None
  in
  let aux_word se =
    match se with
    | SExpr.Expr ((SExpr.Atom "word")::se_word) ->
        let ass_word = List.filter_map aux_word_2 se_word in
        (ass_word)
    | SExpr.Expr _ | SExpr.Atom _ -> failwith "read-expr"
  in
  let rec aux se cc =
    match se with
    | se::se_lst ->
        let ass_word = aux_word se in
        aux se_lst (ass_word::cc)
    | [] -> (cc)
  in
  let ass_words = aux se [] in
  List.iter (fun word ->
    Printf.printf "# %s\n%!" word;
    List.iter (fun ass_word ->
      let _wrd = ref "" in
      let found = ref false in
      List.iter (fun (lg, wd) ->
        if (lg, wd) = (lng1, word) then found := true
      ) ass_word;
      if !found then begin
        List.iter (fun (lg, wd) ->
          if lg = lng2 then _wrd := wd
        ) ass_word;
      end;
      if !_wrd <> "" then Printf.printf "> %s\n%!" ( !_wrd);
    ) ass_words;
    Printf.printf "\n%!";
  ) sent;
;;


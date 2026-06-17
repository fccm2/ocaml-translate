
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
    | 'a'..'z' -> aux (c+1) (c1::cc_wrd) cc_wrds
    | ' ' ->
        let word = String.concat "" (List.map (String.make 1) (List.rev cc_wrd)) in
        aux (c+1) [] (word::cc_wrds)
    | _ -> aux (c+1) cc_wrd cc_wrds
  in
  aux 0 [] []

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
  let lng1, lng2 = split_langs "en-ru" in
  Printf.printf "> %s %s\n%!" lng1 lng2;

  let txt_in = input_file "./tr5.in" in
  let sent = txt_to_sent txt_in in

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
        if (lg, wd) = ("en", word) then found := true
      ) ass_word;
      if !found then begin
        List.iter (fun (lg, wd) ->
          if lg = "ru" then _wrd := wd
        ) ass_word;
      end;
      if !_wrd <> "" then Printf.printf "# %s\n%!" ( !_wrd);
    ) ass_words;
    Printf.printf "\n%!";
  ) sent;
;;


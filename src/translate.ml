let split_langs s =
  if String.contains s '-' then
  begin
    match String.split_on_char '-' s with
    | lng1::lng2::[] -> (lng1, lng2)
    | _ -> failwith "lang-split"
  end
  else failwith "lang-split"

let m () =
  let s = "fr-en" in
  let lng1, lng2 = split_langs s in
  Printf.printf "# %s %s\n%!" lng1 lng2;
;;

let () =
  let dict_file = "./test5.se" in
  let se = SExpr.parse_file dict_file in
  let translate = ("fr", "en") in
  let _ = translate in
  let sent = "said"::"prince"::"andrew"::"gently"::[] in
  let _ = sent in
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
        (* Printf.printf "# %s %s\n%!" lg wd; *)
        if (lg, wd) = ("en", word) then found := true
      ) ass_word;
      if !found then begin
        List.iter (fun (lg, wd) ->
          if lg = "ru" then _wrd := wd
        ) ass_word;
      end;
      if !_wrd <> "" then Printf.printf "# %s\n%!" !_wrd;
    ) ass_words;
    Printf.printf "\n%!";
  ) sent;
;;


open Format
open Lexing

let () = 

  if Array.length Sys.argv <> 2
  then
    begin
      eprintf "usage: expr2json.exe <file.c> @?" ;
      exit(1)
    end ;
  
  if not (Filename.check_suffix Sys.argv.(1) ".c")
  then
    begin
      eprintf "Le fichier n'a pas l'extension .c !@?"
    end ;
  let fichier_entree = Sys.argv.(1)  in
  let fichier_sortie = Filename.chop_suffix fichier_entree ".c" ^ ".json" in

  (* localise une erreur en indiquant la ligne et la colonne *)
  let localisation pos =
    let l = pos.pos_lnum in
    let c = pos.pos_cnum - pos.pos_bol + 1 in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" fichier_entree l (c-1) c
  in
  
  let f = open_in fichier_entree in
  let buf = Lexing.from_channel f in
  
  try
    let p = Parser.prog Lexer.token buf in
    close_in f;
    Ast.toJSON p |> Yojson.to_file fichier_sortie
  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On récupère sa position absolue et 
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c@." c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
	





open Ast;;
open Ast_util;;

open Lexer;;
open Parser;;


let main =
  let channel = In_channel.open_text "test.sc" in
  let lb = Lexing.from_channel channel in
  Parser.program Lexer.read_token lb |> string_of_program |> print_endline;;
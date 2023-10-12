open Printf

(* determine whether a string contains a specific string *)
let contains_substring str substring =
    let substring_length = String.length substring in
    let rec loop i =
        if i + substring_length > String.length str then
          false
        else if String.sub str i substring_length = substring then
          true
        else
          loop (i + 1)
    in
        loop 0

(* new cont lines *)
let count_lines filename =
    let ic = open_in filename in
    let rec count_lines_helper count empty_count comment_count in_comment =
        try
          let line = input_line ic in
          let trimmed_line = String.trim line in
              if in_comment then
                if contains_substring trimmed_line "*/" then
                  count_lines_helper count empty_count (comment_count + 1) false
                else
                  count_lines_helper count empty_count (comment_count + 1)
                    in_comment
              else if
                contains_substring trimmed_line "//"
                || contains_substring trimmed_line "/*"
                   && contains_substring trimmed_line "*/"
              then
                count_lines_helper count empty_count (comment_count + 1)
                  in_comment
              else if contains_substring trimmed_line "/*" then
                count_lines_helper count empty_count (comment_count + 1) true
              else if String.length trimmed_line = 0 then
                count_lines_helper count (empty_count + 1) comment_count
                  in_comment
              else
                count_lines_helper (count + 1) empty_count comment_count
                  in_comment
        with End_of_file ->
          close_in ic;
          (filename, count, empty_count, comment_count)
    in
        count_lines_helper 0 0 0 false

(* old count lines *)
(* let count_lines filename =
    let ic = open_in filename in
    let rec count_lines_helper count empty_count comment_count in_comment =
        try
          let line = input_line ic in
          let trimmed_line = String.trim line in
              if String.length trimmed_line = 0 then
                count_lines_helper count (empty_count + 1) comment_count in_comment
              else if in_comment then
                let end_comment_index =
                    try String.index trimmed_line '*' with Not_found -> -1
                in
                    if
                      end_comment_index >= 0
                      && end_comment_index < String.length trimmed_line - 1
                      && String.sub trimmed_line end_comment_index 2 = "*/"
                    then
                      count_lines_helper count empty_count (comment_count + 1)
                        false
                    else
                      count_lines_helper count empty_count (comment_count + 1)
                        true
              else if
                String.length trimmed_line >= 2
                && String.sub trimmed_line 0 2 = "//"
              then
                count_lines_helper count empty_count (comment_count + 1)
                  in_comment
              else if
                String.length trimmed_line >= 2
                && String.sub trimmed_line 0 2 = "/*"
              then
                let end_comment_index =
                    try String.index trimmed_line '*' with Not_found -> -1
                in
                    if
                      end_comment_index >= 0
                      && end_comment_index < String.length trimmed_line - 1
                      && String.sub trimmed_line end_comment_index 2 = "*/"
                    then
                      count_lines_helper count empty_count (comment_count + 1)
                        false
                    else
                      count_lines_helper count empty_count (comment_count + 1)
                        true
              else
                count_lines_helper (count + 1) empty_count comment_count
                  in_comment
        with End_of_file ->
          close_in ic;
          (filename, count, empty_count, comment_count)
    in
        count_lines_helper 0 0 0 false *)

(* print usage *)
let print_usage =
    print_endline "Unknown!";
    print_endline "Usage: Vrosi [f/d] [files/directories]"

(* print a file info *)
let count_and_print_file_info filename =
    let filepath = Printf.sprintf "%s" filename in
    let name, code_lines, empty_lines, comment_lines = count_lines filepath in
        printf "File: %s\n" name;
        printf "Code Lines: %d\n" code_lines;
        printf "Empty Lines: %d\n" empty_lines;
        printf "Comment Lines: %d\n" comment_lines;
        printf "\n"

(* recursively traverse a directory and apply a handler to each file *)
let rec traverse_directory path handler =
    let files = Sys.readdir path in
        Array.iter
          (fun file ->
            let full_path = Filename.concat path file in
                if Sys.is_directory full_path then
                  traverse_directory full_path handler
                else
                  handler full_path)
          files

(* handle command line agruments *)
(* arccording to different options, choose different way to handle *)
let process_command_line_args args =
    match Array.length args with
    | n when n < 3 -> print_usage
    | _ -> (
        let option = args.(1) in
            match option with
            | "f" ->
                let filenames = Array.sub args 2 (Array.length args - 2) in
                    Array.iter count_and_print_file_info filenames
            | "d" ->
                let directories = Array.sub args 2 (Array.length args - 2) in
                    Array.iter
                      (fun directory ->
                        traverse_directory directory count_and_print_file_info)
                      directories
            | _ -> print_usage)

(* entry point *)
let () =
    let args = Sys.argv in
        process_command_line_args args

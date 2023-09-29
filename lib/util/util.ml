open Printf
open Unix

type line_flag =
    | Blank
    | Code
    | SingleComment
    | MutiCommentBegin
    | MutiCommentIn
    | MutiCommentEnd

let write_file file msg =
    let oc = open_out file in
        fprintf oc "%s\n" msg;
        close_out oc

let print_file file =
    let ic = open_in file in
        try
          let line = input_line ic in
              print_endline line;
              close_in ic
        with e ->
          close_in_noerr ic;
          raise e

(* 递归获取目录下的所有子目录和文件 *)
let rec get_all_files dir =
    let handle_entry entry =
        match entry with
        | "." | ".." -> []
        | entry_name ->
            let full_path = Filename.concat dir entry_name in
                if Sys.is_directory full_path then
                  get_all_files full_path
                else
                  [full_path]
    in
    let dir_handle = opendir dir in
    let entries = ref [] in
        try
          while true do
            let entry = readdir dir_handle in
                entries := entry :: !entries
          done;
          []
        with End_of_file ->
          closedir dir_handle;
          List.concat (List.map handle_entry !entries)

(* 判断是否是空行 *)
let is_blank_line line_str = String.trim line_str = ""

(* 判断是否是单行注释 *)
(* 对于一行中只要是有注释，那就算是单行注释 *)
let is_single_comment_line line_str =
    let line_str_trimmed = String.trim line_str in
    let len = String.length line_str_trimmed in
        len >= 2
        && String.contains line_str_trimmed '/'
        && String.get line_str_trimmed (String.index line_str_trimmed '/') = '/'

(* 判断是否是代码行 *)
let is_code_line line_str = not (is_blank_line line_str)

(* 判断是否是多行注释开始 *)
let is_block_comment_start_line line_str =
    String.length line_str >= 2 && String.sub line_str 0 2 = "/*"

(* 判断是否是多行注释结束 *)
let is_block_comment_end_line line_str =
    String.length line_str >= 2
    && String.sub line_str (String.length line_str - 2) 2 = "*/"

(* 统计一个文件中的各种行数 *)
let count_file file_str =
    let lines = String.split_on_char '\n' file_str in
    let rec count_lines_helper comment_count empty_count code_count lines =
        match lines with
        | [] -> (comment_count, empty_count, code_count)
        | line :: rest ->
            let trimmed_line = String.trim line in
            (* 调用其他函数进行计数 *)
            let is_blank = is_blank_line trimmed_line in
            let is_single_comment = is_single_comment_line trimmed_line in
            let is_code = is_code_line trimmed_line in
            let is_block_comment_start =
                is_block_comment_start_line trimmed_line
            in
            let is_block_comment_end = is_block_comment_end_line trimmed_line in
                if is_blank then
                  count_lines_helper comment_count (empty_count + 1) code_count rest
                else if is_single_comment then
                  count_lines_helper (comment_count + 1) empty_count code_count rest
                else if is_code then
                  count_lines_helper comment_count empty_count (code_count + 1) rest
                else if is_block_comment_start then
                  count_lines_helper (comment_count + 1) empty_count code_count rest
                else if is_block_comment_end then
                  count_lines_helper (comment_count + 1) empty_count code_count rest
                else
                  (* count_comments, count_empty, count_code 是计数结果 *)
                  let count_comments, count_empty, count_code =
                      count_lines_helper 0 0 0 [trimmed_line]
                  in
                      count_lines_helper
                        (comment_count + count_comments)
                        (empty_count + count_empty)
                        (code_count + count_code) rest
    in
    let comment_count, empty_count, code_count =
        count_lines_helper 0 0 0 lines
    in
        (comment_count, empty_count, code_count)

let print_all_files dir =
    let files = get_all_files dir in
        List.iter
          (fun file ->
            printf "file name:%s\n" file;
            print_file file)
          files

let count_lines file =
    let ic = open_in file in
    let rec count_lines_helper comment_count empty_count code_count
        in_block_comment in_doc_comment =
        try
          let line = input_line ic in
          let trimmed_line = String.trim line in
              if String.length trimmed_line = 0 then
                count_lines_helper comment_count (empty_count + 1) code_count
                  in_block_comment in_doc_comment
              else if in_block_comment then
                if
                  String.length trimmed_line >= 2
                  && String.sub trimmed_line (String.length trimmed_line - 2) 2
                     = "*/"
                then
                  count_lines_helper comment_count empty_count code_count false
                    in_doc_comment
                else
                  count_lines_helper comment_count empty_count code_count true
                    in_doc_comment
              else if in_doc_comment then
                if
                  String.length trimmed_line >= 2
                  && String.sub trimmed_line (String.length trimmed_line - 2) 2
                     = "*/"
                then
                  count_lines_helper comment_count empty_count code_count false
                    false
                else
                  count_lines_helper (comment_count + 1) empty_count code_count
                    true in_doc_comment
              else if
                String.length trimmed_line >= 2
                && String.sub trimmed_line 0 2 = "//"
              then
                count_lines_helper (comment_count + 1) empty_count code_count
                  in_block_comment in_doc_comment
              else if
                String.length trimmed_line >= 2
                && String.sub trimmed_line 0 2 = "/*"
              then
                if
                  String.length trimmed_line >= 4
                  && String.sub trimmed_line (String.length trimmed_line - 2) 2
                     = "*/"
                then
                  count_lines_helper (comment_count + 1) empty_count code_count
                    in_block_comment in_doc_comment
                else
                  count_lines_helper (comment_count + 1) empty_count code_count
                    true in_doc_comment
              else
                count_lines_helper comment_count empty_count (code_count + 1)
                  in_block_comment in_doc_comment
        with
        | End_of_file -> (comment_count, empty_count, code_count)
        | exn ->
            close_in_noerr ic;
            raise exn
    in
    let comment_count, empty_count, code_count =
        count_lines_helper 0 0 0 false false
    in
        close_in ic;
        (comment_count, empty_count, code_count)

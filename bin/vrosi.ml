open Printf
open Str

let filename = "test.txt"
let message = "Hello!"

let match_regex pattern str =
    let regex = regexp pattern in
        try
          ignore (search_forward regex str 0);
          true
        with Not_found -> false

let () =
    let args_num = Array.length Sys.argv - 1 in
        Printf.printf "Number of arguments:%i\n" args_num;
        (* 遍历命令行参数 *)
        for i = 1 to args_num do
          Printf.printf "Argument %i:%s\n" i Sys.argv.(i)
        done;
        Util.write_file filename message;
        Util.print_file filename;
        Util.print_all_files ".";
        let t = match_regex "123" "?2?" in
            printf "%b" t

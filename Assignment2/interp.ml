(* $Id: interp.ml,v 1.9 2020-01-28 13:33:00-08 - - $ *)

open Absyn
open Tables

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with
        | Variable ident -> 
            Hashtbl.find Tables.variable_table ident
        | Arrayref (ident, expr) -> 
            Array.get (Hashtbl.find Tables.array_table ident) 
                (Float.to_int (eval_expr expr)))
    | Unary (oper, expr) -> let value = eval_expr expr 
                                in Hashtbl.find
                                    Tables.unary_fn_table oper value
    | Binary (oper, expr1, expr2) -> let value1 = eval_expr expr1 in
                                     let value2 = eval_expr expr2
                                            in Hashtbl.find 
                                                Tables.binary_fn_table
                                                oper value1 value2

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim (ident, expr) continuation
    | Let (memref, expr) -> interp_let (memref, expr) continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> interp_if (expr, label) continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation

and interp_dim (ident, expr)
                 (continuation : Absyn.program) =
    Hashtbl.add Tables.array_table ident
        (Array.make (Float.to_int (eval_expr expr)) 0.0);
    interpret continuation

and interp_let (memref, expr)
                 (continuation : Absyn.program) = match memref with
    | Variable ident -> (Hashtbl.add 
        Tables.variable_table ident (eval_expr expr); 
            (interpret continuation) )
    | Arrayref (ident, x) ->
        try let value = Hashtbl.find Tables.array_table ident
                            in (Array.set value 
                                (Float.to_int (eval_expr x))
                                (eval_expr expr); 
                                (interpret continuation))
        with Not_found -> (exit 1);

and interp_goto label
                 (continuation : Absyn.program) =
    try let value = Hashtbl.find Tables.label_table label 
        in interpret value
    with Not_found -> (exit 1);

and interp_if (expr, label)
                 (continuation : Absyn.program) = match expr with
    | Binary (oper, expr1, expr2) -> 
        try let value = Hashtbl.find Tables.boolean_fn_table oper in
            if value (eval_expr expr1) (eval_expr expr2)
                then (try let value = 
                    Hashtbl.find Tables.label_table label 
                          in interpret value
                      with Not_found -> (exit 1))
                else interpret continuation
        with Not_found -> (exit 1);
    | _ -> (exit 1)

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ());
    interpret continuation

and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in match memref with 
             | Variable ident -> (Hashtbl.add 
               Tables.variable_table ident number)
        with End_of_file ->
             (print_string "End_of_file"; (Hashtbl.add 
               Tables.variable_table "eof" 1.); print_newline ())
    in List.iter input_number memref_list;
    interpret continuation

let interpret_program program =
    (Tables.init_label_table program;
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
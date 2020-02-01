(* $Id: interp.ml,v 1.9 2020-01-28 13:33:00-08 - - $ *)

open Absyn
open Tables

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> match memref with
        | Variable variable -> 
            Hashtbl.find Table.variable_table variable
        | Arrayref arrayref::x::xs -> 
            Hashtbl.find Table.array_table x xs
    | Unary (oper, expr) -> let value = eval_expr expr 
                                in Hashtbl.find
                                    Tables.unary_fn_table oper value
    | Binary (oper, expr1, expr2) -> let value1 = eval_expr expr1
                                         value2 = eval_expr expr2
                                            in Hashtbl.find 
                                                Tables.binary_fn_table
                                                oper value1 value2
    | Boolean (oper, expr1, expr2) -> let value1 = eval_expr expr1 
                                          value2 = eval_expr expr2 
                                            in Hashtbl.find
                                                Tables.boolean_fn_table 
                                                oper value1 value2

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

let rec interpret_labels (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret_labels continuation
      | _, _, Some label -> ((Hashtbl.add 
                                Table.label_table label program)
                            (interpret_labels continuation))
      | _, _, Some stmt -> interpret_labels continuation

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
        (Array.make (to_int(eval_expr expr)) 0.0)
    interpret continuation

and interp_let (memref, expr)
                 (continuation : Absyn.program) = match memref with
    | Variable variable -> Hashtbl.add
        Table.variable_table variable (eval_expr expr)
    | Arrayref arrayref::x::xs -> 
        try let value = Hashtbl.find Table.array_table x
                                      in Array.set value xs 
                                          (eval_expr expr)
                                  with Not_found -> (exit 1)
    interpret continuation

and interp_goto label
                 (continuation : Absyn.program) = 
    try let value = Hashtbl.find Table.label_table label
        interpret value
    with Not_found -> (exit 1)
    interpret continuation

and interp_if (expr::x::xs, label)
                 (continuation : Absyn.program) =
    try let value = Hashtbl.find Table.function_table expr
        if value x xs
        then interp_goto label
    with Not_found -> (exit 1)
    interpret continuation

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
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list;
    interpret continuation

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret_labels program
     interpret program)

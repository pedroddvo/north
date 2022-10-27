open North
open North.Ast
open Printf

let list_zip a b =
  Seq.zip (List.to_seq a) (List.to_seq b)

let hashtable_of_list lst =
  let tbl = Hashtbl.create (List.length lst) in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) lst;
  tbl

let int_of_bool = function
| true -> 1
| false -> 0

let _list_contains lst e =
  match List.find_opt (fun x -> x = e) lst with
  | Some _ -> true
  | None -> false

let exact_match a p =
  match (a, p) with
  | Int p, Int a -> a = p
  | Boolean p, Boolean a -> a = p
  | Symbol _, _ -> true
  | _ -> false

let priority_sort a b =
  match (a, b) with
  | Symbol _, Symbol _ -> 0
  | _, Symbol _ -> -1
  | Symbol _, _ -> 1
  | _ -> 0

let parse s =
  let err_pos (lb: Lexing.lexbuf) =
    let pos = lb.lex_curr_p in
    sprintf "[%d:%d]" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in

  let lexbuf = Lexing.from_string s in
  try Ok (Parser.program Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
    let error_msg = sprintf "%s: %s" (err_pos lexbuf) msg in
    Error error_msg
  | Parser.Error ->
    let error_msg = sprintf "%s: syntax error" (err_pos lexbuf) in
    Error error_msg

let _test_parse s =
  match parse s with
  | Error e -> print_string e
  | Ok es -> List.iter (fun x -> printf "%s\n" (Ast.show_expr x)) es

let intrinsics () =
  let intr2 = fun s -> (Stack.pop s, Stack.pop s) in

  let intr2_int f = Intrinsic (fun s ->
    match intr2 s with
    | (Int a, Int b) -> Int (f a b)
    | _ -> failwith "mismatched types")
  in

  let intr2_bool f = Intrinsic (fun s ->
    match intr2 s with
    | (Int a, Int b) -> Boolean (f a b)
    | (Boolean a, Boolean b) -> Boolean (f (int_of_bool a) (int_of_bool b))
    | _ -> failwith "mismatched types")
  in

  let lst = [
    "+", intr2_int (fun x y -> x + y);
    "-", intr2_int (fun x y -> x - y);
    "*", intr2_int (fun x y -> x * y);
    "/", intr2_int (fun x y -> x / y);

    "=",  intr2_bool (fun x y -> x = y);
    "!=", intr2_bool (fun x y -> x != y);
    "<",  intr2_bool (fun x y -> x < y);
    ">",  intr2_bool (fun x y -> x > y);
    "<=", intr2_bool (fun x y -> x <= y);
    ">=", intr2_bool (fun x y -> x >= y);
  ] in

  hashtable_of_list lst

let eval s =
  let global_scope = intrinsics () in
  let global_stack = Stack.create () in

  let rec eval_expr st sc e =
    match e with
    | Nil -> ()
    | Int _ | Boolean _ -> Stack.push e st

    (* | AnonFunc es ->
      eval_expr st sc (assemble_anon_func es) *)

    | ExprRef x ->
      (match x with
      | Symbol x -> 
        (match Hashtbl.find_opt sc x with
        | Some x -> Stack.push x st
        | None -> sprintf "undeclared symbol '%s'" x |> failwith)
      | _ -> Stack.push x st)

    | Symbol x ->
      (match Hashtbl.find_opt sc x with
      | Some (Intrinsic f) -> Stack.push (f st) st
      | Some (Function (args, _)) -> 
        let all = Hashtbl.find_all sc x in
        eval_func st sc all (List.length args)
      | Some x -> Stack.push x st
      | None -> sprintf "undeclared symbol '%s'" x |> failwith)

    (* | Function _ as f -> eval_func st sc [f] *)

    | Grouping es -> eval_exprs st sc es
    | If (cond, t, f) -> eval_if st sc cond t f
    | Def (name, args, exprs) ->
      Hashtbl.add sc name (Function (args, exprs))

    | _ -> failwith "eval_expr unimplemented"
(*   
  and assemble_anon_func es =
    let ids = 
      List.fold_right 
      (fun x a -> match x with
                  | AnonId n when list_contains a n -> a
                  | AnonId n -> n :: a
                  | _ -> a) es []
    in
    let es = 
      List.map 
      (fun x -> match x with
                  | AnonId n -> Symbol (string_of_int n ^ "&")
                  | _ -> x) es
    in
    Function (List.map (fun x -> string_of_int x ^ "&") ids, es) *)

  
  and eval_exprs st sc es =
    List.iter (fun e -> eval_expr st sc e) es
  
  (* evaluate many exprs and confirm only one value was pushed to the stack *)
  and eval_exprs_value st sc es =
    let size1 = Stack.length st in
    eval_exprs st sc es;
    let size2 = Stack.length st in
    
    if (size2 - size1) != 1
      then failwith "expected one value"
      else Stack.pop st
    
  
  and eval_if st sc cond t f =
    match eval_exprs_value st sc cond with
    | Boolean b -> eval_exprs st sc (if b then t else f)
    | _ -> failwith "value in if statement condition is not a boolean!"
  
  and eval_func st sc fs argc =
    let inner = Hashtbl.copy sc in
    let stack = Stack.create () in

    let match_all args params =
      Seq.zip (args |> List.to_seq) (params |> List.to_seq)
      |> Seq.for_all (fun (a, b) -> exact_match a b)
    in

    (* pick the function which pattern matches correctly *)
    let rec pick_func params fs =
      match fs with
      | Function (args, exprs) :: fs -> 
        if match_all args params then (args, exprs)
        else pick_func params fs
      | _ -> failwith "no function overload matches this expression"
    in

    let rec params xs n =
      if n = 0 then xs else params (Stack.pop st :: xs) (n - 1)
    in
    let params = params [] argc in

    (* sort the functions for pattern matching, exact values come first, symbols come last *)
    (* check priority_sort *)
    let fs =
      List.sort
      (fun a b -> match (a, b) with
      | Function (a, _), Function (b, _) ->
        Seq.fold_left (fun a (x, y) -> a + priority_sort x y) 0 (list_zip a b)
      | _ -> failwith "unreachable") fs
    in

    let (args, exprs) = pick_func params fs in
    
    (* bring the named arguments into scope *)
    List.iteri
    (fun i x -> match x with
              | Symbol x -> Hashtbl.add inner x (List.nth params i)
              | _ -> ()) args;


    eval_exprs stack inner exprs;

    Stack.to_seq stack 
    |> List.of_seq
    |> List.rev
    |> List.iter (fun e -> Stack.push e st)
  in
  
  match parse s with
  | Error msg -> failwith msg
  | Ok es -> 
    List.iter (fun e -> eval_expr global_stack global_scope e) es;
    global_stack

let () =
  let ch = open_in "test.nth" in
  try
    let str = really_input_string ch (in_channel_length ch) in
    let s = eval str in
    Stack.iter (fun e -> show_expr e |> printf "%s\n") s
  with e ->
    close_in_noerr ch; raise e
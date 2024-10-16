open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
(*
let move (nfa: ('q, 's) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let rec helper ac qss = match qss with
    | [] -> ac
    | q::t -> let rec helper2 acc qsss = match qsss with
              | [] -> acc
              | (q0, c, q1)::xs -> if (q0=q && c=s) then helper2 (q1::acc) xs else helper2 acc xs
              in helper (helper2 ac nfa.delta) t
  in helper [] qs
*)

  let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    (* Helper method to determine if the nfa delta matches the transition we want *)
      (* Returns a list of all the matches using List.filter *)

    (* Extract the target states *)
    List.fold_left (fun acc (_, _, q) -> insert q acc) [] (List.filter (fun (q0, c, q1) -> (elem q0 qs) && (c = s)) nfa.delta)




let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = let rec helper v qss =
    match qss with
    | [] -> v
    | x::xs -> if elem x v then helper v xs else (let em = (move nfa [x] None)
                in helper (insert x v) (List.sort_uniq Stdlib.compare (em@xs)))
  in helper [] qs

(*accept implementation in Part 2*)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = let rec helper sigm ret = match sigm with 
[] -> ret
| s::t -> helper t ((e_closure nfa ((move nfa qs (Some(s)))))::ret)
in List.rev (helper nfa.sigma [])

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = let rec helper sigm ret = match sigm with 
[] -> ret
| h::t -> helper t ((qs, Some(h), (e_closure nfa ((move nfa qs (Some(h))))))::ret)
in List.rev (helper nfa.sigma [])

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = let rec helper sts fin = match sts with
[] -> fin
| h::t -> helper t (let rec helper2 fss ret = match fss with
    [] -> ret
    | x::xs -> if h=x then [qs] else (helper2 xs ret) in helper2 nfa.fs fin)
in helper qs []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) (dfaqs: 'q list list) : ('q list, 's) nfa_t = match work with
    | [] -> dfa
    | qs::rest -> let add dfaqs qs = if elem qs dfaqs then dfaqs else qs::dfaqs in
    let qss = new_states nfa qs in let ndfa = {
          sigma = nfa.sigma;
          qs = List.fold_left add dfaqs qss;
          q0 = dfa.q0;
          fs = List.fold_left add dfa.fs (new_finals nfa qs);
          delta = List.fold_left (fun acc t -> if elem t acc then acc else t::acc) dfa.delta (new_trans nfa qs);
        } 
      in nfa_to_dfa_step nfa ndfa ((List.filter (fun s -> not (elem s dfaqs)) qss) @ rest) ndfa.qs

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let q0 = e_closure nfa [nfa.q0]
  in let dfa = {
    sigma = nfa.sigma;
    qs = [q0];
    q0 = q0;
    fs = if (elem q0 [nfa.fs]) then [q0] else [];
    delta = [];
  } 
in nfa_to_dfa_step nfa dfa [q0] [q0]

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let dfa = (nfa_to_dfa nfa) in let rec helper dfa cl current = 
  match cl with
  [] -> elem current dfa.fs
  | h::t -> let qto = (let rec find cs cc tl = match tl with
      [] -> []
      | x::xs -> match x with (a,b,c) -> if ((cs=a) && (Some(cc)=b)) then c else (find cs cc xs)
      in find current h dfa.delta) 
      in if qto = [] then false else helper dfa t qto
in helper dfa (explode s) dfa.q0
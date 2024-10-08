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

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = let rec helper nfa qs s = match qs with
| [] -> []
| h::t -> (helper nfa t s) @ (let rec helper2 nfad q s ret = match nfad with
    | [] -> ret
    | x::xs -> match x with
      | (q0,c,q1) -> if (q0=q && c=s) then (helper2 xs q s (q1::ret)) else (helper2 xs q s ret)
  in helper2 nfa.delta h s [])
in helper nfa qs s

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = let fin = let rec helper nfa qs v = match qs with
| [] -> []
| h::t -> [h] @ (helper nfa t (h::v)) @ (let ret = 

  (let rec helper2 nfad q ret = match nfad with
    | [] -> ret
    | x::xs -> match x with
      | (q0,c,q1) -> if (q0=q && c=None) then (if (not (elem q ret)) then (helper2 xs q (q1::ret)) else (helper2 xs q ret)) else (helper2 xs q ret)
  in helper2 nfa.delta h [])

in if subset ret v then v else (helper nfa ret (h::v)))
in helper nfa qs []
in List.sort_uniq Stdlib.compare fin

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
| h::t -> helper t ( (qs, Some(h) , (e_closure nfa ((move nfa qs (Some(h))))) ) :: ret  )
in List.rev (helper nfa.sigma [])

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = let rec helper sts fin = match sts with
[] -> fin
| h::t -> helper t (let rec helper2 fss ret = match fss with
    [] -> ret
    | x::xs -> if h=x then [qs] else (helper2 xs ret) in helper2 nfa.fs fin)
in helper qs []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match work with
    [] -> dfa
    | h::t -> nfa_to_dfa_step nfa (let qss = dfa.qs@(new_states nfa h) in 
                                  (*(let qss = (let n = (new_states nfa h) in match n with [[]]->dfa.qs | _->(dfa.qs@n)) in*)       
                {
                sigma = dfa.sigma;
                qs = List.sort_uniq Stdlib.compare qss;
                q0 = dfa.q0;
                fs = List.sort_uniq Stdlib.compare (dfa.fs@(let rec helper sts ret = match sts with []->ret | x::xs-> (helper xs (ret@new_finals nfa x)) in helper qss []));
                (*delta = (let n = (new_trans nfa h) in match n with [(_,_,[])]->dfa.delta | _->(dfa.delta@n))*)
                delta = (dfa.delta@(new_trans nfa h))
                }) t

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = let ret = let dfa = {
  sigma= nfa.sigma;
  qs= [e_closure nfa [nfa.q0]];
  q0= e_closure nfa [nfa.q0];
  fs= [];
  delta= []
  } 
in let visited = [] in let rec helper v d = match (v,d.qs) with
([],b) -> helper (v@b) (nfa_to_dfa_step nfa d (v@b))
| (a,b) -> if a=b then d else let v1 = List.sort_uniq Stdlib.compare (v@b) in helper v1 (nfa_to_dfa_step nfa d v1)
in helper visited dfa
in {
  sigma = ret.sigma;
  qs = List.sort_uniq Stdlib.compare ret.qs;
  q0 = List.sort_uniq Stdlib.compare ret.q0;
  fs = List.sort_uniq Stdlib.compare ret.fs;
  delta = List.sort_uniq Stdlib.compare ret.delta
}

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let dfa = (nfa_to_dfa nfa) in let rec helper dfa cl current = 
  match cl with
  [] -> elem current dfa.fs
  | h::t -> let qto = (let rec find cs cc tl = match tl with
      [] -> []
      | x::xs -> match x with (a,b,c) -> if ((cs=a) && (Some(cc)=b)) then c else (find cs cc xs)
      in find current h dfa.delta) 
      in if qto = [] then false else helper dfa t qto
in helper dfa (explode s) dfa.q0
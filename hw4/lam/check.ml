open Ast
open Helper

exception TypeError 
exception UnificationError

(* ---------- UNIFICATION ---------- *)
(* [unify c0] solves [c0] (if possible), yielding a substitution. Raise UnificationError if constraints cannot be unified. *)
let rec unify (c0:constr) : subst = 
  (* FILL IN HERE *)
  raise (Failure "unimplemented")

(* [check g e0] typechecks [e0] in the context [g] generating a type and a set of constraints. Raise TypeError if constraints cannot be generated. *)
let rec check (g:context) (e0:exp) : typ * constr = 
  (* FILL IN HERE *)
  raise (Failure "unimplemented")

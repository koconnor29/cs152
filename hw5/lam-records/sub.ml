open Ast
open Helper

exception TypeError

(* A helper function for record types:
  * inputs should be two record types [(x1,t1),...,(xk,tk)] and
  * [(y1,s2),...,(ym,sm)]. Returned value will be a record type
  * [(x1,t1),...,(xk,tk),(y1,s1),...,(ym,sm)]
  *)
let rec comb_records (t1 : typ) (t2 : typ) : typ = 
  match t1, t2 with
  | TRecord(l1), TRecord(l2) -> TRecord(List.append l1 l2)
  | _ -> raise TypeError


let rec is_subtype (t1 : typ) (t2 : typ) : bool =
  match t1,t2 with
  | TInt, TInt -> true
  | TBool, TBool -> true
  | TArrow(s1,s2),TArrow(s3,s4) -> 
      (is_subtype s2 s4) && (is_subtype s3 s1)
  | TRecord(l1), TRecord(l2) -> 
      (match l1,l2 with
      | _,[] -> true
      | [],hd::tl -> false
      | (x1,s1)::tl1, (x2,s2)::tl2 -> 
          (is_subtype s1 s2) && (is_subtype (TRecord(tl1)) (TRecord(tl2)))
      )
  | _ -> false


let rec lub (t1 : typ) (t2 : typ) : typ =
  match t1,t2 with
  | TInt, TInt -> TInt
  | TBool, TBool -> TBool
  | TArrow(ty1, ty2), TArrow(ty3, ty4) -> 
      TArrow((glb ty1 ty3), (lub ty2 ty4))
  | TRecord(l1), TRecord(l2) ->
    (match l1, l2 with
    | [], _ -> TRecord([])
    | _,[] -> TRecord([])
    | (x1, ty1)::tl1, (x2, ty2)::tl2 ->
        let joe = [(x1,(lub ty1 ty2))] in
        let temp = TRecord(joe) in
        let temp1 = TRecord(tl1) in
        let temp2 = TRecord(tl2) in
        comb_records temp (lub temp1 temp2)
    )
  | _ -> raise TypeError
and glb (t1 : typ) (t2 : typ) : typ =
  match t1,t2 with
  | TInt, TInt -> TInt
  | TBool, TBool -> TBool
  | TArrow(ty1, ty2), TArrow(ty3, ty4) -> 
      TArrow((lub ty1 ty3), (glb ty2 ty4))
  | TRecord(l1), TRecord(l2) ->
    (match l1, l2 with
    | [], _ -> TRecord(l2)
    | _,[] -> TRecord(l1)
    | (x1, ty1)::tl1, (x2, ty2)::tl2 ->
        let temp = TRecord([(x1,(glb ty1 ty2))]) in
        let temp1 = TRecord(tl1) in
        let temp2 = TRecord(tl2) in
        comb_records temp (lub temp1 temp2)
    )
  | _ -> raise TypeError


let rec check (g : context) (e : exp) : typ =
  match e with
  | Var(x) -> if (VarMap.mem x g) then 
              (VarMap.find x g) else
                raise TypeError
  | App(e1,e2) -> let t2 = check g e2 in
                  let t1 = check g e1 in 
                  (match t1 with
                  | TArrow(t3,t4) -> if (is_subtype t2 t3) then t4
                       else raise TypeError
                  | _ -> raise TypeError
                  )
  | Lam(x,t,e1) -> let t' = check (VarMap.add x t g) e1 in
                  TArrow(t,t')

  | Int(n) -> TInt
  | Plus(e1,e2) | Minus(e1,e2) | Times(e1,e2) -> 
                    (match (check g e1, check g e2) with
                    | (TInt, TInt) -> TInt
                    | _ -> raise TypeError  )
  | True | False -> TBool
  | Eq(e1,e2) -> (match (check g e1, check g e2) with
                  | (TInt, TInt) -> TBool
                  | _ -> raise TypeError )
  | If(e1,e2,e3) -> 
      let t1,t2,t3 = (check g e1, check g e2, check g e3) in
      if t1 = TBool then lub t2 t3 else raise TypeError
  | Record(l) ->
      (match l with
      | [] -> TRecord([])
      | (v,e1)::tl -> 
          comb_records (TRecord([v,check g e1])) (check g (Record(tl)))
      )
  | Field(e1,v) ->
      (match (check g e1) with
      | TRecord(l) -> lookup v l
      | _ -> raise TypeError
      )

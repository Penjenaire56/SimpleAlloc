open Usuba_AST

module ISet = Set.Make(Int)
module SSet = Set.Make(String)
module IMap = Map.Make(Int)
module SMap = Map.Make(String)

type memory = {
    k : int;
    max : int;
    free : int IMap.t;
    usage : int IMap.t;
    regs : string IMap.t;
    binds : int SMap.t;
}

type _var = string 
type _val = int 

type src = 
    | IO of _var * string * int 
    | Var of _var
    | Val of _val
 
type dst = 
    | DVar of _var
    | DIO of _var * string * int

type op = 
    OMul | OSum | ODiv | 
    OSub | OMod | OAnd | 
    OOr | OXor | OAndn | 
    OMasked

type index = src

type inst = 
    | Oper of op * dst * src * src
    | Move of dst * src

let srcToStr (exp : src) = 
    match exp with
    | Val i -> string_of_int i
    | Var s -> s
    | IO(s,_,_) -> s

let dstToStr (exp : dst) = 
    match exp with 
    | DVar s -> s
    | DIO(s,_,_) -> s 

let getOperStr o = 
    match o with
    | OAnd -> " & "
    | OXor -> " ^ "
    | OOr -> " | "
    | _ -> failwith "OPERATOR"

(* Donne les variables définies pendant une instruction *)
let defN = function
    | Move(dst,_) | Oper(_,dst,_,_) -> 
        dstToStr dst

let srcToStrOpt (a : src) = 
    match a with 
    | Var a | IO(a,_,_) -> Some a
    | Val _ -> None

(* Donne les variables utilisés pendant une instruction *)
let useN = function
    | Move(_, Val _) -> SSet.empty 
    | Move(_,s) -> SSet.singleton (srcToStr s)
    | Oper(_,_, s1, s2) -> (
        match srcToStrOpt s1, srcToStrOpt s2 with
        | None, None -> SSet.empty
        | None, Some s | Some s, None -> 
            SSet.singleton s
        | Some s1, Some s2 -> 
            SSet.add s1 (SSet.singleton s2)
    )

let read_lines name : string list =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []

let readSexpr (path : string) = 
    let lines = read_lines path in 
    let txt = String.concat "" lines in 
    prog_of_sexp (Sexplib.Sexp.of_string txt)

let pprintInst (i : inst) = 
    let printDst dst = 
        match dst with
        | DIO(_,x,k) -> "[" ^ x ^ " + " ^ (string_of_int (k * 8)) ^ "]"
        | _ -> dstToStr dst 
    in
    
    let printSrc src = 
        match src with
        | IO(_,x,k) -> "[" ^ x ^ " + " ^ (string_of_int (k * 8)) ^ "]"
        | _ -> srcToStr src
    in
    
    match i with 
    | Oper(o , dst, src1, src2) -> 
        printDst dst ^ " = " ^ printSrc src1 ^ getOperStr o ^ printSrc src2
    | Move(dst,src) -> 
        printDst dst ^ " = " ^ printSrc src

let pprintInstC (i : inst) = 
    let printDst dst = 
        match dst with
        | DIO(_,x,k) -> x ^ " [" ^ string_of_int k ^ "] "
        | _ -> dstToStr dst 
    in
    
    let printSrc src = 
        match src with
        | IO(_,x,k) -> x ^ " [" ^ string_of_int k ^ "]"
        | _ -> srcToStr src
    in
    
    match i with    
    | Oper(o , dst, src1, src2) -> 
        printDst dst ^ " = " ^ printSrc src1 ^ getOperStr o ^ printSrc src2 ^ ";"
    | Move(dst,src) -> 
        printDst dst ^ " = " ^ printSrc src ^ ";"

let instTest (i1 : inst list) (i2 : inst list) pTest = 
    pTest "
btor = Boolector()
btor.Set_opt(pyboolector.BTOR_OPT_MODEL_GEN, True)
bvsort = btor.BitVecSort(1)
";

    let rec printBodyTest (i : inst list) (spe : string) = 
        let getOperTest (o : op) d s1 s2 = 
            let f = match o with 
            | OMul -> "Mul" | OSum -> "Add"
            | ODiv -> "Udiv" | OSub -> "Sub"
            | OAnd -> "And" | OOr -> "Or"
            | OXor -> "Xor" | _ -> failwith "Ope"
            in
            d ^ " = btor." ^ f ^ "(" ^ s1 ^ "," ^ s2 ^ ")"
        in

        let getSpeSrc src =
            match src with 
            | IO(s,_,_) | Var s -> spe ^ s
            | Val x -> "btor.Const(" ^ string_of_int x ^ ", 1)"
        in

        match i with
        | [] -> ()
        | x :: subL -> (
            match x with 
            | Move(dst, src) -> 
                let dst = spe ^ (dstToStr dst) in 
                let src = getSpeSrc src in
                    pTest ((dst ^ " = " ^ src) ^ "\n");
                    printBodyTest subL spe


            | Oper(op , dst, src1, src2) -> 
                let dst = spe ^ (dstToStr dst) in 
                let s1 = getSpeSrc src1 in
                let s2 = getSpeSrc src2 in
                    pTest ((getOperTest op dst s1 s2) ^ "\n");
                    printBodyTest subL spe
    )
    in

    let rec instToTest (i : inst list) (spe : string) v_out v_in = 
        let dstInEnv dst env = 
            match dst with 
            | DIO(s,_,_) -> 
                SMap.add s (spe ^ s) env
            | _ -> env
        in

        let srcInEnv src env = 
            match src with 
            | IO(s,_,_) -> SMap.add s (spe ^ s) env
            | _ -> env
        in

        match i with
        | [] -> v_out, v_in
        | x :: subL -> (
            match x with 
            | Move(dst, src) -> 
                let v_out = dstInEnv dst v_out in 
                let v_in = srcInEnv src v_in in
                instToTest subL spe v_out v_in

            | Oper(_ , dst, src1, src2) -> 
                let v_out = dstInEnv dst v_out in 
                let v_in = srcInEnv src1 v_in in
                let v_in = srcInEnv src2 v_in in
                    instToTest subL spe v_out v_in  
        ) 
    in

    let v_out1, v_in1 = instToTest i1 "p1_" SMap.empty SMap.empty in 
    let v_out2, v_in2 = instToTest i2 "p2_" SMap.empty SMap.empty in
    SMap.iter (fun x a -> 
        pTest (x ^ " = btor.Var(bvsort, \"" ^ x ^ "\")\n" ^ 
        a ^ " = " ^ x ^ "\n" ^ 
        (SMap.find x v_in2) ^ " = " ^ x ^ "\n")
    ) v_in1;

    printBodyTest i1 "p1_";
    printBodyTest i2 "p2_";


    let list_assert = SMap.fold (fun x a l -> 
        pTest (x ^ " = btor.Ne(" ^ a ^ ", " ^ (SMap.find x v_out2) ^ ")\n"); x :: l
    ) v_out1 [] in 

    let rec aux_assert = function
        | [] -> ()
        | [x] -> pTest ("btor.Assert(" ^ x ^ ")")
        | x :: y :: subL -> 
            pTest (y ^ " = btor.Or(" ^ x ^ ", " ^ y ^ ")\n");
            aux_assert (y :: subL)
    in

    aux_assert list_assert;
        
    pTest "
res = btor.Sat()
if res == btor.SAT:
    btor.Print_model(\"btor\")
    print(\"sat\")
elif res == btor.UNSAT:
    print(\"unsat\")
else:
    print(\"unknown\")
"

let getOp (v : string) = 
    match v with
    | "+" -> OXor
    | "*" | "x" -> OAnd
    | _ -> failwith ("Operator " ^ v)

let arith_opToOp (a : arith_op) = 
    match a with 
    | Add -> OSum | Mul -> OMul | Sub -> OSub
    | Div -> ODiv | Mod -> OMod

let log_opToOp (a : log_op) = 
    match a with 
    | And -> OAnd | Or -> OOr | Xor -> OXor 
    | Andn -> OAndn | Masked _ -> OMasked

let cleanName name =
    String.concat "" (String.split_on_char '\'' name)

let freshName (k : int) = 
    k + 1 , "tmp" ^ (string_of_int k)

let getIOName name =
    match String.split_on_char '_' name with
    | n :: k :: _ -> n , int_of_string k
    | _ -> failwith "ERROR NAME"

let srcName (name : string) (v_io : SSet.t) (io : (bool * string) SMap.t) k = 
    if SSet.mem name v_io then 
        if SMap.mem name io then
            let _ , n = SMap.find name io in 
            k , io, [], Var n
        else
            (let k, tmp = freshName k in
            k, SMap.add name (false,tmp) io, [], Var tmp)
    else 
        k, io, [], Var name

let dstName (name : string) (v_io : SSet.t) (io : (bool * string) SMap.t) v2 k = 
    if SSet.mem name v_io then 
        if SMap.mem name io then (
            let b, tmp = SMap.find name io in
            if b then 
                k, io, [ Move(DVar tmp, v2) ]
            else
                let n, x = getIOName name in
                k, SMap.add name (true,tmp) io, [ Move(DVar tmp, v2); Move(DIO(name, n, x), Var tmp) ]
        ) else
            let k, tmp = freshName k in
            let n, x = getIOName name in
            k, SMap.add name (true,tmp) io, [ Move(DVar tmp, v2); Move(DIO(name, n, x), Var tmp) ]
    else 
        k, io , [ Move(DVar name, v2) ]

let rec numToInstr (n : arith_expr) (k : int) (v_io : SSet.t) (io : (bool * string) SMap.t) = 
    match n with       
    | Const_e i -> k, io, [] , Val i
    | Var_e i -> srcName (cleanName i.name) v_io io k
    | Op_e (op, n1, n2) -> 
        let k, io, i1, v1 = numToInstr n1 k v_io io in 
        let k, io, i2, v2 = numToInstr n2 k v_io io in 
        let k, tmp = freshName k in 
        k, io, i1 @ i2 @ [Oper(arith_opToOp op, DVar tmp, v1, v2)], Var tmp


let rec varToInstr (v : var) (k : int) (v_io : SSet.t) (io : (bool * string) SMap.t) = 
    match v with 
    | Var id -> 
        let tmp = cleanName id.name in
            k, v_io, io, [], tmp

    | Index(v, n) -> 
        let k, v_io, io, i1, v1 = varToInstr v k v_io io in 
        let k, io, i2, v2 = numToInstr n k v_io io in 
        
        let name = v1 ^ "_" ^ srcToStr v2 in
        
        let v_io = if SSet.mem v1 v_io then
            SSet.add name v_io
        else
            v_io
        in

        k, v_io, io, i1 @ i2 , name

    | _ -> failwith "varToINst"

let rec exprToInst (e : expr) (k : int) (v_io : SSet.t) (io : (bool * string) SMap.t) = 
    match e with
    | Const(i, _) -> 
        k, v_io, io, [] , Val i
    | ExpVar(v) -> 
        let k, v_io, io, i1, v = varToInstr v k v_io io in 
        let k, io, i2, v = srcName v v_io io k in 
            k, v_io, io, i1 @ i2, v
    | Log(o,e1,e2) -> 
        let k, v_io, io, i1, v1 = exprToInst e1 k v_io io in 
        let k, v_io, io, i2, v2 = exprToInst e2 k v_io io in 
        let k, tmp = freshName k in 
        k, v_io, io, i1 @ i2 @ [Oper(log_opToOp o, DVar tmp, v1, v2)] , Var tmp

    | Not(e1) -> 
        let k, v_io, io, i, v = exprToInst e1 k v_io io in
        let k, tmp = freshName k in 
        k, v_io, io, i @ [Oper(OXor, DVar tmp, v, Val (-1))] , Var tmp
    | Shift(_, _, _) -> failwith "Expr SHIFT ERROR"
    | Arith(_, _, _) -> failwith "Expr Arith ERROR"
    | Shuffle(_,_) -> failwith "Expr Shuffle ERROR"
    | Bitmask(_, _) -> failwith "Expr Bitmask ERROR"
    | _ -> failwith "Expr ERROR"

let deq_iToInst (d : deq_i) (k : int) (v_io : SSet.t) (io : (bool * string) SMap.t) = 
    match d with 
    | Eqn(v, exp, _) -> 
        (match v with
        | [] -> failwith "Empty dst"
        | [x] -> 
            let k, v_io, io, i1, v1 = varToInstr x k v_io io in 
            let k, v_io, io, i2, v2 = exprToInst exp k v_io io in 

            let k, io, li = dstName v1 v_io io v2 k in

            k, v_io, io, i1 @ i2 @ li
        | _ -> failwith "Too many dst")
    | _ -> failwith "deq_i"

let deqToInst (d : deq list) (v_io : SSet.t) = 
    (List.fold_right (fun x (k,v_io,io,l) -> 
        let k',v_io',io',l' = (deq_iToInst x.content k v_io io) in
            k', v_io', io', l' @ l) d (0, v_io, SMap.empty, []))

let def_iToInst (d : def_i) (v_io : SSet.t) = 
    match d with
    | Single(_, code) ->
        let _, v_io, io, instrs = deqToInst code v_io in
            (v_io, io, instrs)
    | _ -> failwith "Error def_i"

type stability = 
    | Stable
    | UnStable
    | Mixed of (int * int) list  

type structSize = 
    | Len of int
    | Dim of int * structSize

let rec getSizeStruct = function    
    | Len(x) -> x
    | Dim(x, l) -> x * getSizeStruct l 

(* Récupère la taille en bit des entrées sorties du programme *)
let getSizeParams pin = 
    (* Récupère la taille en se basant sur le typage *)
    let rec typeSize t = 
        match t with
        | Uint (Bslice,Mint(_),x) -> 
            Len(x)
        | Array(t, Const_e(x)) -> Dim(x, typeSize t)
        | _ -> failwith "SIZE ERROR"
    in

    let addSize x m = 
        let name = x.vd_id.name in 
        let size = typeSize x.vd_typ in 
            SMap.add (cleanName name) (size, 0) m
    in
    
    List.fold_right addSize pin SMap.empty

let defToInst (d : def) (max : int -> bool) shape =
    let v_io = List.fold_right 
        (fun x s -> SSet.add (cleanName x.vd_id.name) s) d.p_in SSet.empty
    in
    let v_io = List.fold_right 
        (fun x s -> SSet.add (cleanName x.vd_id.name) s) d.p_out v_io
    in

    let head = v_io in 

    let v_io, io, inst = def_iToInst d.node v_io in

    let rec setInputs li inputs env = 
        match li with
        | [] -> []
        | i :: subL -> (
            let env, ins = SSet.fold (fun x (e,l) -> 
                if SSet.mem x env then
                    e, l
                else if SMap.mem x inputs then (
                    let name = SMap.find x inputs in 
                    let n , k = getIOName name in
                    SSet.add x e, l @ [Move(DVar x , IO(name,n,k))]
                )
                else
                    e, l
            ) (useN i) (env , []) in 
            
            ins @ (i :: (setInputs subL inputs env))
        )
    in

    let rec apply shape lp = 
        match shape, lp with 
        | [_], [d] -> int_of_string d
        | s :: shape, d :: lp -> 
            s * (int_of_string d) + (apply shape lp) 
        | _ -> failwith "error shape"
    in

    let f x (i,o) = 
        match String.split_on_char '_' x with
        | "key" :: _ :: _ -> 
            SMap.add x true i,o
        | "plain" :: lp -> 
            if List.length lp <> List.length shape then
                SMap.add x false i , o
            else
                let n = apply (List.rev shape) lp in 
                SMap.add x (max n) i,o
        | "cipher" :: _ -> 
            i, SSet.add x o
        | _ -> i,o
    in

    let (_in_, _out_) = SSet.fold f v_io (SMap.empty, SSet.empty) in

    let inputs = SMap.fold (fun k _ m ->
        match SMap.find_opt k io with
        | Some(_, v) -> 
            SMap.add v k m
        | None -> m
    ) _in_ SMap.empty in

    let inst = setInputs inst inputs SSet.empty in

    (head, _in_,_out_) , inst

(* Récupère les instructions correspondant au prog usuba (+ info sur les tailes) *)
let sexprToInst (p : prog) (max : int -> bool) shape = 
    match p.nodes with 
    | [] -> failwith "Empty program"
    | f :: _ -> defToInst f max shape

let getIndiceFromName (name : string) = 
    (String.make 1 (String.get name 0)) ^ "[" ^ (String.sub name 1 ((String.length name) - 1)) ^ "]"

let addVars (v : SSet.t) (n : string) = 
    SSet.add n v

let getIndice (name : string) = 
    int_of_string (String.sub name 1 ((String.length name) - 1))

let getDST (s : string) (v : SSet.t) = 
    match String.get s 0 with
    | '0' .. '9' -> 
        failwith "ERROR DST CANT BE A CONST"
    | c when c = 'A' || c = 'B' || c = 'C' ->
        let array = String.make 1 c in 
        let id = getIndice s in 
            DIO (array ^ string_of_int id, array, id) , addVars v array
    | _ -> 
        DVar s, addVars v s

let getSRC (s : string) (v : SSet.t) = 
    match String.get s 0 with
    | '0' .. '9' -> 
        Val (int_of_string s) ,  v
    | c when c = 'A' || c = 'B' || c = 'C' ->
        let array = String.make 1 c in 
        let id = getIndice s in 
            IO (array ^ string_of_int id, array, id) , addVars v array
    | _ -> 
        Var s, addVars v s

let getMove (line : string list) (v : SSet.t) = 
    let v1,v = getDST (List.nth line 0) v in 
    let v2,v = getSRC (List.nth line 2) v in 
        v,[Move(v1 , v2)]

let getOper (line : string list) (v : SSet.t) = 
    let v1, v = getDST (List.nth line 0) v in 
    let v2, v = getSRC (List.nth line 2) v in 
    let o = List.nth line 3 in 
    let op = getOp o in 
    let v3, v = getSRC (List.nth line 4) v in 
        v, [Oper(op, v1, v2, v3)]

(* Construit le programme à partir d'un fichier test *)
let parse (path : string) = 
    let lines = read_lines path in

    let rec parse_line v = function
    | [] -> v , []
    | x :: subL -> 
        let line = String.split_on_char ' ' x in 
        let len = List.length line in
            if len < 2 then  
                parse_line v subL
            else
                let v,i = 
                    if len = 5 then
                        getOper line v
                    else if len = 3 then 
                        getMove line v
                    else 
                        failwith "format line"
                in let v,l = parse_line v subL in 
                    v, i @ l

        in parse_line SSet.empty lines

let rec aux_reduce (instrs : inst list) =
    match instrs with
        | [] -> []
        | i :: subI -> (
            match i with
            | Oper(_, DVar dst, _,_) | Move(DVar dst,_) 
            | Oper(_, DIO (dst,_,_), _,_) | Move(DIO (dst,_,_),_) -> 
                aux_insert subI dst i
        )  

and aux_insert (instrs : inst list) (name : string) (x : inst) = 
    match instrs with
    | [] -> [] 
    | i :: subI -> (
        match i with
        | Oper(_, _, Var s1, Var s2) when s1 = name || s2 = name -> 
            x :: i :: aux_reduce subI
        | Move(_, Var s) when s = name -> 
            x :: i :: subI
        | _ -> 
            i :: (aux_insert subI name x)
    )

let reduce (instrs : inst list) = 
    let rec aux instrs k = 
        if k > 0 then
            aux (aux_reduce instrs) (k - 1) 
        else
            instrs
    in aux instrs 1000

let mem_free (mem : memory) (v : string) = 
    let n = SMap.find v mem.binds in 
    let v = IMap.find n mem.usage in {
    k = mem.k;
    max = mem.max;
    free = IMap.add n v mem.free;
    usage = IMap.remove n mem.usage;
    binds = mem.binds;
    regs = mem.regs;
}

let popS (newest : SSet.t) = 
    let n = SSet.choose newest in 
        (n, SSet.remove n newest) 

let getFresh (mem : memory) n = 
    if IMap.is_empty mem.free then 
    { 
        k = mem.k + 1;
        max = mem.max;
        free = mem.free;
        usage = IMap.add mem.k 0 mem.usage;
        binds = SMap.add n mem.k mem.binds;
        regs = IMap.add mem.k n mem.regs;
    } 
    else let (k, v) = IMap.choose mem.free in {
        k = mem.k;
        max = mem.max;
        free = IMap.remove k mem.free;
        usage = IMap.add k v mem.usage;
        binds = SMap.add n k mem.binds;
        regs = IMap.add k n mem.regs;
    }

let rec aux_update newest (mem : memory) = 
    if SSet.is_empty newest then
        mem
    else 
        let (n, newest) = popS newest in 
            aux_update newest (getFresh mem n)

(* Mets à jour les binds et les variables définies *)
let update def newest (mem : memory) = 
    match def with
    | None ->
        aux_update newest mem 
    | Some(def) -> 
        aux_update newest (mem_free mem def)

let getRegistreName (var : string) (binds : int SMap.t) = 
    "r" ^ (string_of_int (SMap.find var binds))

let getRegistreNameDST (var : dst) (binds : int SMap.t) = 
    match var with
    | DVar s -> 
        DVar (getRegistreName s binds)
    |  _ -> var

let getRegistreNameSRC (var : src) (binds : int SMap.t) = 
    match var with 
    | Var s ->  
        Var (getRegistreName s binds)
    | _ -> var

let getRegistreNames (d,s1,s2) prevbinds binds = 
    let rdest = getRegistreNameDST d prevbinds in 
    let rserc1 = getRegistreNameSRC s1 binds in 
    let rserc2 = getRegistreNameSRC s2 binds in 
        (rdest, rserc1, rserc2)

let srcInSSet (expr : src) (s : SSet.t) = 
    match expr with
    | IO (x,_,_) | Var x -> 
        SSet.mem x s
    | _ -> false

let dstInSSet (expr : dst) (s : SSet.t) = 
    match expr with
    | DIO (x,_,_) | DVar x -> 
        SSet.mem x s

(* Construit les nouvelles instructions avec les registres générés automatiquement *)
let distribute (i : inst) (prevbinds : int SMap.t) (newest : SSet.t) mem = 
    match i with
    | Move(dst, src) ->
        [Move(getRegistreNameDST dst prevbinds, getRegistreNameSRC src mem.binds)] , mem
    | Oper(op, d, s1, s2) when (srcInSSet s1 newest) && (srcInSSet s2 newest) -> 
        let tmp1 = "tmp1" in 
        let mem = aux_update (SSet.singleton tmp1) mem in 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 

        let rtmp1 = getRegistreNameDST (DVar tmp1) mem.binds in
   
        let stmp1 = getRegistreNameSRC (match s1 with 
            | IO(_,_,_) -> s1
            | _ -> Var tmp1
        ) mem.binds in
        
        [Oper(op, rdest, stmp1, rserc2); Move(rtmp1, rserc1)], mem_free mem tmp1 
    | Oper(op, d, s1, s2) when (srcInSSet s1 newest) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 
        [Oper(op, rdest, rserc2, rserc1)] , mem
    | Oper(op, d, s1, s2) -> 
        let rdest, rserc1, rserc2 = getRegistreNames (d,s1,s2) prevbinds mem.binds in 
        [Oper(op, rdest, rserc1, rserc2)] , mem

let clean prog =
    List.filter (function
        | Move(DVar d,Var s) -> d <> s 
        | _ -> true
    ) prog 

let update_usage (mem : memory) (vars : SSet.t) = {
    k = mem.k;
    max = mem.max;
    free = mem.free;
    usage = SSet.fold 
    (fun s u -> 
        let k = (SMap.find s mem.binds) in
        let v = IMap.find_opt k u in 
        match v with
        | None -> u 
        | Some(v) -> (IMap.add k (v + 1) u)
    ) vars mem.usage;
    binds = mem.binds;
    regs = mem.regs;
}

let init (initials : SSet.t) (max : int) = (* TODO stack if enougth variables *)
    let (k, regs, usage) = SSet.fold 
        (fun s (k,m,u) -> k + 1, (IMap.add k s m), (IMap.add k 0 u)) 
        initials (0, IMap.empty, IMap.empty) 
    in let binds = IMap.fold (fun k v m -> SMap.add v k m) regs SMap.empty 
    in {
        k = k;
        max = max;
        free = IMap.empty; 
        usage = usage;
        regs = regs;
        binds = binds;
    }

(* Construit un nouveau programme avec les registres automatiquement alloués *)
let analyse (instr : inst list) (initials : SSet.t) (max : int) = 
    let instr = List.rev instr in 

    let mem = init initials max in
    let rec cross (variables : SSet.t) (mem : memory) = function
    | [] -> 
        let acc = (IMap.bindings mem.usage) @ (IMap.bindings mem.free) in 
        let acc = List.sort (fun (_,x) (_,y) -> Int.compare x y) acc in 
        let acc = List.rev acc in 

        let rec split k = function 
        | [] -> [],[]
        | (x,_) :: subL -> 
            let (l1,l2) = split (k - 1) subL in 
                if k > 0 then 
                    (x :: l1, l2)
                else 
                    (l1, x :: l2)
        in

        let (regAcc, stackAcc) = split mem.max acc in
        (List.map (fun n -> "r" ^ (string_of_int n)) regAcc, List.map (fun n -> "r" ^ (string_of_int n)) stackAcc) , []
    | i :: subL -> 
        (* Variable déclaré pendant l'instruction *)
        let def = defN i in 
        
        (* Variable existant avant l'instruction *)
        let defopt = SSet.singleton def in 
        let pre = SSet.diff variables defopt in 

        (* Variable utilisé pendant l'instruction *)
        let use = useN i in

        (* Variable déclaré pendant l'instruction et étant nouvelle *)
        let newest = SSet.diff (SSet.diff use pre) variables in 

        let previousBind = mem.binds in
         
        let mem = update (Some def) newest mem in 

        let mem = update_usage mem use in 
        let instrs, mem = distribute i previousBind (SSet.diff use newest) mem in 
        let (regAcc, stackAcc) , subL = cross (SSet.union pre use) mem subL in 
            (regAcc, stackAcc) , instrs @ subL
    in 
    let (regAcc, stackAcc), prog = cross initials mem instr in 
    let optiProg = clean prog in (regAcc, stackAcc), optiProg

let slpToJasmin dst prog stack reg head id = 
    let header = 
        (SSet.fold(fun x str -> str ^ ", reg u64 " ^ x) head ("export fn p" ^ (string_of_int id) ^ "(reg u64 _tmp")) ^ "){\n"
    in 
    let footer = "\n}\n" in 
    let declaration = 
        (List.fold_right (fun n str -> str ^ "\tstack u64 " ^ n ^ ";\n") 
            stack 
            (List.fold_right (fun n str -> str ^ "\treg u64 " ^ n ^ ";\n") reg "")) in 
            
    let body = List.fold_right (fun x str -> str ^ "\t" ^ (pprintInst x ^ ";\n")) prog "" in 
    
    let saveProg = open_out dst in 
        output_string saveProg (header ^ declaration ^ body ^ footer);
        close_out saveProg

let slpToC dst prog stack reg head id = 
    let header = 
        (SSet.fold(fun x str -> str ^ ", uint64_t* " ^ x) head ("void p" ^ (string_of_int id) ^ "(uint64_t* _tmp")) ^ "){\n"
    in
    let footer = "\n}\n" in 
    let declaration = 
        (List.fold_right (fun n str -> str ^ "\tuint64_t " ^ n ^ ";\n")) (stack @ reg) ""
    in 

    let body = List.fold_right (fun x str -> str ^ "\t" ^ (pprintInstC x ^ "\n")) prog "" in 

    let saveProg = open_out dst in 
        output_string saveProg (header ^ declaration ^ body ^ footer);
        close_out saveProg

let stackAlloc (i : inst) (stack : SSet.t) = 
    match i with
    | Oper(o , dst, s1, s2) when (dstInSSet dst stack) && (srcInSSet s1 stack) && (srcInSSet s2 stack) -> 
        [
            Move(DVar "tmp", s1);
            Oper(o, DVar "tmp", Var "tmp", s2);
            Move(dst,Var "tmp");
        ]
    | Oper(o , dst, s1, s2) when srcInSSet s1 stack && srcInSSet s2 stack -> 
        [
            Move(DVar "tmp", s1);
            Oper(o, dst,Var "tmp", s2);
        ]
    | Oper(o , dst, s1, s2) when dstInSSet dst stack && srcInSSet s1 stack -> 
        [
            Move(DVar "tmp", s1);
            Oper(o, DVar "tmp",Var "tmp", s2);
            Move(dst,Var "tmp");
        ]
    | Oper(o , dst, s1, s2) when dstInSSet dst stack -> 
        [
            Oper(o, DVar "tmp", s1, s2);
            Move(dst, Var "tmp");
        ]
    | Oper(o , dst, s1, s2) when srcInSSet s1 stack -> 
        [
            Oper(o, dst, s2, s1);
        ]
    | i -> [i]
    
let getArrayOfName (name : string) = 
    let n = String.make 1 (String.get name 0) in 
    let c = int_of_string (String.sub name 1 ((String.length name) - 1)) in 
        n,Val c

let isStr (a : src) (s : string) = 
    match a with 
    | Val _ -> false
    | Var a | IO (a,_,_) -> a = s

let dstToStr (a : dst) = 
    match a with 
    | DVar a | DIO (a,_,_) -> a

let pickWorst (t : SSet.t) (prog : inst list) (used : SSet.t) = 
    let dstisStr (d : dst) s = 
        match d with 
        | DVar d | DIO (d,_,_) -> d = s
    in
    let rec pick (s : string) k = function
    | [] -> k
    | i :: subL -> 
        (match i with
            | Move(dst, src) when dstisStr dst s || isStr src s -> k 
            | Oper(_, dst, s1 , s2) when dstisStr dst s || isStr s1 s || isStr s2 s -> k
            | _ -> pick s (k + 1) subL
        )
    
    in let default_value s = 
        match SSet.mem s used with
        | true -> (-40)
        | false -> 0
    
    in let lp = SSet.fold (fun k l -> (k, pick k (default_value k) prog) :: l) t [] 
    
    in let lp = List.sort (fun (_,x) (_,y) -> Int.compare y x) lp 
    in let (k,v) = (List.hd lp) in 
    
    (* TODO BETTER SPILLING *)
    if v > 5 then
        Some k
    else 
        None
    
let rename (name : string) (reg : string) (sta : string) = 
    if name = reg then 
        sta
    else if name = sta then 
        reg
    else 
        name

let partial (i : inst) (reg : string) = 
    let reg = DVar reg in 
    match i with 
    | Move(_,src) -> Move(reg, src)
    | Oper(o, _, s1 , s2) -> 
        Oper(o, reg, s1,s2)   

(* Renomme les variables pour effectuer un spill *)
let spill (i : inst) (sta : string) (reg : string) = 
    let rename_src src = 
        match src with
        | Var src -> Var(rename src reg sta)
        | _ -> src
    in

    let rename_dst dst = 
        match dst with
        | DVar dst -> DVar (rename dst reg sta)
        | _ -> dst
    in 

    match i with 
    | Move(dst, src) -> 
        Move(rename_dst dst, rename_src src)
    | Oper(o, dst, s1 , s2) -> 
        Oper(o, rename_dst dst, rename_src s1, rename_src s2) 
      

let spilling (prog : inst list) (accReg : SSet.t) (accStack : SSet.t) = 
    let update_used (used : SSet.t) reg stack =
        match SSet.find_opt reg used, SSet.find_opt stack used with
        | None, None -> 
            SSet.add reg used
        | Some(_), None -> 
            SSet.add stack used
        | None, Some(s) ->
            SSet.add reg (SSet.remove s used)
        | _ -> used  
    in


    let rec aux (prog : inst list) (accReg : SSet.t) (accStack : SSet.t) (used : SSet.t) = 
    match prog with
    | [] -> []
    | i :: subL -> 
        (match defN i with
        | stack when SSet.mem stack accStack -> 
            let nused = SSet.add stack used in
            (match pickWorst accReg prog used with
            | None -> 
                (stackAlloc i accStack) @ (aux subL accReg accStack nused)  
            | Some reg -> 
                let nused = update_used used reg stack in 
                let subL = List.map (fun i -> spill i stack reg) subL in 
                    if SSet.mem stack (useN i) then
                        if SSet.mem stack nused then
                            (Move(DVar "tmp", Var stack) :: 
                            Move(DVar stack, Var reg) :: 
                            (stackAlloc (spill (partial i reg) stack "tmp") accStack) @ 
                            (aux subL accReg accStack nused))
                        else
                            (stackAlloc (partial i reg) accStack) @ 
                            (aux subL accReg accStack nused)
                    else if SSet.mem stack nused then
                        Move(DVar stack, Var reg) :: 
                        (stackAlloc (partial i reg) accStack) @ 
                        (aux subL accReg accStack nused)
                    else 
                        (stackAlloc (partial i reg) accStack) @ 
                        (aux subL accReg accStack nused)
            )
        | reg -> 
            (stackAlloc i accStack) @ 
            (aux subL accReg accStack (SSet.add reg used))
    )
    in aux prog accReg accStack (SSet.empty)

let generate (instrs : inst list) (init : bool SMap.t) =
    let testSrc (exp : src) env = 
        match exp with
        | Val _ -> true
        | Var s -> 
            SMap.find s env
        | IO (x,_,_) ->
            SMap.find x env
    in              

    let addDst (exp : dst) b env = 
        match exp with
        | DVar s -> SMap.add s b env
        | DIO (x,_,_) -> SMap.add x b env
    in

    let rec aux instrs env _f f_ io k = 
        match instrs with
        | [] -> _f, f_, io
        | i :: subL -> (
            match i with 
            | Move(dst,src) -> (
                if testSrc src env then
                    aux subL (addDst dst true env) (i :: _f) f_ io k
                else
                    aux subL (addDst dst false env) _f (i :: f_) io k
            )
            | Oper(o, dst, s1, s2) -> 
                match testSrc s1 env, testSrc s2 env with 
                | true, true -> 
                    aux subL (addDst dst true env) (i :: _f) f_ io k
                | false, true -> 
                    let tmp = "_tmp" ^ (string_of_int k) in 
                    let _f  = Move(DIO(tmp,"_tmp",k), s2) :: _f in 
                    let f_ = Oper(o, dst, s1, Var "tmp_") :: Move(DVar "tmp_", IO(tmp,"_tmp",k)) :: f_ in 
                    let k = k + 1 in 
                    aux subL (addDst dst false env) _f f_ (SSet.add tmp io) k
                | true, false ->
                    let tmp = "_tmp" ^ (string_of_int k) in 
                    let _f  = Move(DIO(tmp,"_tmp",k), s1) :: _f in 
                    let f_ = Oper(o, dst, Var "tmp_", s2) :: Move(DVar "tmp_", IO(tmp,"_tmp",k)) :: f_ in 
                    let k = k + 1 in 
                    aux subL (addDst dst false env) _f f_ (SSet.add tmp io) k
                | false, false -> 
                    aux subL (addDst dst false env) _f (i :: f_) io k
        )
    in 
    let _f, f_, io = aux instrs init [] [] (SSet.empty) 0 in 
        (List.rev (clean _f), List.rev (clean f_), io)

let propagate (instrs : inst list) (init : bool SMap.t) = 
    let testSrc (exp : src) env = 
        match exp with
        | Val _ -> true
        | IO(s,_,_) | Var s -> 
            SMap.find s env 
        in              

    let addDst (exp : dst) b env = 
        SMap.add (dstToStr exp) b env
    in

    let aux_propagate (i : inst) (env : bool SMap.t) (out : bool SMap.t) = 
        match i with 
        | Move(dst, src) -> (
            let b = testSrc src env in 
            addDst dst b env, 
            match dst with
            | DIO(s,_,_) -> 
                SMap.add s b out
            | _ -> out
        )

        | Oper(_, dst, s1, s2) -> (
            let b1 = testSrc s1 env in 
            let b2 = testSrc s2 env in 
            let b = b1 && b2 in 
                addDst dst b env , out
        )

    in let rec aux env out = function
    | [] -> out 
    | i :: subL -> 
        let env, out = aux_propagate i env out in 
            aux env out subL
   
    in aux init SMap.empty instrs 

let buildTest (path : string) = 
    Printf.printf "BUILD TEST\n";
    let head = "
import pyboolector
from pyboolector import Boolector
"   in 
    let saveTest = open_out path in 
        output_string saveTest head;
        output_string saveTest

(* Construit un fichier pour jasmin *)
let build (max : int) (src : string) (dst : string) = 
    Printf.printf "BUILD...\n";
    Printf.printf "parsing...\n";

    let pTest = buildTest ("build_test.py") in 

    let (_, instrs) = parse src in
    let instrs = reduce instrs in
    
    Printf.printf "analyse...\n";
    let (accReg, accStack), newProg = analyse instrs SSet.empty (max - 1) in
    
    Printf.printf "analyse test...\n";
    instTest instrs (List.rev newProg) pTest;

    Printf.printf "spilling...\n";
    let toSet acc = List.fold_left (fun s x -> SSet.add x s) SSet.empty acc in
    let progFinal = spilling (List.rev newProg) (toSet accReg) (toSet accStack) in
    
    Printf.printf "spill test...\n";
    instTest (List.rev newProg) progFinal pTest;   
    
    Printf.printf "save...\n";
    slpToJasmin dst (List.rev progFinal) accStack ("tmp" :: accReg) (SSet.empty) 0

let rec clearAcc acc instrs = 
    match acc with
    | [] -> []
    | x :: subL -> 
        if List.for_all (fun i -> (not (SSet.mem x (useN i) || (x = defN i)))) instrs then 
            clearAcc subL instrs
        else
            x :: (clearAcc subL instrs)

let buildUA instrs (dst : string) outs head id pTest =     
    (* let instrs = reduce instrs in *)
    Printf.printf "analyse...\n";

    let (accReg, accStack), newProg = analyse instrs outs 10 in
    Printf.printf "analyse test...\n";

    instTest instrs (List.rev newProg) pTest; 
    let toSet acc = List.fold_left (fun s x -> SSet.add x s) SSet.empty acc in

    Printf.printf "spilling...\n";
    let progFinal = spilling (List.rev newProg) (toSet accReg) (toSet accStack) in

    Printf.printf "spill test...\n";
    instTest (List.rev newProg) progFinal pTest;

    Printf.printf "save...\n";
    slpToC dst (List.rev progFinal) (clearAcc accStack progFinal) ("tmp" :: accReg) head id

let rec iter (k : int) f (b : bool)  = 
    if k > 0 then
        if b then
        (
            f (k - 1) ; iter (k - 1) f b
        )
        else
        (
            iter (k - 1) f b ; f (k - 1)
        )
    else
        ()

let rec findFirst max (k : int) (f : int -> int option) = 
    if k < max then 
        match f k with 
        | Some s -> 
            Some s
        | None -> 
            findFirst max (k + 1) f
    else
        None

let printOrder res n = 
    let b = SMap.find n res in 
    Printf.printf (if b then "1" else "0")

let rec printMatrix list name env b = 
    match list with
    | [] -> printOrder env name
    | x :: subL -> 
        let _ = iter x (fun k -> printMatrix subL (name ^ "_" ^ (string_of_int k)) env b) b in ()

let rec firstInter list name env y start = 
    match list with
    | [] -> 
        if SMap.find name env || y < start then 
            None
        else
            Some y

    | x :: subL -> 
        findFirst x start (fun k -> 
            let nb = y + (List.fold_right ( * ) subL k) in 
            let n = name ^ "_" ^ (string_of_int k) in
            firstInter subL n env nb start 
        )

let countLogic (instrs : inst list) = 
    let rec aux k = function
    | [] -> k
    | Oper(_,_,_,_) :: subL -> 
        aux (k + 1) subL
    | _ :: subL -> 
        aux k subL
    in aux 0 instrs

let renameio ma name old ins = 
    SMap.fold (fun k v m -> 
        let n = match String.split_on_char '_' k with
        | x :: l when x = old -> (String.concat "_" (name :: l))
        | _ -> name in 
            SMap.add n v m
    ) ma ins 

let interference (instrs : inst list) result shape start sizeMax p =
    let addToEnv env out src b =  
        match src with
        | Val _ -> env , out
        | Var s -> SMap.add s b env, out
        | IO(s,n,_) -> SMap.add s b env, if n = "plain" then SMap.add s b out else out
    in

    let rec aux_inter (env : bool SMap.t) (out : bool SMap.t) (instrs : inst list) = 
        match instrs with
        | [] -> out
        | i :: subL -> (
            match i with
            | Move(dst, src) -> 
                let b = SMap.find (dstToStr dst) env in 
                let env, out = addToEnv env out src b in 
                    aux_inter env out subL
            | Oper(_, dst, s1,s2) ->
                let b = SMap.find (dstToStr dst) env in 
                let env, out = addToEnv env out s1 b in 
                let env, out = addToEnv env out s2 b in 
                    aux_inter env out subL
        )
    in 
    
    let rec interf result p = 
        match p with
        | 0 ->
            let env = aux_inter result SMap.empty (List.rev instrs) in 
            (match firstInter shape "plain" env 0 start with
                | None -> sizeMax
                | Some(x) -> x)
        | k -> 
            let env = aux_inter result SMap.empty (List.rev instrs) in 
            let env = renameio env "cipher" "plain" result in 
                interf env (k - 1)
    in
        interf result p  

let getCalcPass k biti bit p2 p1 = 
    let f_ = Float.mul (Float.pow 2. (Float.of_int (biti - k))) (Float.of_int p2) in
    let _f = Float.mul (Float.pow 2. (Float.of_int (k + biti - bit))) (Float.of_int p1) in
    Float.add f_ _f 

let getDatas (src : string) shape max limit sizeMax nbpass = 
    let sexpr = readSexpr src in 
    let pTest = buildTest (src ^ "_test.py") in 

    let (_, _, _), tem = sexprToInst sexpr (fun _ -> false) shape in 

    let aux_get sexpr k = 
        Printf.printf "%d\t" k;
        let (_, ins, _), instrs = sexprToInst sexpr (max k) shape in 
        instTest instrs tem pTest;

        let porte = countLogic instrs in 

        let f_ , _f, _ = generate instrs ins in 

        instTest instrs (f_ @ _f) pTest;

        let result = propagate instrs ins in
        let biti = interference instrs result shape k sizeMax 0 in 
        let porte1 = countLogic f_ in
        let porte2 = countLogic _f in 

        let reference = Float.mul (Float.pow 2. (Float.of_int biti)) (Float.of_int porte) in 
        
        let rec pass ins result p res =   
            if SMap.exists (fun _ v -> v) result && SMap.exists (fun _ v -> not v) result then (
                printMatrix shape "cipher" result false;
                Printf.printf "\t\t";
                let ins = renameio result "plain" "cipher" ins in 
                    let f_ , _f, _ = generate instrs ins in 
                    let result = propagate instrs ins in
                    let bit = interference instrs result shape k sizeMax p in 
                    let porte1 = countLogic f_ in
                    let porte2 = countLogic _f in 
                    let opti = getCalcPass k biti bit porte1 porte2 in
                    instTest instrs (f_ @ _f) pTest;
                    pass ins result (p + 1) (Float.add res opti)
            ) else 
                Float.add res (Float.mul reference (Float.of_int (nbpass - p - 1)))
        in
            let opti = pass ins result 0 (getCalcPass k biti biti porte1 porte2) in 
            let nopti = Float.mul reference (Float.of_int nbpass) in 
            Printf.printf "  %F : %F = %F \n" opti nopti (Float.mul 100. (Float.div (Float.sub opti nopti) nopti))
    in 

    iter limit (fun k -> aux_get sexpr k) false
        

(* Construit un fichier pour jasmin à partir d'un fichier UA0 *)
let buildUA0 (src : string) (dst : string) = 
    Printf.printf "BUILDUA0...\n";
    let sexpr = readSexpr src in 
    let pTest = buildTest "buildUa0_test.py" in 

    Printf.printf "parsing...\n";
    let (head,ins,outs), instrs = sexprToInst sexpr (fun x -> x >= 32) [128] in 

    Printf.printf "generate...\n";

    let f_ , _f, io = generate instrs ins in 

    buildUA f_ (dst ^ "_1") (SSet.union outs io) head 0 pTest;
    buildUA _f (dst ^ "_2") outs head 1 pTest;

    Printf.printf "save f_ _f ...\n";
    let f = f_ @ _f in 
    instTest instrs f pTest

let mainGetDatas() = 
    Printf.printf "aes \n";
    getDatas "test/aes_short.ua0" [128] (fun x k -> x <=  127 - k) 26 128 10;

    Printf.printf "rectangle \n";
    getDatas "test/rectangle_short.ua0" [4;16] (fun x k -> x <=  63 - k) 17 64 10;

    Printf.printf "present \n";
    getDatas "test/present_short.ua0" [64] (fun x k -> x <=  63 - k) 64 64 10;

    Printf.printf "xoodoo \n";
    getDatas "test/xoodoo_short.ua0" [3;4;16] (fun x k -> x <= 75 - k) 24 192 10;
    
    Printf.printf "gift_bitslice \n";
    getDatas "test/gift_bitslice_short.ua0" [4;32] (fun x k -> x <= 127 - k) 32 128 10;

    Printf.printf "photon_bitslice \n";
    getDatas "test/photon_bitslice_short.ua0" [8;8;4] (fun x k -> x <= 87 - k) 24 256 10;

    Printf.printf "ace_bitslice_short \n";
    getDatas "test/ace_bitslice_short.ua0" [5;2;32] (fun x k -> x <= 161 - k) 132 320 10;

    (* Printf.printf "gimli_bitslice \n";
    getDatas "test/gimli_bitslice_short.ua0" [3;4;32] (fun x k -> x <= 107 - k) 16 384 10; *)

;;

let main () =     
    (* doubleMain "test/slp_630.txt" "test/slp_630.jazz" "test/slp_630.c"; *)
    (* buildUA0 "test/aes_short.ua0" "test/rectangle.jazz" *) 
    (* interference "test/aes_short.ua0" *)
    mainGetDatas()
    (* doubleMain "test/slp_630.txt" "test/slp_630.jazz" "test/slp_630.c";
    doubleMain "test/slp_big.txt" "test/slp_big.jazz" "test/slp_big.c" *)
;;

main ()
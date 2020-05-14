open Ast;;

type myval =
  | Intval of int
  | Floatval of float
  | Funval of { param: string; body: expr; env: environment }
  | Objectval of object_t
  | Objecttupleval of string * myval  
  | Unitval of unit

and environment = (string * myval) list
;;

let main_scene = ref [];;

let error msg = raise(Failure msg) ;;

let rec printval = function
  | Intval n -> Printf.printf "%d" n
  | Floatval f -> Printf.printf "%f" f
  | Objectval o ->( match o with
                  | Sphere s -> Printf.printf  "(Sphere((%f,%f,%f),%f,(%d,%d,%d),%f))" 
                                                  s.s_origin.x s.s_origin.y s.s_origin.z
                                                  s.s_radius
                                                  s.s_color.r s.s_color.g s.s_color.b
                                                  s.s_refl
                  | Plane p -> Printf.printf  "(Plane((%f,%f,%f),(%f,%f,%f),(%d,%d,%d),%f))" 
                                                  p.p_origin.x p.p_origin.y p.p_origin.z
                                                  p.p_normal.x p.p_normal.y p.p_normal.z
                                                  p.p_color.r p.p_color.g p.p_color.b
                                                  p.p_refl
                  | Source s ->  Printf.printf  "(Source((%f,%f,%f),(%f,%f,%f),(%d,%d,%d)))" 
                                                  s.l_origin.x s.l_origin.y s.l_origin.z
                                                  s.l_direction.x s.l_direction.y s.l_direction.z
                                                  s.l_color.r s.l_color.g s.l_color.b
                  | Eye e -> Printf.printf  "(Eye((%f,%f,%f),(%f,%f,%f)))" 
                                                  e.e_origin.x e.e_origin.y e.e_origin.z
                                                  e.e_direction.x e.e_direction.y e.e_direction.z 
  )
  | Objecttupleval (n,o) -> Printf.printf "Tuple not printable yet"
  | Funval _ -> Printf.printf "<fun>"
  | Unitval () -> Printf.printf "unit()"
;;

let rec print_to_cpp oc = function 
  | [] -> ""
  | t::q -> (match t with 
            | Objecttupleval (n,o) -> ( match o with
                  | Objectval (Sphere s) -> Printf.fprintf oc "surface surface_%s = {PLAIN, %d, %d, %d, 1000.0,%f,0.0};\nPoint3D origin_%s(%f,%f,%f);\nSphere %s(surface_%s,%f,origin_%s);\nmyObjs.push_back(&%s);\n"
                                                  n
                                                  s.s_color.r s.s_color.g s.s_color.b
                                                  s.s_refl
                                                  n
                                                  s.s_origin.x s.s_origin.y s.s_origin.z
                                                  n
                                                  n
                                                  s.s_radius
                                                  n
                                                  n
                  |Objectval (Plane p) -> Printf.fprintf oc "surface surface_%s = {PLAIN, %d, %d, %d, 1000.0,%f,0.0};\nPoint3D normal_%s(%f,%f,%f);\nPoint3D origin_%s(%f,%f,%f);\nPlan %s(surface_%s,normal_%s,origin_%s);\nmyObjs.push_back(&%s);\n" 
                                                  n
                                                  p.p_color.r p.p_color.g p.p_color.b
                                                  p.p_refl
                                                  n
                                                  p.p_normal.x p.p_normal.y p.p_normal.z
                                                  n
                                                  p.p_origin.x p.p_origin.y p.p_origin.z
                                                  n
                                                  n
                                                  n
                                                  n
                                                  n
                  | Objectval (Source s)->  Printf.fprintf oc "Point3D direction_%s(%f,%f,%f);\nPoint3D color_%s(%d,%d,%d);\nSource %s(direction_%s,color_%s);\n" 
                                                  (* s.l_origin.x s.l_origin.y s.l_origin.z *)
                                                  n
                                                  s.l_direction.x s.l_direction.y s.l_direction.z
                                                  n
                                                  s.l_color.r s.l_color.g s.l_color.b
                                                  n
                                                  n
                                                  n
                  | Objectval (Eye e) -> Printf.fprintf oc "Point3D origin(%f,%f,%f);\nPoint3D originPlan(%f,%f,%f);\n" 
                                                  e.e_origin.x e.e_origin.y e.e_origin.z
                                                  e.e_direction.x e.e_direction.y e.e_direction.z
                  | _ -> error (Printf.sprintf "There should only be objects in the scene") 
  ) 
            |_ -> error (Printf.sprintf "Invalid scene") );
  print_to_cpp oc q
  ;;

(* Environnement. *)
let init_env = [] ;;


let extend rho x v = (x, v) :: rho ;;

let lookup var_name rho =
  try List.assoc var_name rho
  with Not_found -> error (Printf.sprintf "Undefined ident '%s'" var_name)
;;

let rmvfromscene s = 
  let rec aux x l acc =
     match l with
     |[] -> acc
     |Objecttupleval(t,u)::q -> if t = x then (acc@q) else aux x q (acc@[Objecttupleval(t,u)])
     |_ -> error (Printf.sprintf "Can only remove Objecttupleval from scene")
  in 
  main_scene := aux s (!main_scene) [];
  Unitval(())
;;

let addtoscene v = 
      main_scene := v :: (!main_scene); 
      Unitval(())
;;

let counter = ref 0;;


let rec eval e rho =
  match e with
  | EInt n -> Intval n
  | EFloat f -> Floatval f
  | EIdent v ->  
  (match lookup v rho with 
                | Objectval (Sphere s) -> Objecttupleval (v,lookup v rho)
                | Objectval (Plane p) -> Objecttupleval (v,lookup v rho)
                | Objectval (Eye e) -> Objecttupleval (v,lookup v rho)
                | Objectval (Source s) -> Objecttupleval (v,lookup v rho)
                | _ -> lookup v rho
  )
  | EObject o -> ( match o with
                  | Sphere s -> Objectval(Sphere(s))
                  | Plane p -> Objectval(Plane(p))
                  | Eye e -> Objectval(Eye(e))
                  | Source s -> Objectval(Source(s))
                ) 
  | EApp (e1, e2) -> 
  (let (v1,v2) = (eval e1 rho, eval e2 rho) in 
                    match v1 with 
                    |Funval {param = a; body = e; env = rho0} -> eval e (extend rho0 a v2)
                    |_ -> error (Printf.sprintf "Fun value expected"))
  | EMonop ("-", e) -> (
      match eval e rho with
        | Intval n -> Intval (-n)
        | _ -> error (Printf.sprintf "Integer value expected")
  )
  | EMonop (op, _) ->
      error (Printf.sprintf "Unknown unary op: %s" op)
  | EBinop (op, e1, e2) -> (match (eval e1 rho, eval e2 rho) with
                            | (Intval a, Intval b) -> (
                              match op with 
                              | "+" -> Intval (a + b)
                              | "-" -> Intval (a - b)
                              | "*" -> Intval (a * b)
                              | "/" -> Intval (a / b)
                              | _ -> error (Printf.sprintf "Invalid operator for int")
                            )
                             | (Floatval a, Floatval b) -> (
                              match op with 
                              | "+" -> Floatval (a +. b)
                              | "-" -> Floatval (a -. b)
                              | "*" -> Floatval (a *. b)
                              | "/" -> Floatval (a /. b)
                              | _ -> error (Printf.sprintf "Invalid operator for float")
                            )
                            |_ -> error (Printf.sprintf "Invalid expression for binop")
                            )
  | EFun (a, e) -> Funval { param = a; body = e; env = rho }
  | ELet (x, e1, e2) -> let v1 = eval e1 rho in 
                        eval e2 (extend rho x v1)
  | EAdd e -> let v = eval e rho in
              (match v with 
            | Objecttupleval _ -> addtoscene v
            | Objectval o -> addtoscene (Objecttupleval("unnamed"^(string_of_int !counter),v));
                            counter := !counter+1; Unitval(())
            |_ -> error (Printf.sprintf "No object to add")     )    
              
  | ERemove e ->(match e with 
                 | EIdent s -> rmvfromscene s
                 |_ -> error (Printf.sprintf "Cannot remove unnamed objects") )
;;

let eval e = eval e init_env ;;
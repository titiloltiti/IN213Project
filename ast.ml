(* Ce fichier contient la d�finition du type OCaml des arbres de
 * syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
 * du langage.
*)
type point3D = { x : float; y : float ; z : float };;
type color_t = { r : int ; g : int ; b : int};;

type sphere_t = { s_origin : point3D ; s_radius : float ; s_color : color_t; s_refl : float} ;;
type plane_t = { p_origin : point3D ; p_normal : point3D ; p_color : color_t; p_refl : float } ;;
type source_t = { l_origin : point3D ; l_direction : point3D ; l_color : color_t} ;;
type eye_t = { e_origin : point3D ; e_direction : point3D } ;;

type object_t = Sphere of sphere_t | Plane of plane_t | Source of source_t | Eye of eye_t ;;

type expr =
  | EInt of int                                 (* 1, 2, 3 *)
  | EFloat of float                             (* 0., 1.5 *)
  | EIdent of string                            (* x, toto, fact *)
  | EObject of object_t                         (* Sphere, Eye, *)
  | EApp of (expr * expr)                       (* application e1 e2 *)
  | EMonop of (string * expr)                   (* -e *)
  | EBinop of (string * expr * expr)            (* e1 + e2 *)
  | EFun of (string * expr)                     (* fun v -> e *)
  | ELet of (string * expr * expr)              (* let x = e1 in e2 *)
  | EAdd of (expr)                              (* add object to scene *)
  | ERemove of (expr)                           (* remove e from scene // you can remove an object by removing its ident*)
;;


(* Extrait les parametres d'une fonction anonyme
          (fun x1 -> fun x2 -> ... -> e)
   et produit
          ([x1; x2; ...], e)
 *)
let params_body e =
  let rec un_body params expr = match expr with
  | EFun( p, e) -> un_body (p::params) e
  | e -> (List.rev params, e) in
  un_body [] e
;;


(* Note : dans le printf d'OCaml, le format %a
   correspond a 2 arguments consecutifs :
        - une fonction d'impression de type (out_channel -> 'a -> unit)
        - un argument a imprimer, de type 'a
   Voir le cas EApp ci-dessous.
 *)
let rec print oc = function
  | EInt n -> Printf.fprintf oc "%d" n
  | EIdent s -> Printf.fprintf oc "%s" s
  | EFloat f -> Printf.fprintf oc "%f" f
  | EObject o -> ( match o with
                  | Sphere s -> Printf.fprintf oc "(Sphere((%f,%f,%f),%f,(%d,%d,%d),%f))" 
                                                  s.s_origin.x s.s_origin.y s.s_origin.z
                                                  s.s_radius
                                                  s.s_color.r s.s_color.g s.s_color.b
                                                  s.s_refl
                  | Plane p -> Printf.fprintf oc "(Plane((%f,%f,%f),(%f,%f,%f),(%d,%d,%d),%f))" 
                                                  p.p_origin.x p.p_origin.y p.p_origin.z
                                                  p.p_normal.x p.p_normal.y p.p_normal.z
                                                  p.p_color.r p.p_color.g p.p_color.b
                                                  p.p_refl
                  | Source s -> Printf.fprintf oc "(Source((%f,%f,%f),(%f,%f,%f),(%d,%d,%d)))" 
                                                  s.l_origin.x s.l_origin.y s.l_origin.z
                                                  s.l_direction.x s.l_direction.y s.l_direction.z
                                                  s.l_color.r s.l_color.g s.l_color.b
                  | Eye e -> Printf.fprintf oc "(Eye((%f,%f,%f),(%f,%f,%f)))" 
                                                  e.e_origin.x e.e_origin.y e.e_origin.z
                                                  e.e_direction.x e.e_direction.y e.e_direction.z
  )

  | EApp (e1, e2) -> Printf.fprintf oc "(%a %a)" print e1 print e2
  | ELet (f, e1, e2) ->
      let (params, e) = params_body e1 in
      Printf.fprintf oc "(let %s %a= %a in %a)"
        f
        (fun oc -> List.iter (fun s -> Printf.fprintf oc "%s " s)) params
        print e
        print e2
  | EFun (x, e) -> Printf.fprintf oc "(fun %s -> %a)"  x print e
  | EBinop (op,e1,e2) ->
      Printf.fprintf oc "(%a %s %a)" print e1 op print e2
  | EMonop (op,e) -> Printf.fprintf oc "%s%a" op print e
  | EAdd e -> Printf.fprintf oc "(add %a)" print e             
  | ERemove e -> Printf.fprintf oc "(remove %a)" print e              
;;

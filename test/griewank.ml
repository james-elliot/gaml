(* Optimisation de la fonction de Griewank *)
(* L'optimum (101) est obtenu avec un vecteur dont toutes les coordonnées valent 0 *)

(* dimension du problème *)
let dim=10;;

 (*la fonction a minimiser*)
let f x =  
  let sum=ref 0. and prod=ref 1.
  and dim=Array.length x in
  for i=0 to dim-1 do
    sum:= !sum +. x.(i)*.x.(i);
    prod:= !prod*. (cos (x.(i)/.(sqrt (float (i+1)))))
  done;
  let res = (!sum/.4000.) -. !prod in
  let res = if res >100. then 100. else res in
  100. -. res;;
    
module L = struct
  open Gaml.Types

  (*
  let gvars = Gaml.Cfg.read_config "/home/alliot/griewank.cfg"
   *)
  let gvars = {
      seed = 12334;
      ncores = 2;
      nbgens = 1000;
      nbelems = 200;
      pcross = 0.4;
      pmut = 0.4;
      scaling = Ranking;
      elitist = true;
      display_time = false;
      sharing = 0.9;
      complex_sharing = 1.0;
      evolutive = false}
  
  type data = float array
  type user_data= unit
  type result = unit
  exception Fin_AG


  let eval __u __numgen data = 
    (lazy (f data))

  let compare_data = compare
                   
  let generate __u __numgen =
    Array.init dim (fun __i -> -10. +. Random.float 20.0)
      
  let cross __u __numgen a b =
    let dim= Array.length a in
    let newa = Array.copy a
    and newb = Array.copy b 
    and alpha=(Random.float 2.) -.0.5 in
    for i=0 to dim-1 do
      if Random.int 2 =0 then 
	(newa.(i) <- (alpha*. a.(i) +. (1.-.alpha)*. b.(i));
	 newb.(i) <- (alpha*. b.(i) +. (1.-.alpha)*. a.(i)))
      else 
	(newa.(i) <- b.(i) ;newb.(i) <- a.(i)) 
    done;
    (newa,newb)
      
  let mutate __u __numgen a =
    let dim= Array.length a in
    let newa=Array.copy a in
    let i = Random.int dim in
    newa.(i)<-newa.(i) +. (Random.float 1.) -.0.5;
    newa

      
  let distance __u d1 d2 =
    let dim= Array.length d1 in
    let sum=ref 0. in
    for i=0 to dim-1 do
      sum:= !sum+. (d1.(i)-.d2.(i))*.(d1.(i)-.d2.(i))
    done;
    sqrt (!sum/.(float dim))
            
  let barycenter __u d1 n1 d2 n2 =
    let dim= Array.length d1 in
    let f1=float n1 and f2=float n2 in
    let g=Array.make dim 0. in
    for i=0 to dim-1 do
      g.(i) <- ((f1*. d1.(i) +. f2*. d2.(i))/.(f1+. f2))
    done;
    g

  let init __u = 
    Random.init gvars.seed
      
  let prepare_ag __u __pop = ()
    
  let prepare_gen __u __numgen pop =  pop
      
  let after_scale __u __numgen __pop best =
    Array.iter (fun x-> Printf.printf "%f " x) best.data;
    Printf.printf "r_fit=%f" (Lazy.force best.r_fit);
    print_newline();
    ()

  let after_share __u __numgen __pop __clus = ()

  let after_reproduce __u __numgen __pop __prot = ()

  let after_gen __u __numgen __pop = ()
      
  let terminate_ag __u __pop1 __best_elems __nb_done = ()
      
end
    
module M = Gaml.Optimize.Make(L);;


let __start_ag  = M.opti () ;;



open Array;;

type gene = float ;;
type chrom = gene array ;;
type pop = chrom array ;;

let pop_size = 10; ;;
let chrom_size = 4 ;;

let pm = 0.25 ;;

let init_pop (): pop =
  (init pop_size
     (fun i -> init chrom_size (fun i -> float_of_int(Random.int 50))));;

let fitness (c : chrom) : float =
  abs_float( 1./.(1.+.(get c 0)+. 2.*.(get c 1) +. 3.*.(get c 2) +. 4.*.(get c 3) -. 50.)) ;;

let fit_scores (p : pop) : float array = (init pop_size (fun i -> (fitness (get p i))));;

let selection (p : pop) (scores : float array) : pop =
  let fittest : int array =
    (init pop_size (fun i ->
         let fighters = (init 4 (fun j -> (Random.int pop_size))) in
         (fold_right
            (fun a b-> if (get scores a) >= (get scores b) then a else b)
            fighters (get fighters 3))))
  in (map (fun i -> (get p i)) fittest) ;;

let crossover (p : pop) : pop =
  let cross (a : chrom) (b : chrom) (point : int): pop =
    [|(init chrom_size (fun i -> if i<=point then (get a i) else (get b i)));
      (init chrom_size (fun i -> if i<=point then (get b i) else (get a i)))|] in
    let rec loop (j : int) =
      if j < (pop_size/2) then
        (append (cross (get p (Random.int pop_size))
                       (get p (Random.int pop_size))
                       (Random.int chrom_size)) (loop (j+1)))
      else [||] in (loop 0) ;;

let mutation (p : pop) : pop =
  let mut_chrom = (init (int_of_float (pm*.(float_of_int chrom_size)*.(float_of_int pop_size))) (fun i -> (Random.int pop_size))) in
  (mapi (fun i a -> if (mem i mut_chrom) then
            (mapi  (fun i b -> if ((Random.int chrom_size) = i) then (float_of_int(Random.int 50)) else b) a) else a) p);;

let fit_comp (a : chrom) (b : chrom) : int = -1*(compare (fitness a) (fitness b)) ;;

let print (p : pop) : unit =
  let sum (c : chrom) : float = (1. +.(get c 0)+. 2.*.(get c 1) +. 3.*.(get c 2) +. 4.*.(get c 3)) in
  (iter (fun a -> (iter (fun b -> (print_string ((string_of_float b)^"\t"))) a);
          (print_string ("= "^(string_of_float(sum a))^"\n"))) p);;

let starti (gens:int)=
  let rec loop (g : int) (p : pop)=
    if g < (gens) then
      (loop (g+1) (mutation (crossover (selection p (fit_scores p)))))
    else ((sort fit_comp p); (print p))
  in (loop 0 (init_pop ())) ;;

let start () =
  let rec loop (g : int) (p : pop)=
    if (exists (fun c -> ((1.+.(get c 0)+. 2.*.(get c 1) +. 3.*.(get c 2) +. 4.*.(get c 3)) = 50.)) p)
    then (((sort fit_comp p);
           (print_string ("Terminated on generation "^(string_of_int g)^"\n"));(print p)))
    else (loop (g+1)  (mutation (crossover (selection p (fit_scores p)))))
  in (loop 0 (init_pop ())) ;;

(* start ();; *)

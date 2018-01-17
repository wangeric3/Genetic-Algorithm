let c1 = [|13.; 12.; 23.; 36.|];;
let c2 = [|40.; 10.; 7.; 1.|] ;;
let scores1 =
[|0.00497512437810945264; 0.00330033003300330037; 0.00510204081632653;
  0.00409836065573770513; 0.0277777777777777762; 0.00323624595469255679;
  0.0119047619047619041; 0.00354609929078014176; 0.0188679245283018861;
  0.012048192771084338|] ;;

let mutation (p : pop) : pop =
  let mut_chrom = (init (int_of_float (pm*.(float_of_int chrom_size)*.(float_of_int pop_size))) (fun i -> (Random.int pop_size))) in
      (mapi (fun i a -> if (mem i mut_chrom) then (set a (Random.int chrom_size) (float_of_int(Random.int 50))) else ()) p);p;;

let prob (p : pop) : float array =
  let fit_scores : float array = (Array.init pop_size (fun i -> (fitness (get p i)))) in
  let total : float = (fold_right (fun a b-> a +. b) fit_scores 0.) in
  (map (fun i -> i/.total) fit_scores) ;;
let init_pop = make_matrix 10 4 0 in
  (let rec set_chrom (j : int) (p : pop): unit=
     if (j = (pop_size-1)) then () else
      let rec set_genes (i : int) (c : chrom): unit =
        if (i = (chrom_size-1)) then ()
        else (set c i (Random.int 50));(set_genes (i+1) c)
      in (set_genes 0 (get p j));(set_chrom (j+1) p)
  in (set_chrom 0 init_pop)) ;;

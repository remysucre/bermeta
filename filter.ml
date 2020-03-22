let filter : float array -> float array -> float array = fun b x ->
  let m = Array.length b in
  let y i =
    if i < m-1 then x.(i) else
      let sum = ref 0.0 in
      for k = 0 to m-1 do
        sum := !sum +. b.(k) *. x.(i-k)
      done;
      !sum
  in
  Array.init (Array.length x) y         (* essentially, for-loop *)
;;

let impulses : int -> int -> int -> float array = fun i j n ->
  let x = Array.make n 0.0 in
  x.(i) <- 1.0;
  x.(j) <- 1.0;
  x
;;

type spine_arr = float code array
type dyn_arr = float array code

let filter_spine : spine_arr -> (float array -> float array) code =
  fun b -> .<fun x ->
    .~(let m = Array.length b in
       .<let y i =
           if i < m-1 then x.(i) else
             .~(let sum = ref .<0.0>. in
             for k = 0 to m-1 do
               sum := .<.~(!sum) +. .~(b.(k)) *. x.(i-k)>.
             done;
             !sum)
       in
       Array.init (Array.length x) y
       >.)>.
;;

let filter_staged : float array -> (float array -> float array) code =
  fun b -> .<fun x ->
    .~(let m = Array.length b in
       .<let y i =
           if i < m-1 then x.(i) else
             .~(let sum = ref .<0.0>. in
                for k = 0 to m-1 do
                  let bk = b.(k) in
                  sum := .<.~(!sum) +. bk *. x.(i-k)>.
                done;
                !sum)
       in
       Array.init (Array.length x) y
       >.)>.
;;

let of_spine_list : float code list -> float list code = fun xs ->
  List.fold_right (fun x acc -> .<.~x :: .~acc>.) xs .<[]>.
;;

let dyn_arr_of_spine_arr : spine_arr -> dyn_arr = fun xs ->
  .<Array.of_list
    .~(Array.fold_right (fun x acc -> .<.~x :: .~acc>.) xs .<[]>.) >.
;;

let filter_spine_adaptive ?(threshold = 3) :
  spine_arr -> (float array -> float array) code = fun b ->
  let m = Array.length b in
  if m <= threshold then
    filter_spine b
  else
    .<let b_dyn = .~(dyn_arr_of_spine_arr b) in
    fun x ->
      let y i =
        if i < m-1 then x.(i) else
          let sum = ref 0.0 in
          for k = 0 to m-1 do
            sum := !sum +. b_dyn.(k) *. x.(i-k)
          done;
          !sum
      in
      Array.init (Array.length x) y>.
;;

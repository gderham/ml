(* Matrix utility functions. *)

open Core.Std;;

(* Apply a function f to a matrix (list list) to create a new matrix. *)
let transform f matrix = List.map matrix (fun l -> List.map l f)

(* Convert a list list to an Array Array. *)
let ll_to_aa ll = Array.of_list (List.map ll Array.of_list)

(* "Devectorise" a 1 x w list to a m x n matrix where m * n = w *)
let list_to_matrix list num_cols =
  if List.length list mod num_cols <> 0 then
    invalid_arg "num_cols must divide the size of list."
  else 
    let rec aux acc_matrix curr_row i = function
      | [] -> acc_matrix
      | h :: t ->
        let new_row = h :: curr_row in
        if i mod num_cols = 0 then aux (List.rev new_row :: acc_matrix) []      (i+1) t
        else                       aux acc_matrix                       new_row (i+1) t  in
    List.rev (aux [] [] 1 list)

(* Apply a function to each element of a matrix (list list) columnwise. *)
let rec apply f ll =
  match ll with
  | [] -> []
  | ll when List.mem ll [] -> []
  | ll -> f (List.map ll List.hd_exn) :: apply f (List.map ll List.tl_exn)

let id (x:int list) = x;; (* why is the type info required? *)

(* Transpose a list list. *)
let transpose = apply id;;

(* Apply a function to each element of a matrix (list list) *)
let apply_matrix f m = List.map m (fun row -> (List.map row (fun x -> f x)));;

(* Hadamard product for two vectors *)
let componentwise_product v1 v2 = List.map2_exn v1 v2 ( * );;

(* Standard matrix multiplication nxm * mxp -> nxp. *)
let multiply m1 m2 =
  List.map m1 (fun row ->
      apply
        (fun col -> List.fold_left (componentwise_product row col) ~init:0 ~f:(+))
        m2
    )

(* Scale a matrix. *)
let scale s = apply_matrix (fun x -> s * x)

(* Add two matrices of the same size *)
let add m1 m2 =
  List.map2_exn m1 m2 (fun a b -> List.map2_exn a b (+))

(* Combining two vectors of size 1xn and mx1 -> n x m matrix. *) 
let bsx v1 v2 f =
  List.map v1 (fun a -> List.map v2 (fun b -> f a b))

let bsx_add v1 v2 = bsx v1 v2 (+)

(* convert vector into matrix *)
let matrixify v =
  List.map v (fun r -> [r])

let square = apply_matrix (fun x -> x * x)

let sum_rows m = (* generate a vector of the row sums of a matrix *)
  List.map m (fun row -> List.fold row ~init:0 ~f:(+))

(* Create sum of squares vector for a matrix. *)
let sos m = m |> square |> sum_rows

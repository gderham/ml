(* Matrix utility functions. *)

(* Apply a function f to a matrix (list list) to create a new matrix. *)
let transform f matrix = List.map (List.map f) matrix

(* Convert a list list to an Array Array. *)
let ll_to_aa ll = Array.of_list (List.map Array.of_list ll)

(* Convert a 1 x w list to a m x n matrix where m * n = w *)
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

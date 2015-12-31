(* Use the K nearest neighbour algorithm to classify handwritten digits in the MNIST database.

  1. Save the MNIST database files locally:

   http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

  2. To compile:

   ocamlfind ocamlc -linkpkg -thread -syntax camlp4o -package bitstring.syntax -package bitstring,graphics matrix.ml mnist.ml display.ml knn.ml

  3. To run in toplevel:

   #camlp4o;; 
   #require "bitstring.syntax";; x 2
   #require "graphics";;

   #mod_use "matrix.ml";;
   #mod_use "display.ml";;
   #mod_use "mnist.ml";;
   #mod_use "knn.ml";;

*)

open Mnist
open Core.Std


let base_dir = "/Users/guy/repos/ml/mnist/"

let training_labels_filename = base_dir ^ "train-labels-idx1-ubyte"
let training_images_filename = base_dir ^ "train-images-idx3-ubyte"
let test_labels_filename = base_dir ^ "t10k-labels-idx1-ubyte"
let test_images_filename = base_dir ^ "t10k-images-idx3-ubyte"

let get_training_labels = get_labels training_labels_filename
let get_training_images = get_images training_images_filename
let get_test_labels = get_labels test_labels_filename
let get_test_images = get_images test_images_filename

(* Hadamard product for two vectors *)
let componentwise_product v1 v2 = List.map2_exn v1 v2 ( * );;

let rec apply f (ll : int list list) =
  match ll with
  | [] -> []
  | ll when List.mem ll [] -> []
  | ll -> f (List.map ll List.hd_exn) :: apply f (List.map ll List.tl_exn)
;;

let id x = x;;

let transpose = apply id;;

let multiply m1 m2 =
  (* Transpose m2's rows and multiply with the rows of m1. *) 
  List.map m1 (fun row ->
      apply
        (fun col -> List.fold_left (componentwise_product row col) ~init:0 ~f:(+))
        m2
    )
;;

let m_sq m = (* squares each element of a matrix *)
  let sq list = List.map list (fun elm -> elm * elm) in
  List.map m (fun row -> sq row)
;;

let m_sum m = (* generate a vector of the row sums of a matrix *)
  List.map m (fun row -> List.fold row ~init:0 ~f:(fun acc elm -> acc + elm))
;;



(* Display one of the digits from the database *)
let () =
  let _, training_images = Knn.get_training_images in
 
  List.nth training_images 10 |> Display.display_image
;;


(* choose a single test digit *)
let d_te = [[0;0;0;4];[0;2;0;0]] in

(* compute euclidean distance to all the training digits *)

let d_tr = [[0;0;0;5];[0;3;0;0];[1;0;0;0]] in

(* compute sum of squares *)

let sos m = m_sum (m_sq m) in 

let d_tr_sos = sos d_tr in
let d_te_sos = sos d_te in


d_tr_sos, m_transpose [d_te_sos];;

(* find the min distance *)


(* find the corresponding label *)




(*
2. Split into training and test data
3. For each test digit, compute the euclidean distance to all digits in the training set
4. For K=1, use pick the closest digit
5. Compute the misclassification rate on the test set.
6. Try various preprocessing and see how this affects the misclassification rate.

*)


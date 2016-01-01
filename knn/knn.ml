(* Use the K nearest neighbour algorithm to classify handwritten digits in the MNIST database.

  1. Save the MNIST database files locally:

   http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

   and set base_dir (below) to the save directory.

  2. To compile:

   ocamlfind ocamlc -linkpkg -thread -syntax camlp4o -package bitstring.syntax -package bitstring,core,graphics matrix.ml mnist.ml display.ml knn.ml

   3. To run in toplevel:

   #camlp4o;; 
   #require "bitstring.syntax";; x 2
   #require "graphics";;

   #mod_use "matrix.ml";;
   #mod_use "display.ml";;
   #mod_use "mnist.ml";;
   #mod_use "knn.ml";;

*)

open Core.Std
open Matrix
open Mnist
open Printf

(* Find the squared Euclidean distance between two sets of points in r dimensions.
   p is a mxr matrix of m points in r dimensional space.
   q is a nxr matrix of n points in r dimensional space.
   The result is an mxn matrix of the squared distances from the m points to the n points.
*)
let squared_euclidean_distance p q =
  add
    (bsx_add (sos p) (sos q))
    (scale (-2) (multiply p (transpose q)))

(* Find a list's minimum value and its zero based index. *)
let find_min_index_val list =
  if list = [] then
    failwith "Cannot find minimum value of empty list."
  else
    let find_min curr_idx idx_min x =
      match idx_min with
      | (-1,_) -> (curr_idx, x)
      | (i,m) when x < m -> (curr_idx, x)
      | _ -> idx_min in
    List.foldi list ~f:find_min ~init:(-1,0)

(* For an nxm matrix, find the minimum value of each row (the closest digit), and
   return a corresponding 2xm matrix of (index,value). The index starts at 0.
*)
let min_index m = List.map m (fun row -> find_min_index_val row)

(* Compare two lists (of the same length) and return the number of structurally
   mismatching elements.
*)
let count_mismatches al bl =
  List.fold2_exn al bl ~init:0 ~f:(fun misses a b -> if a <> b then misses+1 else misses)


let base_dir = "/Users/guy/repos/ml/mnist/"

let training_labels_filename = base_dir ^ "train-labels-idx1-ubyte"
let training_images_filename = base_dir ^ "train-images-idx3-ubyte"
let test_labels_filename = base_dir ^ "t10k-labels-idx1-ubyte"
let test_images_filename = base_dir ^ "t10k-images-idx3-ubyte"

let get_training_labels = get_labels training_labels_filename
let get_training_images = get_images training_images_filename
let get_test_labels = get_labels test_labels_filename
let get_test_images = get_images test_images_filename


let () =

  printf "Running 1NN classification of MNIST handwritten digits.\n%!";

  (* Takes about 40s on my MBP. *)
  let train_size = 5000 in 
  let test_size  = 20 in

  printf "Training set size = %d\n%!" train_size;
  printf "Test set size = %d\n%!" test_size;

  printf "Loading training data..\n%!";
  let _,train_digits = get_training_images in
  let train_digits = List.slice train_digits 0 train_size in
  let train_labels = List.slice get_training_labels 0 train_size in

  printf "Loading test data..\n%!";
  let _,test_digits  = get_test_images in
  let test_digits = List.slice test_digits 0 test_size in
  let test_labels = List.slice get_test_labels 0 test_size in

  printf "Computing predicted test digits..\n%!";
  (* Find the distance from all test digits to all training digits. *)
  let squared_distances = squared_euclidean_distance test_digits train_digits in

  (* For each test digit find the closest training digit. *)
  let closest_digits = min_index squared_distances in

  (* Look up the labels for the closest training digit. *)
  let predicted_test_labels = List.map closest_digits (fun (idx,min_dist) -> List.nth_exn train_labels idx) in

  let num_misses = count_mismatches test_labels predicted_test_labels in

  let misclassification_rate = float num_misses /. float test_size in

  printf "Misclassification rate = %f\n%!%!" misclassification_rate;

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

let base_dir = "/Users/guy/repos/ml/mnist/"

let training_labels_filename = base_dir ^ "train-labels-idx1-ubyte"
let training_images_filename = base_dir ^ "train-images-idx3-ubyte"
let test_labels_filename = base_dir ^ "t10k-labels-idx1-ubyte"
let test_images_filename = base_dir ^ "t10k-images-idx3-ubyte"

let get_training_labels = get_labels training_labels_filename
let get_test_labels = get_labels test_labels_filename
let get_training_images = get_images training_images_filename
let get_test_images = get_images test_images_filename

(* Display one of the digits from the database *)
let () =
  let _, training_images = get_training_images in
  List.nth training_images 10 |> Display.display_image


(*
2. Split into training and test data
3. For each test digit, compute the euclidean distance to all digits in the training set
4. For K=1, use pick the closest digit
5. Compute the misclassification rate on the test set.
6. Try various preprocessing and see how this affects the misclassification rate.

*)


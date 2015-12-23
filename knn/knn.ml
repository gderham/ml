(* Use the K nearest neighbour algorithm to classify handwritten digits in the MNIST database.

  1. Save the MNIST database files locally:

   http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

   (see http://yann.lecun.com/exdb/mnist/)

  2. To run from toplevel:

   utop[1]> #camlp4o;;                                                                                               utop[2]> #require "bitstring";;                                                                                   utop[3]> open Bitstring;;        
   utop[4]> #require "bitstring.syntax";;                                                                            
*)

open Bitstring;;
open Printf;;


let read_header pkt =
  bitmatch pkt with
  | { 0               : 16; (* no meaning *) 
      data_type       : 8;  (* 0x08 = unsigned byte, 09 = signed byte, 0B = short, 0C = int, 0D = float, 0E = double *)
      num_dims        : 8;  (* number of dimensions (1=vector, 2=matrix etc) *)
      num_dim1_items  : 32; (* length of dimension 1 *)
      rest            : -1 : bitstring (* the list of labels *)
    } -> data_type, num_dims, num_dim1_items, rest
  | { _ } -> failwith "Failed to parse header."
;;

let read_labels bits =
  let rec loop acc bits = (* TODO: rewrite not as a loop, and raise error on malformed input *)
      bitmatch bits with
      | { label : 8;
          rest  : -1 : bitstring
        } -> loop (label::acc) rest
      | { _ } -> acc in
  List.rev (loop [] bits)
;;

(* Write a fn that doesn't use a loop, and reads the number of known records. *)

let base_dir = "/Users/guy/repos/ml/mnist/";;
let training_labels_filename = base_dir ^ "train-labels-idx1-ubyte";;
let training_images_filename = base_dir ^ "train-images-idx3-ubyte";;
let test_labels_filename = base_dir ^ "t10k-labels-idx1-ubyte";;
let test_images_filename = base_dir ^ "t10k-images-idx3-ubyte";;

let get_labels filename = 
  let bits = Bitstring.bitstring_of_file filename in
  let data_type, num_dims, num_dim1_items, rest = read_header bits in
  read_labels rest
;;

let get_training_labels =
  get_labels training_labels_filename
;;

let get_test_labels =
  get_labels test_labels_filename
;;

let get_training_images =
  let bits = Bitstring.bistring_of_file training_images_filename in
  let blah, rest = read_images_header bits in
  read_images rest
;;


(*
2. Split into training and test data
3. For each test digit, compute the euclidean distance to all digits in the training set
4. For K=1, use pick the closest digit
5. Compute the misclassification rate on the test set.
6. Try various preprocessing and see how this affects the misclassification rate.

*)


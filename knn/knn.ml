(* Using the K nearest neighbour algorithm to classify handwritten digits in the MNIST database. *)

(* To run from toplevel:

utop[1]> #camlp4o;;                                                                                               
utop[3]> #require "bitstring";;                                                                                   utop[4]> #require "bitstring.syntax";;                                                                            utop[5]> open Bitstring;;        

*)

(* 1. Load the MNIST data (http://yann.lecun.com/exdb/mnist/) into some list. *)

(* a. Training set label file *)

open Bitstring;;
open Printf;;


let display pkt =
  bitmatch pkt with
  | { 0               : 16; (* no meaning *) 
      data_type       : 8;  (* 0x08 = unsigned byte, 09 = signed byte, 0B = short, 0C = int, 0D = float, 0E = double *)
      num_dims        : 8;  (* number of dimensions (1=vector, 2=matrix etc) *)
      num_dim1_items  : 32; (* length of dimension 1 *)
      rest            : -1 : bitstring (* the list of labels *)
    } -> data_type, num_dims, num_dim1_items, rest
  | { _ } -> failwith "arrrrrrrrrrrrrrrrrrrrr"
;;

let () =

  let training_labels_filename = "/Users/guy/repos/ml/mnist/t10k-labels-idx1-ubyte" in
  
  let pkt = Bitstring.bitstring_of_file training_labels_filename in

  let data_type, num_dims, num_dim1_items, rest = display pkt in

  printf "%d\n" num_dims;
  printf "%li\n" num_dim1_items;
  print_endline "";;
  (*
  Bitstring.hexdump_bitstring stdout rest *)
;;





(*
2. Split into training and test data
3. For each test digit, compute the euclidean distance to all digits in the training set
4. For K=1, use pick the closest digit
5. Compute the misclassification rate on the test set.
6. Try various preprocessing and see how this affects the misclassification rate.

*)


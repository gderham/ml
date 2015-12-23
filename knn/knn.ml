(* Using the K nearest neighbour algorithm to classify handwritten digits in the MNIST database. *)

(* To run from toplevel:

utop[1]> #camlp4o;;                                                                                               
utop[2]> #require "bitstring";;                                                                                   utop[3]> open Bitstring;;        
utop[4]> #require "bitstring.syntax";;                                                                            
*)

(* 1. Load the MNIST data (http://yann.lecun.com/exdb/mnist/) into some list. *)

(* a. Training set label file *)

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
  let rec loop acc bits =
    (*    let label, rest =  *)
      bitmatch bits with
      | { label : 8;
          rest  : -1 : bitstring
        } -> loop (label::acc) rest
      | { _ } -> acc in

        (* Bitstring.hexdump_bitstring stdout bits;failwith "Failed to parse labels" in*)
          (*    loop (label :: acc) rest in  *)
  loop [] bits
;;

(* Write a fn that doesn't use a loop, and reads the number of known records. *)



let readdata =

  let training_labels_filename = "/Users/guy/repos/ml/mnist/t10k-labels-idx1-ubyte" in
  
  let pkt = Bitstring.bitstring_of_file training_labels_filename in

  let data_type, num_dims, num_dim1_items, rest = read_header pkt in

  (*  printf "%d\n%li\n" num_dims num_dim1_items in  *)

  let labels = read_labels rest in
  labels
  (* List.iter (fun x -> printf "%d\n" x) labels;; *)



  (* print_endline ""*)

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


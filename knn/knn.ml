(* Use the K nearest neighbour algorithm to classify handwritten digits in the MNIST database.

  1. Save the MNIST database files locally:

   http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz
   http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz

   (see http://yann.lecun.com/exdb/mnist/)

  2. To run from toplevel run these first:

   utop[1]> #camlp4o;;
   utop[2]> #require "bitstring";;                                                                                
   utop[3]> #require "bitstring.syntax";;                                             
*)

#camlp4o;;
#require "bitstring";;                                                                         
#require "bitstring.syntax";;

open Bitstring;;
open Printf;;


let pixmap_width, pixmap_height = 28,28;;
let pixmap_num_pixels = pixmap_width * pixmap_height;;


let read_labels_header pkt =
  bitmatch pkt with
  | { 0               : 16; (* Blank. *) 
      data_type       : 8;  (* 0x08 = unsigned byte *)
      num_dims        : 8;  (* number of dimensions (1=vector, 2=matrix etc) *)
      num_dim1_items  : 32; (* length of dimension 1 *)
      rest            : -1 : bitstring (* the list of labels *)
    } -> data_type, num_dims, num_dim1_items, rest
  | { _ } -> failwith "Failed to parse header."
;;

let read_labels bits =
  let parse_one_label bits =
      bitmatch bits with
      | { label : 8;
          rest  : -1 : bitstring
        } -> Some label, rest
      | { _ } -> None, empty_bitstring in
  let rec loop acc bits =
    let label, remainder = parse_one_label bits in
    match label with
    | Some l -> loop (l :: acc) remainder
    | None -> acc in
  List.rev (loop [] bits)
;;

let read_magic_number bits =
  bitmatch bits with
  | { 0               : 16; (* no meaning *) 
      data_type       : 8;  (* 0x08 = unsigned byte, 09 = signed byte, 0B = short, 0C = int, 0D = float, 0E = double *)
      num_dims        : 8;  (* number of dimensions (1=vector, 2=matrix etc) *)
      rest            : -1 : bitstring (* the image data *)
    } -> data_type, num_dims, (*dim_sizes,*) rest
  | { _ } -> failwith "Failed to parse header."
;;

let read_images_header bits =
  bitmatch bits with
  | { num_images : 32;
      num_rows   : 32;    
      num_cols   : 32;
      rest       : -1 : bitstring
    } -> num_images, num_rows, num_cols, rest
  | { _ } -> failwith "Failed to parse header"
;;

let parse_pixmap_into_list bits =

  let parse_next_pixel bits =
    bitmatch bits with
    | { pixel : 8;
        rest  : -1 : bitstring
      } -> pixel, rest
    | { _ } -> failwith "Failed to parse pixmap" in

  let rec loop acc_pixels pix_num bits =
    if pix_num = 0 then
      acc_pixels
    else
      let pixel, rest = parse_next_pixel bits in
      loop (pixel :: acc_pixels) (pix_num-1) rest in

  List.rev (loop [] pixmap_num_pixels bits)
;;

let list_to_matrix list num_cols = (* convert 1xw list to mxn matrix where m*n=w *)
  if List.length list mod num_cols <> 0 then
    failwith "num_cols must divide the size of list."
  else 
    let rec aux acc_matrix curr_row i = function
      | [] -> acc_matrix
      | h :: t ->
        let new_row = h :: curr_row in
        if i mod num_cols = 0 then aux (List.rev new_row :: acc_matrix) []      (i+1) t
        else                       aux acc_matrix                       new_row (i+1) t  in
    List.rev (aux [] [] 1 list)
;; 
  

let read_images bits num_images = (* Read images into a list of float list where each int list is 24x24 pixel densities. *)

  let read_image bits = (* parse one 24 x 24 pixmap from the binary data *)
    bitmatch bits with
    | { pixmap  : 8 * pixmap_num_pixels : bitstring;
        rest   : -1 : bitstring} -> pixmap, rest
    | { _ } -> failwith "Failed to parse images" in

  let rec loop acc_images num_images bits =
    if num_images = 0 then
      acc_images
    else
      let pixmap, rest = read_image bits in
      let row = parse_pixmap_into_list pixmap in
      let matrix = list_to_matrix row pixmap_width in
      loop (matrix :: acc_images) (num_images-1) rest in

  List.rev (loop [] num_images bits)
;;


let base_dir = "/Users/guy/repos/ml/mnist/";;
let training_labels_filename = base_dir ^ "train-labels-idx1-ubyte";;
let training_images_filename = base_dir ^ "train-images-idx3-ubyte";;
let test_labels_filename = base_dir ^ "t10k-labels-idx1-ubyte";;
let test_images_filename = base_dir ^ "t10k-images-idx3-ubyte";;

let get_labels filename = 
  let bits = Bitstring.bitstring_of_file filename in
  let data_type, num_dims, num_dim1_items, rest = read_labels_header bits in
  read_labels rest
;;

let get_training_labels =
  get_labels training_labels_filename
;;

let get_test_labels =
  get_labels test_labels_filename
;;

let get_training_images =
  let bits = Bitstring.bitstring_of_file training_images_filename in
  let data_type, num_dims, rest = read_magic_number bits in
  let num_images, num_rows, num_cols, rest = read_images_header rest in
  let images = read_images rest (Int32.to_int num_images) in
  num_images, num_rows, num_cols, images
;;

#require "graphics";;
open Graphics;;

let display_image pixmap = (* pixels is a rectangular list list int (pixel density) *)

  let get_color d = rgb d d d in

  let colorize pixmap =
    List.map (List.map get_color) pixmap in
  
  let ll_to_aa ll = Array.of_list (List.map Array.of_list ll) in

  let create_window =
    begin
      Graphics.open_graph " 28x28";
      Graphics.set_window_title "some label";
    end
  in

  let () = create_window in
  
  let image = Graphics.make_image (ll_to_aa (colorize pixmap)) in
  Graphics.draw_image image 0 0;
      
  (* then Graphics.close_graph *)
;;


(*
2. Split into training and test data
3. For each test digit, compute the euclidean distance to all digits in the training set
4. For K=1, use pick the closest digit
5. Compute the misclassification rate on the test set.
6. Try various preprocessing and see how this affects the misclassification rate.

*)


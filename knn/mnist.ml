(*
#camlp4o;;
#require "bitstring";;                                                                         
#require "bitstring.syntax";;
*)
open Bitstring;;

(* MNIST handwritten digit database file parsing

   (see http://yann.lecun.com/exdb/mnist/)
*)

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

let read_magic_number bits =
  bitmatch bits with
  | { 0               : 16; (* no meaning *) 
      data_type       : 8;  (* 0x08 = unsigned byte *)
      num_dims        : 8;  (* number of dimensions (1=vector, 2=matrix etc) *)
      rest            : -1 : bitstring (* the image data *)
    } -> data_type, num_dims, rest
  | { _ } -> failwith "Failed to parse header."

let read_images_header bits =
  bitmatch bits with
  | { num_images : 32;
      num_rows   : 32;    
      num_cols   : 32;
      rest       : -1 : bitstring
    } -> num_images, num_rows, num_cols, rest
  | { _ } -> failwith "Failed to parse header"

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
      let matrix = Matrix.list_to_matrix row pixmap_width in
      loop (matrix :: acc_images) (num_images-1) rest in

  List.rev (loop [] num_images bits)

let get_labels filename = 
  let bits = Bitstring.bitstring_of_file filename in
  let data_type, num_dims, num_dim1_items, rest = read_labels_header bits in
  read_labels rest

let get_images filename =
  let bits = Bitstring.bitstring_of_file filename in
  let data_type, num_dims, rest = read_magic_number bits in
  let num_images, num_rows, num_cols, rest = read_images_header rest in
  let images = read_images rest (Int32.to_int num_images) in
  num_images, images
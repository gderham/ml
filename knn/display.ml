(* #require "graphics";; *)
(* Requires the OCaml install to include the graphics package / X11 option *)
open Graphics
open Matrix


let greyscale d = rgb d d d

(* Convert a pixmap to an 8 bit greyscale Graphics.image. *)
let pixmap_to_image pixmap =
  pixmap
  |> transform greyscale
  |> ll_to_aa
  |> make_image

(* Displays a rectangular pixmap (int list list) on the screen *)
let display_image pixmap =
  
    open_graph " 28x28"; (* TODO: Infer from pixmap *)
    set_window_title "Digit";

    draw_image (pixmap_to_image pixmap) 0 0;
    read_key () |> ignore;
    close_graph ();

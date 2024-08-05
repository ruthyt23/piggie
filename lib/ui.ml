open! Core
open! Async
open! Fzf
open Curses

let prerr err =
  match err with
  | false ->
    endwin ();
    failwith "Error!"
  | true -> ()
;;

module Input = struct
  type t =
    | Up
    | Down
    | Escape
    | Enter
    | Invalid

  let of_int value =
    match value with
    | 65 -> Up
    | 66 -> Down
    | 27 -> Escape
    | 10 -> Enter
    | _ -> Invalid
  ;;
end

module State_manager = struct
  module Current_state = struct
    type t =
      | Selecting_Commodity
      | Selecting_Quantity
    [@@deriving equal]
  end

  type t =
    { mutable hand_cursor : int
    ; mutable quantity_cursor : int
    ; mutable hand_selection_list : Commodity.t list
    ; quantity_selection_list : int list
    ; mutable state : Current_state.t
    ; mutable curr_commodity_selected : Commodity.t option
    ; mutable curr_quantity_selected : int option
    }

  let init () =
    { hand_cursor = 0
    ; quantity_cursor = 0
    ; hand_selection_list = []
    ; quantity_selection_list = [ 1; 2; 3; 4 ]
    ; state = Selecting_Commodity
    ; curr_commodity_selected = None
    ; curr_quantity_selected = None
    }
  ;;

  let reset t =
    t.state <- Selecting_Commodity;
    t.quantity_cursor <- 0;
    t.hand_cursor <- 0;
    t.curr_commodity_selected <- None
  ;;

  let move_up t =
    match t.state with
    | Selecting_Commodity ->
      t.hand_cursor
      <- (t.hand_cursor - 1) % List.length t.hand_selection_list
    | Selecting_Quantity -> t.quantity_cursor <- (t.quantity_cursor - 1) % 4
  ;;

  let move_down t =
    match t.state with
    | Selecting_Commodity ->
      t.hand_cursor
      <- (t.hand_cursor + 1) % List.length t.hand_selection_list
    | Selecting_Quantity -> t.quantity_cursor <- (t.quantity_cursor + 1) % 4
  ;;

  let handle_enter t =
    match t.state with
    | Selecting_Commodity ->
      t.curr_commodity_selected
      <- List.nth t.hand_selection_list t.hand_cursor;
      t.state <- Selecting_Quantity
    | Selecting_Quantity ->
      t.curr_quantity_selected
      <- List.nth t.quantity_selection_list t.quantity_cursor
  ;;

  let handle_escape t = reset t
end

type t =
  { parent_window : window
  ; height : int
  ; width : int
  ; trade_parent_window : window
  ; commodity_window : window
  ; quantity_window : window
  ; hand_window : window
  ; book_window : window
  ; state_manager : State_manager.t
  }

let acs_codes = get_acs_codes ()
let nice_box window = box window acs_codes.vline acs_codes.hline
let erase_and_box window = wclear window

let create_hand_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let window = newwin height width 0 width in
  nice_box window;
  wrefresh window |> prerr;
  derwin window (height - 2) (width - 2) 1 1
;;

let create_book_window parent_height parent_width =
  let width = parent_width / 2 in
  let window = newwin parent_height width 0 0 in
  nice_box window;
  wrefresh window |> prerr;
  derwin window (parent_height - 2) (width - 2) 1 1
;;

let create_make_trade_window parent_height parent_width =
  let height = parent_height / 2 in
  let width = parent_width / 2 in
  let parent_window = newwin height width height width in
  let commodity_window =
    derwin parent_window (height - 3) ((width - 2) / 2) 2 1
  in
  let quantity_window =
    derwin parent_window (height - 3) ((width - 2) / 2) 2 (width / 2)
  in
  nice_box parent_window;
  nice_box commodity_window;
  nice_box quantity_window;
  mvwaddstr parent_window 1 1 "Select the trade you want to make: " |> prerr;
  parent_window, commodity_window, quantity_window
;;

let reset_commodity_window t =
  let y, x = ref 1, 1 in
  let state_manager, commodity_window =
    t.state_manager, t.commodity_window
  in
  List.iteri state_manager.hand_selection_list ~f:(fun index commodity ->
    if State_manager.Current_state.equal
         state_manager.state
         Selecting_Commodity
       && index = state_manager.hand_cursor
    then wattron commodity_window Curses.A.standout
    else wattroff commodity_window Curses.A.standout;
    mvwaddstr commodity_window !y x (Commodity.to_string commodity) |> prerr;
    y := !y + 1);
  wrefresh commodity_window |> prerr
;;

let reset_quantity_window t =
  let y, x = ref 1, 1 in
  let state_manager, quantity_window = t.state_manager, t.quantity_window in
  List.iteri state_manager.quantity_selection_list ~f:(fun index quantity ->
    if State_manager.Current_state.equal
         state_manager.state
         Selecting_Quantity
       && index = state_manager.quantity_cursor
    then wattron quantity_window Curses.A.standout
    else wattroff quantity_window Curses.A.standout;
    mvwaddstr quantity_window !y x (Int.to_string quantity) |> prerr;
    y := !y + 1);
  wrefresh quantity_window |> prerr
;;

let reset_hand_window window =
  erase_and_box window;
  waddstr window "Player Hand: " |> prerr
;;

let reset_book_window window =
  erase_and_box window;
  waddstr window "Book Updates: " |> prerr
;;

let refresh_all_windows t =
  wrefresh t.parent_window |> prerr;
  wrefresh t.trade_parent_window |> prerr;
  wrefresh t.commodity_window |> prerr;
  wrefresh t.quantity_window |> prerr;
  wrefresh t.book_window |> prerr;
  wrefresh t.hand_window |> prerr
;;

let init () : t =
  let parent_window = initscr () in
  (* Turn off input buffering *)
  cbreak () |> prerr;
  refresh () |> prerr;
  let height, width = getmaxyx parent_window in
  let state_manager = State_manager.init () in
  let hand_window = create_hand_window height width in
  let book_window = create_book_window height width in
  let trade_parent_window, commodity_window, quantity_window =
    create_make_trade_window height width
  in
  let ui =
    { parent_window
    ; height
    ; width
    ; trade_parent_window
    ; commodity_window
    ; quantity_window
    ; hand_window
    ; book_window
    ; state_manager
    }
  in
  reset_commodity_window ui;
  reset_quantity_window ui;
  refresh_all_windows ui;
  ui
;;

let update_hand t (hand : Commodity.t list) =
  reset_hand_window t.hand_window;
  let updated_hand = Player.hand_to_string hand in
  let hand_selection_list =
    List.dedup_and_sort hand ~compare:Commodity.compare
  in
  t.state_manager.hand_selection_list <- hand_selection_list;
  State_manager.reset t.state_manager;
  mvwaddstr t.hand_window 2 1 updated_hand |> prerr;
  reset_commodity_window t;
  refresh_all_windows t
;;

let update_book t book =
  reset_book_window t.book_window;
  let updated_book = Game.book_to_string book in
  mvwaddstr t.book_window 2 1 updated_book |> prerr;
  refresh_all_windows t
;;

let update_game_over t message =
  werase t.hand_window;
  werase t.book_window;
  mvwaddstr t.hand_window 1 1 message |> prerr;
  refresh_all_windows t
;;

(* let enable_trade_input t = timeout 0; ignore (getch ()); wgetstr
   t.trade_parent_window ;; *)

let manage_user_input t =
  timeout 0;
  noecho () |> prerr;
  curs_set 0 |> prerr;
  Deferred.repeat_until_finished () (fun () ->
    (match t.state_manager.state with
     | State_manager.Current_state.Selecting_Commodity ->
       reset_commodity_window t
     | State_manager.Current_state.Selecting_Quantity ->
       reset_quantity_window t);
    refresh_all_windows t;
    let user_input = getch () in
    if not (user_input = -1) then printf "%d " user_input;
    match Input.of_int user_input with
    | Up ->
      print_endline "Up reached";
      State_manager.move_up t.state_manager;
      return (`Repeat ())
    | Down ->
      print_endline "Down reached";
      State_manager.move_down t.state_manager;
      return (`Repeat ())
    | Escape ->
      State_manager.handle_escape t.state_manager;
      return (`Repeat ())
    | Enter ->
      print_endline "Enter reached";
      State_manager.handle_enter t.state_manager;
      (match
         ( t.state_manager.curr_commodity_selected
         , t.state_manager.curr_quantity_selected )
       with
       | Some commodity, Some quantity ->
         return (`Finished (commodity, quantity))
       | _ -> return (`Repeat ()))
    | Invalid -> return (`Repeat ()))
;;

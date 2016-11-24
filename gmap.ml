open Data
open Reader

(*******************************************
 * utility methods
 *******************************************)

(* [constuct_map] creates a map data structure.
 *)
let construct_map () =
  failwith "unimplemented"

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window.
 *)
let print_map map =
  failwith "unimplemented"

(* [get_exits map] returns an association list of exit coordinates to their
 * respective buildings.
 *)
let get_exits map =
  List.fold_left (fun acc (b, el) -> 
    acc@(List.fold_left (fun acc (n, (x,y)) ->
      ((x,y), b)::acc) [] el)
    ) [] map.exits

(* [is_exit_blocked map coord] returns true if the exit at location [coord] is 
 * blocked in by another player. Does not check that [coord] is a valid exit. 
 *)
let is_exit_blocked map (r,c) =
  let res = ref true in
  let m = map.map_values in
    for cur_r = r -1 to r + 1 do
      for cur_c = c -1 to c + 1 do
        if cur_r <> r && cur_c <> c then
          if m.(cur_r).(cur_c) = Some "." then
            res := false
      done;
    done;
    !res

(* [is_building_blocked map b] returns true if all exits out of building [b] is
 * blocked. 
 * requires: [b] is a valid building.
 *)
let is_building_blocked map b =
  List.fold_left (fun acc (n, c) ->
    acc && (is_exit_blocked map c) 
    true (List.assoc b map.exits)


(*******************************************
 * Methods for player queries
 *******************************************)

(* [is_in_building map p] checks if player [p] is currently in a building on 
 * the [map].
 * Returns: [true] iff player [p] is in a building.
 *)
let is_in_building map p =
  List.mem_assoc p map.in_building

(* [get_current_building map p]
 * Returns: [some r] if player [p] is in building [b] or [None] if player [p]
 * is currently not in a building.
 *)
let get_current_building map p =
  try
    let b = List.assoc p map.in_building in
    Some b
  with
  | _ -> None

(* [get_current_location map p]
 * Returns: the coordinate where player [p] is on the [map].
 *)
let get_current_location map p =
  List.assoc p map.location


(* [closest_buildings map p]
 * Returns: a list of buildings n order of closeness to player [p]
 * If the player is inside a building, the player's location is the
 * average of the coordinates of the building's exits
 *)
let closest_buildings map p =
  let (my_r, my_c) = 
    if is_in_building map p then
      let cur_building = get_current_building map p in
      let building_exits = List.assoc cur_building map.exits in
      let len = List.length building_exits in
      let (sumr, sumc) = List.fold_left 
        (fun (accr,accc) (n, (r,c)) -> (accr + r, accc+c)) 
        (0,0) building_exits in 
        (sumr/len, sumc/len)
    else get_current_location map p in
      (* find closest exit for each building *)
      let exits = List.map (fun (b, el) -> 
        (get_closest_exit el (my_r,my_c), b)) map.exits in
        (* list of (distance to exit, building name) *)
      let dists = List.map( fun ((r,c), b) ->
        (abs (r - my_r) + abs (c - my_c), b)) exits in
        List.sort_uniq (Pervasives.compare) dists
and get_closest_exit lst (r,c) =
  let dists = List.map (fun (_, (r',c')) ->
    (abs (r - r') + abs (c - c'), (r',c')) ) lst in
  let sorted = List.sort (Pervasives.compare) dists in
  let (d, p) = List.hd sorted in p


(*
  let exits = get_exits map in
    if is_in_building map p then
      let dists = List.map (fun ((x,y),b) -> 
          () ) exits in
          failwith "unimplemented"
    else  (* out in the hallway *)
      let (my_x, my_y) = get_current_location map p in
      (* list of (distance to coord, building name) *)
      let dists = List.map( fun ((x,y), b) ->
        (abs (x - my_x) + abs (y - my_y), b), exits in
      (* sort list so that it is in order of closest distance *)
      let sorted = List.sort_uniq (Pervasives.compare) dists in
        List.fold_left (fun acc (d,b) -> acc@[b]) [] sorted
and get_closest_exit_coords exits (x,y) =
  List.fold_left (fun (best_d, (best_p, best_b)) ((x1,y1), b) ->
    let dist = abs(x1-x) + abs(y1-y) in
    if dist < best_d then
      (dist, ((x1,y1), b))
    else
      (best_d, (best_p, best_b))) (1000, List.hd exits) exits
*)

(*******************************************
 * methods for moving around on the map
 *******************************************)


(* [leave_building map p n] moves player [p] to exit [n] of the current
 * building [p] is in, and performs all changes necessary to update the [map].
 * Raises: InvalidOperation if the player p is not in a room.
 *)
let leave_building map p n =
  match get_current_building map p with
  | None   -> raise InvalidOperation
  | Some b -> failwith "unimplemented" 


(* [move map p dir n] tries to move player [p] on the [map] [n] steps in [dir]
 * direction.
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 *)
let move map p dir n =
  failwith "unimplemented"

(* [move_towards_coord map p c n] tries to move player [p] on the [map]
 * [n] steps towards the coordinate [c].
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to [c]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [c] is off the map or inside a building.
 *)
 let move_towards_coord map p c n =
  let (r, c) = c in
    if r < 0 || c < 0 || r >= map.num_rows || c >- map.num_cols
    then raise InvalidLocation
    else failwith "unimplemented"

(* [move_towards_building map p b n] tries to move player [p] on the [map] [n]
 * steps towards the building [b].
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to building [b]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
 let move_towards_building map p b n =
  failwith "unimplemented"

(* [teleport_player map p b] moves a person [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
 let teleport_player map p b =
  if is_in_building map p then
  (*
    - check if it is the same room -> do nothing
    - remove entry from in_building
    - set map location to None
    - add new entry for in_building 
    - *)
    failwith "unimplemented"
  else 
    (* 
    - set current location to Some "."
    - add name to in_building list of map 
    - find spot in room to update
    *)
    failwith "unimplemented"


open Data
open Reader

(*******************************************
 * utility methods
 *******************************************)

(* [constuct_map] creates a map data structure.
 *)
let construct_map () = make_map()

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window.
 * 
 * Code was taken from 2016F Prelim2-Part2 and modified to fit our needs.
 *)
let rec print_map map =
  map.map_values |> string map.num_rows map.num_cols |> print_string
and string nr nc w =
  let buf = Buffer.create ((nr+1)*(nc+1)) in
  for i = 0 to nr-1 do
    for j = 0 to nc-1 do
      Buffer.add_string buf (display w.(i).(j))
    done;
    Buffer.add_string buf "\n"
  done;
  Buffer.contents buf
and display n =
  match n with
    | None   -> ANSITerminal.sprintf [ANSITerminal.on_black] "%s" " "
    | Some i -> ANSITerminal.sprintf 
                [ANSITerminal.on_black; style_of_str i] "%s" (String.make 1 i.[0])
and style_of_str i =
  let open ANSITerminal in
    match i with
      | "."    -> white (* ground *)
      | "*"    -> yellow (* wall   *)
      | "DOOR" -> green (* exit   *)
      | "s"    -> green (* secret passage *)
      | c ->
          let len = String.length c in
            if      len = 1 then yellow  (* part of clue header *)
            else if len = 2 then cyan    (* part of building name *)
            else red                     (* professor name *)


(* [get_exits map] returns an association list of exit coordinates to their
 * respective buildings.
 *)
let get_exits map =
  List.fold_left (fun acc (b, el) -> 
    acc@(List.fold_left (fun acc (n, (x,y)) ->
      ((x,y), b)::acc) [] el)
    ) [] map.exits

(* [is_exit_blocked map coord] returns true if the exit at location [coord] is 
 * blocked in by another professor. Does not check that [coord] is a valid exit. 
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
 * Methods for professor queries
 *******************************************)

(* [is_in_building map p] checks if professor [p] is currently in a building on 
 * the [map].
 * Returns: [true] iff professor [p] is in a building.
 *)
let is_in_building map p =
  List.mem_assoc p map.in_building

(* [get_current_building map p]
 * Returns: [some r] if professor [p] is in building [b] or [None] if professor [p]
 * is currently not in a building.
 *)
let get_current_building map p =
  try
    Some (List.assoc p map.in_building)
  with
  | _ -> None

(*[has_secret_passage map b] returns [true] iff there is a secret passage in
 * building [b].
 *)
let has_secret_passage map b =
  List.mem_assoc b map.secrets

(*[get_secret_passage map b] returns a building option for which building [b] 
 * has a secret passage to. If [b] does not have a secret passage, [None] is
 * returned.
 *)
let get_secret_passage map b =
  try
    Some (List.assoc b map.secrets)
  with
  | _ -> None

(* [get_current_location map p]
 * Returns: the coordinate where professor [p] is on the [map].
 *)
let get_current_location map p =
  List.assoc p map.location


(* [closest_buildings map p]
 * Returns: a list of buildings n order of closeness to professor [p]
 * If the professor is inside a building, the professor's waiting location is
 * used.
 *)
let closest_buildings map p =
  let (my_r, my_c) = get_current_location map p in
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

(*******************************************
 * methods for moving around on the map
 *******************************************)


(* [leave_building map p n] moves professor [p] to exit number [n] of the current
 * building [p] is in, and performs all changes necessary to update the [map].
 * Raises: InvalidOperation if the professor p is not in a room.
 *)
let leave_building map p n =
  match get_current_building map p with
  | None   -> raise InvalidOperation
  | Some b -> 
    try
      let exitL = List.assoc b map.exits in
      let (er,ec) = List.assoc n exitL in
      let (r,c) = get_current_location map p in
      let m = map.map_values in
        m.(r).(c)   <- None;
        m.(er).(ec) <- Some (p^"DOOR");
        let locL  = List.remove_assoc p map.location in
        let nloc  = (p,(er,ec))::locL in
        {map with location = nloc }
    with
    | Not_found -> raise InvalidOperation



(* [move map p dir n] tries to move professor [p] on the [map] [n] steps in 
 * [dir] direction.
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 *)
let move map p dir n =
  failwith "unimplemented"

(* [move_towards_coord map p c n] tries to move professor [p] on the [map]
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

(* [move_towards_building map p b n] tries to move professor [p] on the [map] [n]
 * steps towards the building [b].
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to building [b]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
 let move_towards_building map p b n =
  failwith "unimplemented"

(* [teleport_professor map p b] moves a professor [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
 let teleport_professor map p b =
  let umap = 
    match get_current_building map p with
    | Some curB ->
        if curB <> b 
        then leave_building map b 1
        else map
    | None -> map in
    let (curr, curc) = get_current_location umap p in
      match umap.map_values.(curr).(curc) with
      | None     -> failwith "Unexpected None value in teleport_professor"
      | Some str ->
        if (String.length str) <> (String.length p) then
          umap.map_values.(curr).(curc) <- Some "DOOR"
        else
          umap.map_values.(curr).(curc) <- Some ".";
      (* 
      - add name to in_building list of map 
      - find spot in room to update
      - update was_moved
      *)
      failwith "unimplemented"


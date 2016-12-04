open Data
open Reader

(*******************************************
 * utility methods
 *******************************************)

(* [constuct_map] creates a map data structure. *)
let construct_map () = make_map ()

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window.
 * 
 * Code was taken from 2016F Prelim2-Part2 and modified to fit our needs.
 *)
let rec print_map map =
  map.map_values |> to_string map.num_rows map.num_cols |> print_string
and to_string nr nc w =
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
  | None   -> ANSITerminal.sprintf [ANSITerminal.on_black] "%s" "- "
  | Some i -> ANSITerminal.sprintf 
              [ANSITerminal.on_black; style_of_str i] "%s " 
              (String.make 1 i.[0])
and style_of_str i =
  let open ANSITerminal in
  match i with
  | "."    -> white (* ground *)
  | "*"    -> yellow (* wall   *)
  | "DOOR" -> green (* exit   *)
  | "s"    -> green (* secret passage *)
  | c      ->
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
  let m = map.map_values in
    (m.(r-1).(c) <> Some ".") && (m.(r).(c-1) <> Some ".") &&
    (m.(r+1).(c) <> Some ".") && (m.(r).(c+1) <> Some ".")

(* [is_building_blocked map b] returns true if all exits out of building [b] is
 * blocked. 
 * requires: [b] is a valid building.
 *)
let is_building_blocked map b =
  List.fold_left (fun acc (n, c) ->
    acc && (is_exit_blocked map c)) 
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

(* [get_closest_exit map lst (r,c)] returns the closest exit coordinate in [lst] to 
 * the location[(r,c)] based on manhattan distance (not actual steps it takes to
 * reach).
 *)
let get_closest_exit map lst (r,c) =
  let dists = List.map (fun (_, (r',c')) ->
    if is_exit_blocked map (r',c')
    then 
      (map.num_rows*map.num_cols, (r',c')) (* effectively infinity for map *)
    else
      (abs (r - r') + abs (c - c'), (r',c')) )
    lst in
  let sorted = List.sort (Pervasives.compare) dists in
  let (d, p) = List.hd sorted in p

(* [closest_buildings map p]
 * Returns: a list of buildings in order of closeness to professor [p]
 * If the professor is inside a building, the professor's waiting location is
 * used.
 *)
let closest_buildings map p =
  let (my_r, my_c) = get_current_location map p in
  (* find closest exit for each building *)
  let exits = List.map (fun (b, el) -> 
    (get_closest_exit map el (my_r,my_c), b)) map.exits in
  (* list of (distance to exit, building name) *)
  let dists = List.map( fun ((r,c), b) ->
    if is_building_blocked map b 
    then
      (map.num_rows*map.num_cols, b)
    else
      (abs (r - my_r) + abs (c - my_c), b)) exits in
    List.sort_uniq (Pervasives.compare) dists


(*******************************************
 * methods for moving around on the map
 *******************************************)

(*[replace_tile m p r c] resets the value of the tile professor [p] is or was 
 * standing on in [m]. 
 * requires: [p] is a valid professor name, [r] and [c] are in bounds on [m].
 *)
let replace_tile m p r c =
  match m.(r).(c) with
  | None        -> failwith "Unexpected None value"
  | Some "."    -> m.(r).(c) <- Some "."
  | Some "DOOR" -> m.(r).(c) <- Some "DOOR"
  | Some str    ->
    Printf.printf "replace_tile %s at (%d, %d)\n" str r c; (*TODO debug why not working?*)
    if (String.length str) <> (String.length p) then
      m.(r).(c) <- Some "DOOR"
    else
      m.(r).(c) <- Some "."

(* [update_location locs p c] returns the list of locations [loc] with [p]'s
 * location updated to [c]. 
 * requires: [p] is a valid professor name.
 *)
let update_location locs p c =
  let locL = List.remove_assoc p locs in
  (p, c)::locL


let leave_building_helper map p n =
  match get_current_building map p with
  | None   -> raise InvalidOperation
  | Some b -> 
    try
      let exitL   = List.assoc b map.exits in
      let (er,ec) = List.assoc n exitL in
      let (r,c)   = get_current_location map p in
      let buildL  = List.remove_assoc p map.in_building in
      let m = map.map_values in
        m.(r).(c)   <- None;
        m.(er).(ec) <- Some (p^"DOOR");
        let nloc = update_location map.location p (er,ec) in
        {map with in_building = buildL; location = nloc}
    with
    | Not_found -> raise InvalidOperation


(* [leave_building map p n] moves professor [p] to exit number [n] of the current
 * building [p] is in, and performs all changes necessary to update the [map].
 * Raises: InvalidOperation if the professor p is not in a room.
 *)
let leave_building map p n =
  let umap = leave_building_helper map p n in
  print_map umap;
  umap

(*[enter_building map p b] handles professor [p] entering into building [b] by
 * updating the necessary values in [map] and returning the newly updated map
 * Requires: [p] is a valid professor name, [b] is a valid building name.
 *)
let rec enter_building map p b =
  ANSITerminal.(print_string [yellow] ("Prof. "^p^" enters "^b^" Hall.\n"));
  let m     = map.map_values in
  let wl    = List.assoc b map.waiting_spots in
  let (r,c) = get_open_spot m wl in
  m.(r).(c) <- Some p; 
  let newL = (p, b)::map.in_building in
  let nloc = update_location map.location p (r,c) in
  let umap = {map with location = nloc; in_building = newL } in
  umap
and get_open_spot m lst = match lst with
  | (r,c)::t -> if m.(r).(c) = None then (r,c) 
                else get_open_spot m t
  | [] -> failwith "No open waiting spots! which should not happen."

let move_helper map p bop dir n =
  let (rd, cd) = match dir with
  | "up"    -> (-1, 0)
  | "down"  -> ( 1, 0)
  | "left"  -> ( 0,-1)
  | "right" -> ( 0, 1)
  | _ -> failwith ("Invalid direction to move: "^dir) in
  let m = map.map_values in
  let (sr,sc) = get_current_location map p in
    replace_tile m p sr sc;  (* clear out starting location *)
    let cr = ref sr in
    let cc = ref sc in
    let i  = ref 0  in
    (* move in [dir] direction while you are able to*)
    while (!i < n) && (m.(!cr + rd).(!cc + cd) = Some ".") do
      incr i;
      cr := !cr + rd;
      cc := !cc + cd;
    done;
    (* if you end up in front of a door with steps left, enter the building if permitted *) 
    if (!i < n) && (m.(!cr + rd).(!cc + cd) = Some "DOOR") 
    then
      let b = List.assoc (!cr + rd,!cc + cd) (get_exits map) in
        begin
          match bop with 
          | Some b' when b' = b -> (*TODO debug*)
              begin
                print_endline "Cannot re-enter the same building in the same turn!";
                let nloc = update_location map.location p (!cr,!cc) in
                m.(!cr).(!cc) <- Some p; (* mark down at new location   *)
                (n-(!i), {map with location = nloc; map_values = m})
              end
          | Some _ | None -> (*TODO debug*)
              begin
                i := n; (* no steps left after entering building *)
                Printf.printf "Entering %s Hall debug\n" b; (*TODO*)
                (0, enter_building map p b)
              end
        end
    else
      let nloc = update_location map.location p (!cr,!cc) in
        (* mark down at new location *)
        if m.(!cr).(!cc) = Some "DOOR" 
        then m.(!cr).(!cc) <- Some (p^"DOOR")
        else m.(!cr).(!cc) <- Some p; 
        (n-(!i), {map with location = nloc; map_values = m})

(* [move map p bop dir n] tries to move professor [p] on the [map] [n] steps
 * in [dir] direction. If [bop] is [Some b'] then [p] cannot move into 
 * [b'] since [p] just left that building in the same turn.
 * requires: [n >= 0], [p] is a valid professor name, [dir] = "up", "down",
 *           "left", or "right".
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 *)
let move map p bop dir n = 
  let (steps_left, nmap) = move_helper map p bop dir n in
    print_map nmap;
    (steps_left, nmap)

(* [move_towards_coord map p coord n] tries to move professor [p] on the [map]
 * [n] steps towards the coordinate [coord].
 * Requires: [n >= 0], [p] is not in a building
 *       TODO: [coord] is [Some .] on map ???
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to [coord]
 *   [map2] is the updated map.
 *)
let rec move_towards_coord map p coord n =
  let (destr, destc) = coord in
    if destr < 0 || destc < 0 || destr >= map.num_rows || destc >= map.num_cols
    then raise (InvalidLocation "out of bounds")
    else
      let (sr, sc) = get_current_location map p in
      replace_tile map.map_values p sr sc;
      let path = calc_path map p destr destc in
      let dirs = simplify_path path in
      let steps_left = ref n in
      let map2 =
        List.fold_left (fun accmap (dir,steps) -> 
          if (!steps_left > 0) then
          (* has steps left to take, and isn't in a building *)
            let (_ , accmap) = 
              if !steps_left < steps 
              then
                begin
                  let (s, m) = move_helper accmap p None dir !steps_left in
                    steps_left := s; 
                    (s, m)
                end
              else
                begin
                  let (s, m) = move_helper accmap p None dir steps in
                    steps_left := !steps_left - steps + s;
                    (s, m)
                end
            in accmap
          else accmap) map dirs in
      let arrived = (is_in_building map2 p) ||
                    ((get_current_location map2 p) = (destr,destc)) in
      print_map map2;
      (arrived, map2)
(* calcuates which steps needed be to taken to get to a certain destination and
 * avoiding all obstacles (so collision doesn't happen when moving) *)
and calc_path map p destr destc =
  let m = map.map_values in
    if m.(destr).(destc) = Some "." || m.(destr).(destc) = Some "DOOR" then 
    (* valid destination *)
      let (sr,sc)  = get_current_location map p in 
      let beenHere = Array.make_matrix map.num_rows map.num_cols false in
      let foundEnd = ref false in
      let path     = ref [] in
      let queue    = ref [(sr,sc,[])] in
        (* We use flood fill to find the destination.
         * path = our travel history to a specific location.
         * queue = our queue of possibly unvisited locations, ordered by 
         *         increasing distance
         *)
        while not !foundEnd do
          match !queue with
          | [] -> failwith ("moving to blocked building");
          | (r,c,hist)::t -> 
            if beenHere.(r).(c) then
              queue := t
            else (* visiting a new spot *)
              begin
                (* mark spot as traveled *)
                beenHere.(r).(c) <- true;
                if r = destr && c = destc 
                then (* we've arrived *)
                  begin
                    foundEnd := true;
                    path     := (r,c)::hist
                  end
                else if m.(r).(c) = Some "." || m.(r).(c) = Some "DOOR" then
                  (* valid position, add possible spots to walk into from here *)
                  let nhist = (r,c)::hist in 
                    queue := t@[(r+1,c,nhist); (r,c+1,nhist);
                                (r-1,c,nhist); (r,c-1,nhist)];
                else (* can't travel into this space, try other options *)
                  queue := t                
              end
        done;
        !path
    else (* destination inside a building or on a professor *)
      raise (InvalidLocation "destination not a valid ending spot")
(* simplifies the calculated path so that it is reduced to a sequence of [move] 
 * commands where each subsequent command is in a different direction.*)
and simplify_path lst =
  if List.length lst <= 1 
  then 
    begin
      print_endline "somehow got <= 1 size of path";
      []
    end
  else
    let rec simplify acc dlst =
      (* acc  = accumulated list of simplified directions 
       * dlst = remaining list of coordinates to go through *)
      match acc with
      | []       -> 
        begin 
          (* start of simplify: add the first direction to travel *)
          match dlst with
          | (r1,c1)::(r2,c2)::t -> 
            let dir = match (r1-r2, c1-c2) with
             | ( 1, 0) -> "down"
             | ( 0, 1) -> "right"
             | (-1, 0) -> "up"
             | ( 0,-1) -> "left"
             | _ -> "unexpected delta between steps" in
              simplify [(dir,1)] ((r2,c2)::t)
          | _       -> acc
        end
      | (dir, n)::dt -> 
        begin
          (* decide to add on to current direction, or go in a different one *)
          match dlst with
          | (r1,c1)::(r2,c2)::t -> 
            let ndir = match (r1-r2, c1-c2) with
              | ( 1, 0) -> "down"
              | ( 0, 1) -> "right"
              | (-1, 0) -> "up"
              | ( 0,-1) -> "left"
              | _ -> "unexpected delta between steps" in
              if dir = ndir then
                simplify ((dir, n+1)::dt) ((r2,c2)::t)
              else
                simplify ((ndir,1)::(dir,n)::dt) ((r2,c2)::t)
          | _ -> acc
        end
    in 
      simplify [] lst

(* [move_towards_building map p b n] tries to move professor [p] on the [map] 
 * [n] steps towards the building [b].
 * Requires: [n >= 0], [p] is not in a building already.
 * TODO: called only when [p] can enter [b], i.e., [p] did not just leave [b].
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to [coord]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
let move_towards_building map p b n =
  try
    let loc  = get_current_location map p in
    let el   = List.assoc b map.exits in
    try
      let eloc = get_closest_exit map el loc in
        move_towards_coord map p eloc n
      with
      | InvalidLocation s -> failwith ("[move_towards_building]: "^s)
  with
    | Not_found -> 
      failwith ("[move_towards_building]: could not find professor or building")
      
(* [teleport_professor map p b] moves a professor [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
let teleport_professor map p b =
  let umap = 
    match get_current_building map p with
    (* inside a building, [p] needs to leave first if not in building [b]  *)
    | Some curB ->
      leave_building_helper map p 1
    (* not in a building: *)
    | None -> map in
    let (curr, curc) = get_current_location umap p in
      (* replace current spot w/ the original terrain: *)
      replace_tile umap.map_values p curr curc;
      let umap = enter_building umap p b in
        print_map umap;
        umap

(* [use_secret_passage map p] returns the updated map when [p] takes the secret
 * passage inside a room.
 * requires: [p] is a valid professor name
 * raises: InvalidOperation when [p] is not in a building. Also fails when the 
 *         room has no secret passage.
 *)
let use_secret_passage map p =
  match get_current_building map p with
  | None   -> raise InvalidOperation
  | Some b -> 
    try
      let toB  = List.assoc b map.secrets in
        teleport_professor map p toB
    with
    | _ -> failwith ("No secret passage to take in "^b)


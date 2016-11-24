open Yojson.Basic
open Yojson.Basic.Util
open Data

(* [extract_info field f] is a function applied onto json objects to get the 
 * members of a specific [field] and then performs [f] on them.
 *)
let extract_info field f =
  fun x -> member field x |> f

(* [extract_map json nr nc] extracts the ascii map from [json] and puts it into 
 * a matrix. 
 * requires: [nr] is the number of rows in the map,
 *           [nc] is the number of cols in the map,
 *           the ascii diagram is broken up across multiple lines.
 *)
let extract_map json nr nc =
  let strmap = (extract_info "map_diag" to_string) json in
  let m = Array.make_matrix nr nc None in
    for r = 0 to nr - 1 do 
      for c = 0 to nc - 1 do
        let ch = strmap.[r*(nc+1) + c] in
          if ch <>'?' then
            m.(r).(c) <- Some (String.make 1 ch)
      done;
    done; 
    m

(* [extract_building_info json m] parses [json] for information regarding:
 *   - building name on map [m], and updates [m] accordingly
 *   - exits,
 *   - waiting areas,
 *   - secret passageways.
 *)
let rec extract_building_info json m =
  let buildings  = extract_info "buildings" to_list json in
    let quadList = List.map (fun j ->
    let b        = extract_info "name"     to_string j in
    let n        = extract_info "nickname" to_string j in
    let exitList = extract_exits         (extract_info "exits" to_list j) in
    let waitList = extract_waiting_spots (extract_info "spots" to_list j) in
    let secretP  = extract_secret_passage b j in
      write_name_to_map m j n;
      (b, exitList, waitList, secretP)) 
      buildings in
      List.fold_left (fun (bs, es, ws, ss) (b, e, w, s) ->
        (b::bs, (b,e)::es, (b,w)::ws, ss@s)) 
        ([],[],[],[]) quadList
(* [extract_coord j] parses a pair (r,c) value out of [j] *)
and extract_coord j =
  (extract_info "r" to_int j, extract_info "c" to_int j)
(* [write_name_to_map m j nickname] writes the name [nickname] onto the map [m].
 * requires: [String.length nickname = 4]
 *)
and write_name_to_map m j nickname =
  let loc = member "name_location" j in
  let (r,c) = extract_coord loc in
    for i = 0 to 3 do
      m.(r).(c+i) <- Some ((String.make 1 nickname.[i])^"-")
    done
(* [extract_exits jsonlst] parses [jsonlist] for all exits and returns them
 * as a (door id, coord) list. *)
and extract_exits jsonlst =
  List.fold_left (fun acc j ->
    let id = extract_info "id" to_int j in
    let c  = extract_coord j in
      acc@[(id, c)]
  ) [] jsonlst
(* [extract_secret_passage b json] parses [json] to see if building [b] has a
 * secret passageway to some other building. 
 *)
and extract_secret_passage b json =
  if (extract_info "passage_exits" to_bool json) then
    [(b, extract_info "passage_to" to_string json)] 
  else []
(* [extract_waiting_spots jsonlist] parses [jsonlist] to create a list of
 * coordinates where people inside a specific building should wait. 
 *)
and extract_waiting_spots jsonlst = 
  List.fold_left (fun acc j ->
    (extract_coord j)::acc
  ) [] jsonlst

(* [extract_starting_locations json] parses [json] to returns a list of pairs 
 * where the first element is the player, and the second element is their 
 * starting coordinate on the map. 
 *)
let extract_starting_locations m json=
  let sl = extract_info "starting_locations" to_list json in
    List.fold_left (fun acc j ->
      let p = extract_info "player" to_string j in
      let r = extract_info "r" to_int j in
      let c = extract_info "c" to_int j in
        m.(r).(c) <- Some p;
        (p, (r,c))::acc ) [] sl

(* [make_map] parses the json file storing information about the map and
 * converts it into a [map].
 * Returns: a [map] with only information about the map (no player info) *)
let make_map () = 
  let json = from_file "map.json" in
  let n_r = (extract_info "num_rows" to_int) json in
  let n_c = (extract_info "num_cols" to_int) json in
  let m = extract_map json n_r n_c in
  let (blist, elist, wslist, slist) = extract_building_info json m in
  {
    num_rows      = n_r; 
    num_cols      = n_c;
    map_values    = m;
    exits         = elist; 
    buildings     = blist;
    in_building   = [];    (* no one starts in a building *)
    location      = extract_starting_locations m json; 
    waiting_spots = wslist;
    secrets       = slist;
  }

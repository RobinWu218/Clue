open Data
open Reader

(* [map] is the data structure that will store the state of the map *)
type map

(*******************************************
 * utility methods 
 *******************************************)

(* [constuct_map professors] creates a map data structure with the character names
 * listed in [professors]. The first name in [professors] is taken as the character
 * for the user. The rest are AI. 
 * Returns: the starting map
 *)
val construct_map: unit -> map

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window. 
 *)
val print_map: map -> unit

(* [get_exits map] 
 * Returns: a list of building exits on the map. 
 *)
val get_exits: map ->  coord * building list

(* [is_exit_blocked map coord] returns true if the exit at location [coord] is 
 * blocked in by another professor. Does not check that [coord] is a valid exit. 
 *)
val is_exit_blocked: map -> coord -> bool

(* [is_building_blocked map b] returns true if all exits out of building [b] is
 * blocked. 
 * requires: [b] is a valid building.
 *)
val is_building_blocked: map -> building -> bool


(*******************************************
 * Methods for professor queries 
 *******************************************)

(* [is_in_building map p] checks if professor [p] is currently in a building on 
 * the [map].
 * Returns: [true] iff professor [p] is in a building.
 *)
val is_in_building: map -> prof -> bool

(* [get_current_building map p]
 * Returns: [some r] if professor [p] is in building [b] or [None] if professor [p]
 * is currently not in a building. 
 *)
val get_current_building: map -> prof -> building option

(*[has_secret_passage map b] returns [true] iff there is a secret passage in
 * building [b].
 *)
val has_secret_passage: map -> building -> bool

(*[get_secret_passage map b] returns a building option for which building [b] 
 * has a secret passage to. If [b] does not have a secret passage, [None] is
 * returned.
 *)
val get_secret_passage: map -> building -> building option

(* [get_current_location map p]
 * Returns: the coordinate where professor [p] is on the [map]. 
 * Raises: InvalidLocation exception if the professor is in a building 
 *)
val get_current_location: map -> prof -> coord

(* [closest_buildings map p] 
 * Returns: a list of buildings n order of closeness to professor [p]
 *)
val closest_buildings: map -> prof -> building list


(*******************************************
 * methods for moving around on the map 
 *******************************************)

(* [leave_building map p n] moves professor [p] to exit [n] of the current
 * building [p] is in, and performs all changes necessary to update the [map].
 * Raises: InvalidOperation if the professor p is not in a room.
 *)
val leave_building: map -> prof -> int -> map

(*[enter_building map p b] handles professor [p] entering into building [b] by
 * updating the necessary values in [map] and returning the newly updated map
 * requires: [p] is a valid professor name, [b] is a valid building name.
 *)
val enter_building: map -> prof -> building -> map

(* [move map p dir n] tries to move professor [p] on the [map] [n] steps in [dir]
 * direction. 
 * Returns: the pair [(i, map2)], where 
 *   [i]    is the steps remaining after going in [dir] direction, and 
 *   [map2] is the updated map. 
 *)
val move: map -> prof -> string -> int -> int * map

(* [move_towards_coord map p c n] tries to move professor [p] on the [map] 
 * [n] steps towards the coordinate [c]. 
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [c] is off the map or inside a building. 
 *)
val move_toward_coord: map -> prof -> coord -> int -> bool * map

(* [move_towards_building map p b n] tries to move professor [p] on the [map] 
 * [n] steps towards the building [b].
 * Requires: [n >= 0], [p] is not in a building already.
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
val move_towards_building: map -> prof -> building -> int -> bool * map

(* [teleport_professor map p b] moves a professor [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
val teleport_professor: map -> prof -> building -> map

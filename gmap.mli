open Data
open Reader

(*******************************************
 * utility methods 
 *******************************************)

(* [constuct_map] creates a map data structure. *)
val construct_map: unit -> map

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window.
 * 
 * Code was taken from 2016F Prelim2-Part2 and modified to fit our needs.
 *)
val print_map: map -> unit

(* [get_exits map] returns an association list of exit coordinates to their
 * respective buildings.
 *)
val get_exits: map -> (coord * building) list

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

(* [get_closest_exit lst (r,c)] returns the closest exit coordinate in [lst] to 
 * the location[(r,c)] based on manhattan distance (not actual steps it takes to
 * reach).
 *)
val get_closest_exit : (int * coord) list -> coord -> coord

(* [closest_buildings map p] 
 * Returns: a list of buildings in order of closeness to professor [p]
 *)
val closest_buildings: map -> prof -> (int * building) list


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

(* [move map p bop dir n] tries to move professor [p] on the [map] [n] steps
 * in [dir] direction. If [bop] is [Some b'] then [p] cannot move into 
 * [b'] since [p] just left that building in the same turn.
 * Returns: the pair [(i, map2)], where 
 *   [i]    is the steps remaining after going in [dir] direction, and 
 *   [map2] is the updated map. 
 *)
val move: map -> prof -> building option -> string -> int -> int * map

(* [move_towards_coord map p c n] tries to move professor [p] on the [map] 
 * [n] steps towards the coordinate [c]. 
 * Returns: the pair [(i, map2)], where
 *   [i]    is the steps remaining after going in [dir] direction, and
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [c] is off the map or inside a building. 
 *)
val move_towards_coord: map -> prof -> coord -> int -> bool * map

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

(* [use_secret_passage map p] returns the updated map when [p] takes the secret
 * passage inside a room.
 * requires: [p] is a valid professor name
 * raises: InvalidOperation when [p] is not in a building. Also fails when the 
 *         room has no secret passage.
 *)
val use_secret_passage: map -> prof -> map


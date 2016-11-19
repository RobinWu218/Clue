open Data
open Reader

(* [t] is the data structure that will store the state of the map *)
type t = unit (*temp*)

(*******************************************
 * utility methods
 *******************************************)

(* [constuct_map players] creates a map data structure with the character names
 * listed in [players]. The first name in [players] is taken as the character
 * for the user. The rest are AI.
 * Returns: the starting map
 *)
let construct_map players =
  failwith "unimplemented"

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window.
 *)
let print_map map =
  failwith "unimplemented"

(* [get_exits map]
 * Returns: a list of building exits on the map.
 *)
let get_exits map =
  failwith "unimplemented"

(* [get_players map]
 * Returns: a list of characters in current game.
 *)
let get_players map =
  failwith "unimplemented"

(*******************************************
 * methods for moving around on the map
 *******************************************)

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
  failwith "unimplemented"

(* [move_towards_building map p b n] tries to move player [p] on the [map] [n]
 * steps towards the building [b].
 * Returns: the pair [(tf, map2)], where
 *   [tf]   is [true] iff [p] succesfully made it to building [b]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
 let move_towards_building map p b n =
  failwith "unimplemented"

(* [teleport_player map p r] moves a person [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
 let teleport_player map p r =
  failwith "unimplemented"


(*******************************************
 * Methods for player queries
 *******************************************)

(* [is_in_building map p] checks if player [p] is currently in a building on the [map]
 * Returns: [true] iff player [p] is in a building.
 *)
 let is_in_building map p =
  failwith "unimplemented"

(* [get_current_building map p]
 * Returns: [some r] if player [p] is in building [b] or [None] if player [p]
 * is currently not in a building.
 *)
 let get_current_building map p =
  failwith "unimplemented"

(* [get_current_location map p]
 * Returns: the coordinate where player [p] is on the [map].
 * Raises: InvalidLocation exception if the player is in a building
 *)
 let get_current_location map p =
  failwith "unimplemented"

(* [closest_buildings map p]
 * Returns: a list of buildings n order of closeness to player [p]
 *)
 let closest_buildings map p =
  failwith "unimplemented"
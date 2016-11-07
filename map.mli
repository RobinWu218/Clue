open Data
open Reader

(* [t] is the data structure that will store the state of the map *)
type t

(*******************************************
 * utility methods 
 *******************************************)

(* [constuct_map players] creates a map data structure with the character names
 * listed in [players]. The first name in [players] is taken as the character
 * for the user. The rest are AI. 
 * Returns: the starting map
 *)
val construct_map: prof list -> t

(* [print_map map] prints out an ascii representation of the map and where all
 * characters are on it to the console window. 
 *)
val print_map: t -> unit

(* [get_exits map] 
 * Returns: a list of building exits on the map. 
 *)
val get_exits: t -> building * coord list

(* [get_players map]
 * Returns: a list of characters in current game. 
 *)
val get_players: t -> prof list


(*******************************************
 * methods for moving around on the map 
 *******************************************)

(* [move map p dir n] tries to move player [p] on the [map] [n] steps in [dir]
 * direction. 
 * Returns: the pair [(i, map2)], where 
 *   [i]    is the steps remaining after going in [dir] direction, and 
 *   [map2] is the updated map. 
 *)
val move: t -> prof -> string -> int -> int * t

(* [move_towards_coord map p c n] tries to move player [p] on the [map] 
 * [n] steps towards the coordinate [c]. 
 * Returns: the pair [(tf, map2)], where 
 *   [tf]   is [true] iff [p] succesfully made it to [c]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [c] is off the map or inside a building. 
 *)
val move_toward_coord: t -> prof -> coord -> int -> bool * t

(* [move_towards_building map p b n] tries to move player [p] on the [map] [n] 
 * steps towards the building [b].
 * Returns: the pair [(tf, map2)], where 
 *   [tf]   is [true] iff [p] succesfully made it to building [b]
 *   [map2] is the updated map.
 * Raises: InvalidLocation if [b] is not a valid building id.
 *)
val move_towards_building: t -> prof -> string -> int -> bool * t

(* [teleport_player map p r] moves a person [p] on the [map] to building [b]
 * This event occurs whenever a suggestion or accusation is made; the
 * suspect is moved to the "scene of the crime."
 * Returns: the updated map.
 *)
val teleport_player: t -> prof -> string -> t


(*******************************************
 * Methods for player queries 
 *******************************************)

(* [is_in_building map p] checks if player [p] is currently in a building on the [map]
 * Returns: [true] iff player [p] is in a building.
 *)
val is_in_building: t -> prof -> bool

(* [get_current_building map p]
 * Returns: [some r] if player [p] is in building [b] or [None] if player [p]
 * is currently not in a building. 
 *)
val get_current_building: t -> prof -> string option

(* [get_current_location map p]
 * Returns: the coordinate where player [p] is on the [map]. 
 * Raises: InvalidLocation exception if the player is in a building 
 *)
val get_current_location: t -> prof -> coord

(* [closest_buildings map p] 
 * Returns: a list of buildings n order of closeness to player [p]
 *)
val closest_buildings: t -> prof -> building list


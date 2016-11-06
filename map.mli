open Data
open Reader

(* [Map] is the module to handle interaction with the map object in the game. *)
module type Map = sig

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
  val construct_map: string list -> t

  (* [print_map map] prints out an ascii representation of the map and where all
   * characters are on it to the console window. 
   *)
  val print_map: t -> unit

  (* [get_exits map] 
   * Returns: a list of room exits on the map. 
   *)
  val get_exits: t -> string * (int * int) list

  (* [get_players map]
   * Returns: a list of characters in current game. 
   *)
  val get_players: t -> string list


  (*******************************************
   * methods for moving around on the map 
   *******************************************)

  (* [move map p dir n] tries to move player [p] on the [map] [n] steps in [dir]
   * direction. 
   * Returns: the pair [(i, map2)], where 
   *   [i]    is the steps remaining after going in [dir] direction, and 
   *   [map2] is the updated map. 
   *)
  val move: t -> string -> string -> int -> int * t

  (* [move_towards_coord map p (x,y) n] tries to move player [p] on the [map] 
   * [n] steps towards the coordinate [(x,y)]. 
   * Returns: the pair [(b, map2)], where 
   *   [b]    is [true] iff [p] succesfully made it to [(x,y)]
   *   [map2] is the updated map.
   * Raises: InvalidLocation if [(x,y)] is off the map or inside a room. 
   *)
  val move_toward_coord: t -> string -> int * int -> int -> bool * t
  
  (* [move_towards_room map p r n] tries to move player [p] on the [map] [n] 
   * steps towards the room with id [r].
   * Returns: the pair [(b, map2)], where 
   *   [b]    is [true] iff [p] succesfully made it to room with id [r]
   *   [map2] is the updated map.
   * Raises: InvalidLocation if [r] is not a valid room id.
   *)
  val move_towards_room: t -> string -> string -> int -> bool * t

  (* [teleport_player map p r] moves a person [p] on the [map] to room with room 
   * id [r]. This event occurs whenever a suggestion or accusation is made; the
   * suspect is moved to the "scene of the crime."
   * Returns: the updated map.
   *)
  val teleport_player: t -> string -> string -> t


  (*******************************************
   * Methods for player queries 
   *******************************************)
  
  (* [is_in_room map p] checks if player [p] is currently in a room on the [map]
   * Returns: [true] iff player[p] is in a room.
   *)
  val is_in_room: t -> string -> bool

  (* [get_current_room map p]
   * Returns: [some r] if player [p] is in room with id [r] or [None] if the [p]
   * is currently not in a room. *)
  val get_current_room: t -> string -> string option

  (* [get_current_location map p]
   * Returns: [(x,y)], the coordinate where player [p] is on the [map]. 
   * Raises: InvalidLocation exception if the player is in a room 
   *)
  val get_current_location: t -> string -> int * int

  (* [closest_rooms map p] 
   * Returns: a list of room ids of each room in order of closeness to player [p]
   *)
  val closest_rooms: t -> string -> string list


end
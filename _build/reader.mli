open Yojson.Basic.Util
open Data

(* [make_map] parses the json file storing information about the map and
 * converts it into a [map].
 * Returns: a [map] with only information about the map (no player info) *)
val make_map: unit -> map

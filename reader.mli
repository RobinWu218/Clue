open Yojson.Basic.Util
open Data

(* The type of source file *)
type j = Yojson.Basic.json

(* Methods of reading json files and loading into local types*)
(* [make_prof j] is the professor returned by a json *)
val make_prof : j -> prof

(* [make_building j] is the building returned by a json *)
val make_building : j -> building

(* [make_language j] is the language returned by a json *)
val make_language : j -> language

(* [make_card j] is the card returned by a json *)
val make_card : j -> card 

(* [make_case_file j] is the case_file returned by a json *)
val make_case_file : j -> case_file 

(* [make_state j] is the initial state returned by a json. This 
 * state includes the current map situation and player's and 
 * ais' information *)
val make_state : j -> state

(* [make_map] parses the json file storing information about the map and
 * converts it into a [map].
 * Returns: a [map] with only information about the map (no player info) *)
val make_map: unit -> map

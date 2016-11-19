open Yojson.Basic.Util
open Data

(* The type of source file *)
type j = Yojson.Basic.json

(* Methods of reading json files and loading into local types*)

(* [make_prof j] is the professor returned by a json *)
let make_prof j =
  failwith "unimplemented"

(* [make_building j] is the building returned by a json *)
let make_building j = 
  failwith "unimplemented"

(* [make_language j] is the language returned by a json *)
let make_language j = 
  failwith "unimplemented"

(* [make_card j] is the card returned by a json *)
let make_card j = 
  failwith "unimplemented"

(* [make_case_file j] is the case_file returned by a json *)
let make_case_file j =
  failwith "unimplemented"

(* [make_map] parses the json file storing information about the map and
 * converts it into a [map].
 * Returns: a [map] with only information about the map (no player info) *)
 let make_map () = 
  failwith "unimplemented"

(* [make_state j] is the initial state returned by a json. This 
 * state includes the current map situation and player's and 
 * ais' information *)
 let make_state j = 
  failwith "unimplemented"

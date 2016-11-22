open Yojson.Basic.Util
open Data

(* [make_map] parses the json file storing information about the map and
 * converts it into a [map].
 * Returns: a [map] with only information about the map (no player info) *)
 let make_map () = 
  {
    num_rows: 0; 
    num_cols: 0;
    map_values: Array.make_matrix num_rows num_cols Some "*";
    exits: []]; (* ("Gates", [ (1, (0,0)); (2,(5,5))] *)
    buildings: []];
    in_buildling: [];
    location: [];
    waiting_spots: [];
    secrets: [];
}

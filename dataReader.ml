(*1차 ! : 파일들 읽어서 데이터 저장하기*)

  let edgeList = ref [] (*memory for thple - MUTAG_A.txt*)
  let edgeLables = ref []
  let graphIndicator = ref []
  let graphLables = ref []
  let nodeLables = ref []

  (*The function to check the tuple list read*)
  let rec print_tuples tupleList 
  = match tupleList with
    | (e1, e2)::t -> Printf.printf "(%d, %d)" e1 e2; print_newline (); print_tuples t
    | _ -> print_newline ()
  
  let rec print_list _list 
  = match _list with
    | h::t -> Printf.printf "%d" h; print_newline (); print_list t
    | _ -> print_newline ()

  exception InputError
  
  (*The function to make the tuple - MUTAG_A.txt*)
  let make_tuple oFileLine 
  = let rawline = input_line oFileLine in (*raw line 형태는 'int, int'*)
    let sep = ',' in
    let _str = String.split_on_char sep rawline in
    match _str with
    | h::t -> 
      match t with
      | h2::t -> (int_of_string h, int_of_string (String.trim h2))
      | h2::[] -> (int_of_string h, int_of_string (String.trim h2))
      | _ -> raise InputError
    | h::[] -> raise InputError
    | _ -> raise InputError
  
  (*The function to extend memory for tuple list - MUTAG_A.txt*)
  let read_graphEdge oFile
  = let lines = ref 0 in (*to check the number of lines*)
    try
      while true do
        let line = make_tuple oFile in
        lines := !lines +1; (*to check the number of lines*)
        edgeList := ( line :: !edgeList )
      done 
    with 
    End_of_file ->
      (* to check the number of lines - behind four lines*)
    (*
      print_string "There were ";
      print_int !lines;
      print_string " lines.";
      print_newline ()
    *)
      print_newline ()
  
  (*명령어 : file_graphEdge "MUTAG_A.txt";;*)   
  let file_graphEdge filename 
  = let oFile = open_in filename in
    try
      read_graphEdge oFile;
      close_in oFile;
    with
    _ -> close_in oFile


  let read_label_or_indicator oFile num
  = let lines = ref 0 in
    try
      while true do
        let line = input_line oFile in
        lines := !lines +1;
        match num with
        | 1 -> edgeLables := ( int_of_string line :: !edgeLables )
        | 2 -> graphIndicator := ( int_of_string line :: !graphIndicator )
        | 3 -> graphLables := ( int_of_string line :: !graphLables )
        | 4 -> nodeLables := ( int_of_string line :: !nodeLables )
        | _ -> raise InputError
      done 
    with 
    End_of_file -> print_newline ()

  let file_label_or_indicator filename
  = let oFile = open_in filename in
    try
      match filename with
      | "MUTAG_edge_labels.txt" -> read_label_or_indicator oFile 1
      | "MUTAG_graph_indicator.txt" -> read_label_or_indicator oFile 2
      | "MUTAG_graph_labels.txt" -> read_label_or_indicator oFile 3
      | "MUTAG_node_labels.txt" -> read_label_or_indicator oFile 4
      | _ -> raise InputError
      close_in oFile;
    with 
    _ -> close_in oFile


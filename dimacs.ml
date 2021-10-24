let read_from_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      let line = input_line chan in
      if (String.length line>0)&&(not ((line.[0]) = 'c')) then 
	lines := (!lines)^" "^line
    done; ""
  with End_of_file ->
    close_in chan;
    !lines
      
let rec parse_cnf cnf_so_far = function
  | []     -> List.rev cnf_so_far
  | "0"::l -> parse_cnf (cnf_so_far) l
  | l -> let rec parse_clause clause_so_far = function
    | []     -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) []
    | "0"::l -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) l
    | s::l   -> parse_clause ((int_of_string s)::clause_so_far) l
         in parse_clause [] l
         
let rec parse_cnf_file = function
  | []     -> []
  | "p"::"cnf"::_::_::l -> parse_cnf [] l
  | a::l -> parse_cnf_file l
     
let parse x =
  Str.split (Str.regexp "[ \n\t\r\012]+") (read_from_file x)
  |> parse_cnf_file

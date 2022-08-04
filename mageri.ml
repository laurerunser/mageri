open Unix (* for socket programming *)

(* to store menu maps *)
let menu = []

(* to store the history *)
let hist = []

(* Checks if the string `url` contains "://" *)
let url_contains_scheme (url : Uri.t) : bool =
  let url = Uri.to_string url in
  let re = Str.regexp_string "://" in
  try ignore (Str.search_forward re url 0); true
  with Not_found -> false

(* Get the netloc from the url.
The url is necessarily of the form scheme://netloc/something
So splitting the string gives ["scheme:"; ""; "netloc"; "something"]*)
let get_netloc (url : Uri.t) : string =
  let url = Uri.to_string url in
  let s = String.split_on_char '/' url in
  List.nth s 2


(* Open a socket connection to the server on port 1965
   - ip_addr is the ip address of the server *)
let create_socket (ip_addr: inet_addr) : Ssl.socket =
    (* init SSL : this is NOT THREAD-SAFE.
       To make it thread-safe, use Ssl_threads instead of Ssl *)
    Ssl.init ();

    (* create the SSL context (SSLv23 allows all SSL protocols) *)
    let context = Ssl.create_context SSLv23 Client_context in

    let port = 1965 in (* the port on the server *)
  
    (* ip_addr was a `inet_addr` -> convert it into a `sockaddr` *)
    let sockaddr = Unix.ADDR_INET (ip_addr, port) in

    (* create the socket and open the connection *)
    let sockfd = Ssl.open_connection_with_context context sockaddr in

    (* return the socket*)
    sockfd
  

let get_sockfd (url : Uri.t) : Ssl.socket =
   (* get the server's ip*)
   let hostname = get_netloc url in
   let host_entries = gethostbyname hostname in
   let ip_addr = host_entries.h_addr_list.(0) in
 
   (* create a socket and open the connection *)
   let sockfd = create_socket ip_addr in
 
   (* verify the certificate *)
   (try Ssl.verify sockfd
   with _ -> failwith "Can't verify the SSL certificate");
 
   (* connect the socket *)
   (try Ssl.connect sockfd
   with _ -> failwith "Can't connect the socket");

   (* return the connected socket*)
   sockfd

(* Read the header from the socket and return (status code, mime) *)
let read_header (sockfd:Ssl.socket) : (float * string) =
  let header = Ssl.input_string sockfd in
  let split = String.split_on_char ' ' header in
  let status = float_of_string (List.nth split 0) in (* convert status code for easier handling later *)
  (status, List.nth split 1)


let display_content (sockfd: Ssl.socket) (meta : string) =
  if String.starts_with "text/" meta then


  print_string ""

let rec gemini_transaction (url : Uri.t) =
  (* get the socket ready to use *)
  let sockfd = get_sockfd url in

  (* Make the first query <URL><CR><LF> into a byte buffer *)
  let query = Uri.to_string url ^ "\r\n" in
  let bytes_to_send = String.to_bytes query in

  (* send the first query *)
  let _ = Ssl.write sockfd bytes_to_send 0 (Bytes.length bytes_to_send) in

  (* read the header and return the status code and mime *)
  let (status, meta) = read_header sockfd in

  (* depending on the status code, do the appropriate action *)
  match floor(status /. 10.0) with
  | 1.0 -> (* INPUT : the ressource is requesting a line of user input. 
              The `meta` line is a prompt that should be shown to the user.
              The user's answer is then sent back : `url?answer`. 
              Spaces and reserved characters must be percent-encoded*)
              print_string meta; (* print the prompt *)
              let answer = read_line () in (* get the user's answer *)
              let encoded_answer = Uri.pct_encode answer in (* encode the answer *)
              let new_url_string = (Uri.to_string url) ^ "?" ^ encoded_answer in (* append the answer to the url *)
              let new_url = Uri.of_string new_url_string in (* make the new url *)
              gemini_transaction new_url
  | 2.0 -> (* SUCCESS ! Print the page : `meta` contains the mime type of the info in the response body*)
      display_content sockfd meta
  | 3.0 -> (* REDIRECT *)
      let relative = Uri.of_string meta in 
      let new_url = (* make the new url : must be absolute *)
        (if url_contains_scheme relative then relative (* already absolute url *)
        else Uri.add_query_param' url ("", meta)) (* appen `meta` to the end of the currenct url *)
      in gemini_transaction new_url
  | 4.0 | 5.0 | 6.0 -> (* FAILURE : it would be better to make separate cases 
                        but this is enough for now *)
      failwith (Printf.sprintf "Failure %f : %s" status meta)
  | _-> failwith "Unknown status code"

let main_loop = 
  (* get user input *)
  print_string "> ";
  let cmd = String.trim (read_string ()) in

  let url = 
    (match cmd with
    | "q" -> print_string "Goodbye !"; exit 0
    | "b" -> (* go back in the history *)
      let u = List.hd hist in
      let hist = List.tl hist in Uri.of_string u
    | x ->
      (* if x is an int, the user chose a link in the menu *)
      (try let choice = int_of_string x in Uri.of_string (menu[choice-1])
      (* otherwise x is a link to a gemini page.*)
      (* add the gemini prefix if no scheme is present *)
      with _ -> 
        if url_contains_scheme x then Uri.of_string x
        else Uri.of_string ("gemini://" ^ x) )
    ) in

    (* do the transaction *)
    gemini_transaction url


let main = main_loop ""; ()

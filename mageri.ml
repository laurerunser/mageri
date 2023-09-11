open Unix
(* for socket programming *)

(* to store menu maps *)
(* let menu : string list = []

   (* to store the history *)
   (* let hist = [] *)

   (* Checks if the string `url` contains "://" *)
   let url_contains_scheme (url : Uri.t) : bool =
     let url = Uri.to_string url in
     let re = Str.regexp_string "://" in
     try
       ignore (Str.search_forward re url 0);
       true
     with Not_found -> false

   (* Get the netloc from the url.
      The url is necessarily of the form scheme://netloc/something
      So splitting the string gives ["scheme:"; ""; "netloc"; "something"]*)
   let get_netloc (url : Uri.t) : string =
     let url = Uri.to_string url in
     let s = String.split_on_char '/' url in
     List.nth s 2
*)
(* Open a socket connection to the server on port 1965
   - ip_addr is the ip address of the server *)
let create_socket (ip_addr : inet_addr) port : Ssl.socket =
  Printf.printf "creating socket\n%!";
  (* init SSL : this is NOT THREAD-SAFE.
     To make it thread-safe, use Ssl_threads instead of Ssl *)
  Ssl.init ();
  let sockaddr = ADDR_INET (ip_addr, port) in
  (*
     (* create the SSL context (SSLv23 allows all SSL protocols) *)
     let ssl_context = Ssl.create_context SSLv23 Client_context in

     (* ip_addr was a `inet_addr` -> convert it into a `sockaddr` *)
     let sockaddr = Unix.ADDR_INET (ip_addr, port) in

     (* create the socket and open the connection *)
     Printf.printf "opening sock to %s:%d...\n%!"
       (Unix.string_of_inet_addr ip_addr)
       port;
     let sockfd = Ssl.open_connection_with_context ssl_context sockaddr in
     Printf.printf "4%!";

     (* return the socket*)
     Printf.printf "socket created and connection opened\n%!"; *)
  let ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Client_context in
   Ssl.set_client_verify_callback_verbose true;
  Ssl.set_verify ctx [] None;
  (* Ssl.set_verify ctx [ Ssl.Verify_peer ] (Some Ssl.client_verify_callback);
  Ssl.set_verify_depth ctx 3; *)
  Printf.printf "before opening connection\n%!";
  Printf.printf "opening sock to %s:%d...\n%!"
  (Unix.string_of_inet_addr ip_addr)
  port;
  let socket = Ssl.open_connection_with_context ctx sockaddr in 
  Printf.printf "created socket\n%!";
  socket

let get_sockfd (url : Uri.t) : Ssl.socket =
  (* get the server's ip*)
  let host, port =
    let host = Uri.host url in
    let port = Uri.port url in
    match (host, port) with
    | None, _ -> failwith "host is required"
    | Some host, None -> (host, 1965) (* default post is 1965 *)
    | Some host, Some port -> (host, port)
  in
  let ip_addr =
    try
      let list = Unix.getaddrinfo host "" [] in
      List.find
        (fun a ->
          match a with
          | { Unix.ai_addr = Unix.ADDR_INET (_, _); ai_family = PF_INET; _ } ->
              true
          | _ -> false)
        list
    with Not_found -> failwith "can't resolve the address"
  in
  let ip =
    match ip_addr with
    | { Unix.ai_addr = Unix.ADDR_INET (ip, _); ai_family = PF_INET; _ } -> ip
    | _ -> assert false (* should have failed just before *)
  in

  (* create a socket and open the connection *)
  let sockfd = create_socket ip port in
  Printf.printf "COnnection%!";

  (* verify the certificate *)
  (try Ssl.verify sockfd with _ -> failwith "Can't verify the SSL certificate");
  Printf.printf "socket verified";

  (* connect the socket *)
  (try Ssl.connect sockfd with _ -> failwith "Can't connect the socket");
  Printf.printf "socket connected";

  (* return the connected socket*)
  sockfd

(* Read the header from the socket and return (status code, mime) *)
let read_header (sockfd : Ssl.socket) : float * string =
  let header = Ssl.input_string sockfd in
  let split = String.split_on_char ' ' header in
  (* convert status code for easier handling later *)
  let status = float_of_string (List.nth split 0) in
  (status, List.nth split 1)

let read_body (sockfd : Ssl.socket) : string =
  let buf = Buffer.create 32 in
  let bytes_buf = Bytes.create 1024 in
  let continue = ref true in
  while !continue do
    let len =
      try Ssl.read sockfd bytes_buf 0 (Bytes.length bytes_buf)
      with _ ->
        failwith (Printf.sprintf "SSL error" )
        (* todo: print nice error message: (Ssl.Error.(Ssl.Error.get_error ()) gets the error; need to write a to_string function for it *)
    in
    if len = 0 then continue := false
    else Buffer.add_subbytes buf bytes_buf 0 len
  done;
  Buffer.contents buf

(* let display_content (sockfd : Ssl.socket) (meta : string) =
   (* if String.starts_with "text/" meta then print_string "" *)
   print_string "" *)

let rec gemini_transaction (url : Uri.t) max_redirects =
  (* get the socket ready to use *)
  let sockfd = get_sockfd url in

  (* Make the first query <URL><CR><LF> into a byte buffer *)
  let query = Uri.to_string url ^ "\r\n%!" in
  let bytes_to_send = String.to_bytes query in

  (* send the first query *)
  let _ = Ssl.write sockfd bytes_to_send 0 (Bytes.length bytes_to_send) in
  Ssl.flush sockfd;
  Printf.printf "sent the first query\n%!";

  (* read the header and return the status code and mime *)
  let status, meta = read_header sockfd in

  (* depending on the status code, do the appropriate action *)
  if status >= 20. && status < 30. then (
    let body = read_body sockfd in
    Ssl.shutdown_connection sockfd;
    Ok (status, meta, body))
  else if status >= 30. && status < 40. then
    if max_redirects > 0 then
      let url' = Uri.of_string meta in
      gemini_transaction url' (max_redirects - 1)
    else Error (status, "max redirects reached")
  else (
    Ssl.shutdown_connection sockfd;
    Error (status, meta))
(* match floor (status /. 10.0) with
   | 1.0 ->
       (* INPUT : the ressource is requesting a line of user input.
          The `meta` line is a prompt that should be shown to the user.
          The user's answer is then sent back : `url?answer`.
          Spaces and reserved characters must be percent-encoded*)
       print_string meta;
       (* print the prompt *)
       let answer = read_line () in
       (* get the user's answer *)
       let encoded_answer = Uri.pct_encode answer in
       (* encode the answer *)
       let new_url_string = Uri.to_string url ^ "?" ^ encoded_answer in
       (* append the answer to the url *)
       let new_url = Uri.of_string new_url_string in
       (* make the new url *)
       gemini_transaction new_url
   | 2.0 ->
       (* SUCCESS ! Print the page : `meta` contains the mime type of the info in the response body*)
       display_content sockfd meta
   | 3.0 ->
       (* REDIRECT *)
       let relative = Uri.of_string meta in
       let new_url =
         (* make the new url : must be absolute *)
         if url_contains_scheme relative then relative (* already absolute url *)
         else Uri.add_query_param' url ("", meta)
         (* appen `meta` to the end of the currenct url *)
       in
       gemini_transaction new_url
   | 4.0 | 5.0 | 6.0 ->
       (* FAILURE : it would be better to make separate cases
          but this is enough for now *)
       failwith (Printf.sprintf "Failure %f : %s" status meta)
   | _ -> failwith "Unknown status code" *)

let main_loop str =
  (* get user input *)
  (* print_string "> ";
     (* let str = read_string () in *) *)
  let cmd = String.trim str in
  Printf.printf "Received cmd: %s\n%!" cmd;

  let url =
    Uri.of_string cmd
    (* match cmd with
       | "q" ->
           print_string "Goodbye !";
           exit 0
       (* | "b" ->
           (* go back in the history *)
           let u = List.hd hist in
           let hist = List.tl hist in
           Uri.of_string u *)
       | x -> (
           (* if x is an int, the user chose a link in the menu *)
           try
             let choice = int_of_string x in
             Uri.of_string (List.nth menu (choice - 1))
             (* otherwise x is a link to a gemini page.*)
             (* add the gemini prefix if no scheme is present *)
           with _ ->
             (* if url_contains_scheme x then Uri.of_string x
                else Uri.of_string ("gemini://" ^ x)) *)
             Uri.of_string x) *)
  in
  Printf.printf "Uri is : %s\n%!" (Uri.to_string url);

  (* do the transaction *)
  let res = gemini_transaction url 0 in
  match res with
  | Error (status, meta) ->
      failwith (Printf.sprintf "Failure, code %f; meta: %s" status meta)
  | Ok (_, _, body) ->
      Printf.printf "Body received. Printing now\n%!";
      print_string body

let () = main_loop "gemini://skyjake.fi"

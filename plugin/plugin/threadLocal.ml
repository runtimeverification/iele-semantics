let thread_id () = Thread.id (Thread.self())

type 'a t = ((int,'a) Hashtbl.t * Mutex.t)

let create size : 'a t =
  (Hashtbl.create size, Mutex.create ())

let find (hash,lock) =
  Mutex.lock lock;
  let ret = try Hashtbl.find hash (thread_id ())
            with exn -> Mutex.unlock lock;raise exn
  in Mutex.unlock lock;ret
let getOrUpdate (hash,lock) compute =
  Mutex.lock lock;
  let me = thread_id() in
  let ret =
    try Hashtbl.find hash me
    with Not_found ->
      let init = compute ()
      in Hashtbl.replace hash me init; init
  in Mutex.unlock lock;ret
let remove (hash,lock) =
  Mutex.lock lock;
  Hashtbl.remove hash (thread_id ());
  Mutex.unlock lock
let finalize (hash,lock) clean =
  Mutex.lock lock;
  let me = thread_id () in
  try (let value = Hashtbl.find hash me
      in clean value; Hashtbl.remove hash me)
  with Not_found -> ();
  Mutex.unlock lock
let put (hash,lock) value =
  Mutex.lock lock;
  Hashtbl.replace hash (thread_id ()) value;
  Mutex.unlock lock

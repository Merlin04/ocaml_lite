module type ContextEntry = sig
  type t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Make(E : ContextEntry) = struct
  type 'a t = 'a * (entry list)
  and entry = E.t
  [@@deriving show]

  let return s = (s, [])
  let make s c : 'a t = (s, c)
  let value (a : 'a t) = fst a
  let list (a : 'a t) = snd a
  let from s (c : 'a t) = make s (list c)
  (* bind *)
  let ( >>= ) (a : 'a t) (f : 'a -> 'b t) : 'b t = let v, l2 = a |> fst |> f in (v, l2 @ (snd a))
  let ( let* ) = ( >>= )
  (* map *)
  let ( >|= ) (a : 'a t) (f : 'a -> 'b) : 'b t = let v, l = a in (f v, l)
  let ( let+ ) = ( >|= )
  (* map for monad contents *)
  let map2 (a : 'a t) (f : entry list -> entry list) = let v, l = a in (v, f l)
  let ( let- ) = ( map2 )
  let add (e : entry) (a : 'a t) = let- c = a in e :: c
  let join (e : entry list) (a : 'a t) = let- c = a in e @ c
  let join_end (e : entry list) (a : 'a t) = let- c = a in c @ e
end
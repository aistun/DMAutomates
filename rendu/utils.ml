module type ListMap = sig
  type key
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  val add_list : key -> 'a list -> 'a t -> 'a t
  val remove_key : key -> 'a t -> 'a t
  val remove_key_values : key -> 'a -> 'a t -> 'a t

  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a list

  val map_map : ('a list -> 'b list) -> 'a t -> 'b t
  val filter_map : (key -> 'a list -> bool) -> 'a t -> 'a t
  val fold_map : (key -> 'a list -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter_map : (key -> 'a list -> unit) -> 'a t -> unit

  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold_key : ('a -> 'b -> 'b) -> key -> 'a t -> 'b -> 'b
end

module ListMap(Ord : Map.OrderedType) : ListMap with type key = Ord.t
= struct
  type key = Ord.t

  module Map = Map.Make(Ord)

  type 'a t = 'a list Map.t

  let empty = Map.empty

  let add k v m = Map.add k (v :: (try Map.find k m with Not_found -> [])) m
  let add_list k vl m = Map.add k (vl @ (try Map.find k m with Not_found -> [])) m
  let remove_key k m = Map.remove k m
  let remove_list e l = List.filter (fun x -> x <> e) l
  let remove_key_values k v m =
    try
      let l  = Map.find k m in
      let l' = remove_list v l in
      if l' = [] then Map.remove k m
      else Map.add k l' m
    with Not_found -> m

  let mem k m = Map.mem k m
  let find k m = Map.find k m

  let map_map f m = Map.map f m
  let filter_map f m = Map.filter f m
  let fold_map f m b = Map.fold f m b
  let iter_map f m = Map.iter f m

  let clear_empty_lists m = Map.filter (fun _ l -> l <> []) m

  let map f m = Map.map (fun l -> List.map f l) m
  let filter f m =
    let m' = Map.fold (fun k l m' ->
        Map.add k (List.filter (fun v -> f k v) l) m'
      ) m Map.empty in
    clear_empty_lists m'
  let fold f m b =
    Map.fold (fun k l m' ->
        List.fold_left (fun m' v -> f k v m') m' l
      ) m b
  let iter f m =
    Map.iter (fun k l ->
        List.iter (fun v -> f k v) l
      ) m

  let fold_key f k m b =
    List.fold_left (fun b v ->
        f v b
      ) b (find k m)
end


module type Functor = sig
    type 'a t
    val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type Monad = sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Free = functor (F : Functor) -> struct
    type 'a t = Return of 'a
              | Wrap of ('a t) F.t

    let return a = Return a

    let rec bind m f =
        match m with
        | Return x -> f x
        | Wrap x -> Wrap (F.fmap (fun m -> bind m f) x)

    let liftF (fa : 'a F.t) : 'a t =
        Wrap (F.fmap (fun a -> Return a) fa)

end

module type MonadExtra = sig
    type 'a t

    val join : 'a t t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t

    val seq : 'a t -> 'b t -> 'b t
end

module MonadUtils = functor (M : Monad) -> struct
    type 'a t = 'a M.t

    let join m = M.bind m (fun x -> x)
    let map f m = M.bind m (fun x -> M.return (f x))

    let seq m n = M.bind m (fun _ -> n)
end


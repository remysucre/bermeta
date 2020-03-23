module type RING = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
end

module ExArith(R:RING) = struct
  open R
  let xsq1 x = add (mul x x) one
end

module RingFloat = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = Pervasives.(+.)
  let sub = Pervasives.(-.)
  let mul = Pervasives.( *.)
end
;;

module RingFloatCode = struct
  type t = float code
  let zero = .< 0. >.
  let one = .< 1. >.
  let add = fun x y -> .<.~x +. .~y>.
  let sub = fun x y -> .<.~x -. .~y>.
  let mul = fun x y -> .<.~x *. .~y>.
end
;;

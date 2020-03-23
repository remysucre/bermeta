open Cmplx
open Ring

(* To be domain of vectors, cmplx must be a RING *)
(* QQQ: What if we forget sub? *)
module RingCmplx(R:RING) = struct
  type t = R.t cmplx
  let zero = {re = R.zero; im = R.zero}
  let one  = {re = R.one;  im = R.zero}
  let add = fun x y ->
    {re = R.add x.re y.re;
     im = R.add x.im y.im}
  let sub = fun x y ->
    {re = R.sub x.re y.re;
     im = R.sub x.im y.im}
  let mul = fun x y ->
    {re = R.sub (R.mul x.re y.re) (R.mul x.im y.im);
     im = R.add (R.mul x.re y.im) (R.mul x.im y.re)}
end;;

(* Now, float cmplx is a RING too *)

(* Adding abbreviations since FloatComplex will be used often *)
module FloatComplex = RingCmplx(RingFloat)
type float_complex = FloatComplex.t

module FloatCodeComplex = RingCmplx(RingFloatCode)
type float_code_complex = FloatCodeComplex.t

(* QQQ What is the purpose of those functions? *)
(* QQQ Think of let-insertion on x *)
let of_complex_code : float cmplx code -> float_code_complex =
  fun x -> {re = .<(.~x).re>.; im = .<(.~x).im>.}
let of_code_complex : float_code_complex -> float cmplx code =
  fun {re;im} -> .<{re = .~re; im = .~im}>.

let _ =
  let f = let module M = ExArith(FloatCodeComplex) in M.xsq1
  in .<fun x ->
         .~(of_code_complex
              (f (of_complex_code .<x>.)))>.
;;

module FloatComplexCode = struct
  type t = float cmplx code
  let zero = .<{re = 0.; im = 0.}>.
  let one = .<{re = 1.; im = 0.}>.
  let add = fun x y -> .<{re = (.~x).re +. (.~y).re; im = (.~x).im +. (.~y).im}>.
  let sub = fun x y -> .<{re = (.~x).re -. (.~y).re; im = (.~x).im -. (.~y).im}>.
  let mul = fun x y -> .<{re = ((.~x).re *. (.~y).re) -.((.~x).im *. (.~y).im);
                        im = ((.~x).re *. (.~y).im) +. ((.~x).im *. (.~y).re)}>.
end
;;

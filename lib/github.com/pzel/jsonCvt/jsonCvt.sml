structure JsonCvt =
struct

open Either.Cons

type 'a decoder = Json.t -> (string,'a) either
type 'a result = (string, 'a) either
type error = string

fun decodeValue (d: 'a decoder) (v: Json.t) : 'a result = d v;

fun decodeString (d: 'a decoder) (s: string) : 'a result =
    Json.fromString s >| decodeValue d
    handle Fail jsonError => INL jsonError

fun succeed (v: 'a) (j: Json.t) : 'a result = INR v
fun fail (v: string) (j: Json.t) : 'a result = INL v

local
(* todo hide these via signature *)
fun e (onNone: 'a) (opt : 'b option) : ('a, 'b) either =
    case opt of SOME b => INR b | _ => INL onNone;

val ts = Json.toString
fun parseInt s = Int.fromString s handle Overflow => NONE
fun parseReal s = Real.fromString s handle _ => NONE

in

fun null (v: 'a) (j: Json.t) : 'a result =
    case j of Json.NULL => INR v | _ => INL \> "Not null: " ^ ts j

fun int (j: Json.t) : int result =
    case j
     of Json.NUMBER n => e ("Can't convert to int: " ^n) (parseInt n)
      | jv => INL \> "Not a number: " ^ ts jv

fun real (j: Json.t) : real result =
    case j
     of Json.NUMBER n => e ("Can't convert to real: " ^n) (parseReal n)
      | jv => INL \> "Not a number: " ^ ts jv

fun bool (j: Json.t) : bool result =
    case j
     of Json.BOOL b => INR b
      | jv => INL \> "Not a bool: " ^ ts jv

fun string (j: Json.t) : string result =
    case j
     of Json.STRING s => INR s
      | _ => INL \> "Not a string: " ^ ts j

fun field (v: string) (p: 'a decoder) (j: Json.t) : 'a result =
    let fun fieldError j = "No field '"^v^"' in: " ^ ts j
    in case j
        of Json.OBJECT obj => Json.objLook obj v
                                           >| e (fieldError j)
                                           >| Either.bindRight p
         | _ => INL \> fieldError j
    end


end
end

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

fun raw (j: Json.t) : Json.t result = INR j

(* todo hide these via ascription *)
val ts = Json.toString
fun parseInt s = Int.fromString s handle Overflow => NONE
fun parseReal s = Real.fromString s handle _ => NONE

fun null (v: 'a) (j: Json.t) : 'a result =
    case j of Json.NULL => INR v | _ => INL \> "Not null: " ^ ts j

fun int (j: Json.t) : int result =
    case j
     of Json.NUMBER n => Either.fromOption ("Can't convert to int: " ^n) (parseInt n)
      | jv => INL \> "Not a number: " ^ ts jv

fun real (j: Json.t) : real result =
    case j
     of Json.NUMBER n => Either.fromOption ("Can't convert to real: " ^n) (parseReal n)
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
                                           >| Either.fromOption (fieldError j)
                                           >| Either.bindRight p
         | _ => INL \> fieldError j
    end

fun map2 (p1: 'a decoder) (p2: 'b decoder) (j: Json.t) : ('a, 'b) product result =
    case (p1 j, p2 j)
     of (INR r1, INR r2) => INR (r1 & r2)
      | (INL e1,  _) => INL e1
      | (_, INL e2) => INL e2

fun map3 (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (j: Json.t) 
    : (('a, 'b) product , 'c) product result =
    case (p1 j, p2 j, p3 j)
     of (INR r1, INR r2, INR r3) => INR (r1 & r2 & r3)
      | (INL e1,  _, _) => INL e1
      | (_, INL e2, _ ) => INL e2
      | (_, _, INL e3 ) => INL e3


end

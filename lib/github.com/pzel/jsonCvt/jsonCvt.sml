structure JsonCvt =
struct

open Either.Cons

type 'a decoder = Json.t -> (string,'a) either
type error = string

fun decodeValue (d: 'a decoder) (v: Json.t) : (string, 'a) either = d v;

fun decodeString (d: 'a decoder) (s: string) : (string, 'a) either =
    Json.fromString s >| decodeValue d
    handle Fail jsonError => INL jsonError


fun succeed (v: 'a) (j: Json.t) : (string, 'a) either = INR v
fun fail (v: string) (j: Json.t) : (string, 'a) either = INL v

end

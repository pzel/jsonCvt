structure JsonCvt : JSON_CVT =
struct

open Either.Cons

type 'a decoder = Json.t -> (string,'a) either
type cvtError = string
type 'a result = (cvtError, 'a) either


val ts = Json.toString
fun parseInt s = Int.fromString s handle Overflow => NONE
fun parseReal s = Real.fromString s handle _ => NONE

fun decodeValue (d: 'a decoder) (v: Json.t) : 'a result = d v;

fun decodeString (d: 'a decoder) (s: string) : 'a result =
    Json.fromString s >| decodeValue d
    handle Fail jsonError => INL jsonError

fun succeed (v: 'a) (j: Json.t) : 'a result = INR v
fun fail (v: string) (j: Json.t) : 'a result = INL v

fun raw (j: Json.t) : Json.t result = INR j

fun null (v: 'a) (j: Json.t) : 'a result =
    case j of Json.NULL => INR v | _ => INL \> "Not null: "^ts j

fun int (j: Json.t) : int result =
    case j
     of Json.NUMBER n =>
        Either.fromOption ("Can't convert to int: " ^n) (parseInt n)
      | jv =>
        INL \> "Not a number: "^ts jv

fun real (j: Json.t) : real result =
    case j
     of Json.NUMBER n =>
        Either.fromOption ("Can't convert to real: " ^n) (parseReal n)
      | jv =>
        INL \> "Not a number: "^ts jv

fun bool (j: Json.t) : bool result =
    case j
     of Json.BOOL b => INR b
      | jv => INL \> "Not a bool: "^ts jv

fun string (j: Json.t) : string result =
    case j
     of Json.STRING s => INR s
      | _ => INL \> "Not a string: "^ts j

fun field (v: string) (p: 'a decoder) (j: Json.t) : 'a result =
    let fun fieldError j = "No field '"^v^"' in: "^ts j
    in case j
        of Json.OBJECT obj => (Json.objLook obj v
                              >| Either.fromOption (fieldError j)
                              >| Either.bindRight p)
         | _ => INL \> fieldError j
    end

fun list (p: 'a decoder) (j: Json.t) : 'a list result =
    case j
     of Json.ARRAY l =>
        let val res = map (Either.asRight o p) l
            val allGood = List.all Option.isSome res
        in if allGood then INR \> map Option.valOf res
           else INL \> "Failed to parse list: "^ts j
        end
      | _ => INL \> "Not a list: "^ts j

fun nullable (p: 'a decoder) (j: Json.t) : 'a option result =
    case j
     of Json.NULL => INR NONE
      | _ => p j </ Either.mapRight SOME

fun at (keys: string list) (p: 'a decoder) (j: Json.t) : 'a result =
    let fun build [] = p
          | build (k::ks) = field k (build ks)
    in build keys j
    end

fun index (idx: int) (p: 'a decoder) (j: Json.t) : 'a result =
    case j
     of Json.ARRAY l =>
        let fun err () = concat [
                  "Index out of bounds: idx=", Int.toString idx,
                  " len=", Int.toString (List.length l)]
            val item = INR \> List.nth(l, idx) handle Subscript => INL \> err()
        in Either.bindRight p item
        end
      | _ => INL \> "Not indexable: "^ts j

fun map (f: 'a -> 'b) (p: 'a decoder)  (j: Json.t)
    : 'b result = Either.mapRight f (p j)

fun map2 (f: 'a -> 'b -> 'c)
         (p1: 'a decoder) (p2: 'b decoder) (j: Json.t)
    : 'c result =
    case (p1 j, p2 j)
     of (INR r1, INR r2) => INR (f r1 r2)
      | (INL e1,  _) => INL e1
      | (_, INL e2) => INL e2

fun map3 (f: 'a -> 'b -> 'c -> 'd)
         (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (j: Json.t)
    : 'd result =
    case (p1 j, p2 j, p3 j)
     of (INR r1, INR r2, INR r3) => INR (f r1 r2 r3)
      | (INL e1,  _, _) => INL e1
      | (_, INL e2, _ ) => INL e2
      | (_, _, INL e3 ) => INL e3

fun map4 (f: 'a -> 'b -> 'c -> 'd -> 'e)
        (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder) (j: Json.t)
    : 'e result =
    case (p1 j, p2 j, p3 j, p4 j)
     of (INR r1, INR r2, INR r3, INR r4) => INR (f r1 r2 r3 r4)
      | (INL e1,  _, _, _) => INL e1
      | (_, INL e2, _ , _) => INL e2
      | (_, _, INL e3 , _) => INL e3
      | (_, _, _ , INL e4) => INL e4

fun map5 (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f)
        (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder)
         (p5: 'e decoder) (j: Json.t)
    : 'f result =
    case (p1 j, p2 j, p3 j, p4 j, p5 j)
     of (INR r1, INR r2, INR r3, INR r4, INR r5) =>
        INR (f r1 r2 r3 r4 r5)
      | (INL e1,  _, _, _, _) => INL e1
      | (_, INL e2, _ , _, _) => INL e2
      | (_, _, INL e3 , _, _) => INL e3
      | (_, _, _ , INL e4, _) => INL e4
      | (_, _, _ , _, INL e5) => INL e5

fun map6 (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g)
         (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder)
         (p5: 'e decoder) (p6: 'f decoder) (j: Json.t)
    : 'g result =
    case (p1 j, p2 j, p3 j, p4 j, p5 j, p6 j)
     of (INR r1, INR r2, INR r3, INR r4, INR r5, INR r6) =>
        INR (f r1 r2 r3 r4 r5 r6)
      | (INL e1,  _, _, _, _, _) => INL e1
      | (_, INL e2, _ , _, _, _) => INL e2
      | (_, _, INL e3 , _, _, _) => INL e3
      | (_, _, _ , INL e4, _, _) => INL e4
      | (_, _, _ , _, INL e5, _) => INL e5
      | (_, _, _ , _, _, INL e6) => INL e6


fun map7 (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h)
        (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder)
        (p5: 'e decoder) (p6: 'f decoder) (p7: 'g decoder) (j: Json.t)
    : 'h result =
    case (p1 j, p2 j, p3 j, p4 j, p5 j, p6 j, p7 j)
     of (INR r1, INR r2, INR r3, INR r4, INR r5, INR r6, INR r7) =>
        INR (f r1 r2 r3 r4 r5 r6 r7)
      | (INL e1,  _, _, _, _, _, _) => INL e1
      | (_, INL e2, _ , _, _, _, _) => INL e2
      | (_, _, INL e3 , _, _, _, _) => INL e3
      | (_, _, _ , INL e4, _, _, _) => INL e4
      | (_, _, _ , _, INL e5, _, _) => INL e5
      | (_, _, _ , _, _, INL e6, _) => INL e6
      | (_, _, _ , _, _, _, INL e7) => INL e7

fun map8 (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i)
         (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder)
         (p5: 'e decoder) (p6: 'f decoder) (p7: 'g decoder) (p8: 'h decoder)
         (j: Json.t)
    : 'i result =
    case (p1 j, p2 j, p3 j, p4 j, p5 j, p6 j, p7 j, p8 j)
     of (INR r1, INR r2, INR r3, INR r4, INR r5, INR r6, INR r7, INR r8) =>
        INR (f r1 r2 r3 r4 r5 r6 r7 r8)
      | (INL e1,  _, _, _, _, _, _, _) => INL e1
      | (_, INL e2, _ , _, _, _, _, _) => INL e2
      | (_, _, INL e3 , _, _, _, _, _) => INL e3
      | (_, _, _ , INL e4, _, _, _, _) => INL e4
      | (_, _, _ , _, INL e5, _, _, _) => INL e5
      | (_, _, _ , _, _, INL e6, _, _) => INL e6
      | (_, _, _ , _, _, _, INL e7, _) => INL e7
      | (_, _, _ , _, _, _, _, INL e8) => INL e8

fun map9 (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j)
         (p1: 'a decoder) (p2: 'b decoder) (p3: 'c decoder) (p4: 'd decoder)
         (p5: 'e decoder) (p6: 'f decoder) (p7: 'g decoder) (p8: 'h decoder)
         (p9: 'i decoder) (j: Json.t)
    : 'j result =
    case (p1 j, p2 j, p3 j, p4 j, p5 j, p6 j, p7 j, p8 j, p9 j)
     of (INR r1, INR r2, INR r3, INR r4, INR r5, INR r6, INR r7, INR r8, INR r9) =>
        INR (f r1 r2 r3 r4 r5 r6 r7 r8 r9)
      | (INL e1,  _, _, _, _, _, _, _, _) => INL e1
      | (_, INL e2, _ , _, _, _, _, _, _) => INL e2
      | (_, _, INL e3 , _, _, _, _, _, _) => INL e3
      | (_, _, _ , INL e4, _, _, _, _, _) => INL e4
      | (_, _, _ , _, INL e5, _, _, _, _) => INL e5
      | (_, _, _ , _, _, INL e6, _, _, _) => INL e6
      | (_, _, _ , _, _, _, INL e7, _, _) => INL e7
      | (_, _, _ , _, _, _, _, INL e8, _) => INL e8
      | (_, _, _ , _, _, _, _, _, INL e9) => INL e9

end

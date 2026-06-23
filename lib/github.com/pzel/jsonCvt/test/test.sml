val decodeValueTests = [
  It "can succeed" (
  fn _=>
     let val jv = Json.fromString "{}"
         val p = JsonCvt.succeed "hello"
         val result = JsonCvt.decodeValue p jv
     in result == INR "hello"
     end)

 ,It "can fail" (
  fn _=>
     let val jv = Json.fromString "{}"
         val p = JsonCvt.fail "bad data"
         val result = JsonCvt.decodeValue p jv
     in result == INL "bad data"
     end)
]

val decodeStringTests = [
  It "can succeed" (
    fn _=>
       let val p = JsonCvt.succeed "hello"
           val result = JsonCvt.decodeString p "{}"
       in result == INR "hello"
       end)
 ,It "can fail" (
    fn _=>
       let val p = JsonCvt.fail "bad data"
           val result = JsonCvt.decodeString p "{}"
       in result == INL "bad data"
       end)
 ,It "fails with the underlying Json parse error if that fails" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val p = JsonCvt.succeed "hello"
           val result = JsonCvt.decodeString p "{notjson"
       in result == INL "Json: parser expecting '}'"
       end)
 ,It "field: decodes an object field" (
    fn _=>
       let open JsonCvt
           val op == = Assert.eq PolyML.makestring
           val input = "{\"name\":\"Tom\",\"age\":42}"
           val p = field "age" int
           val result = decodeString p input
       in result == INR 42
       end)
 ,It "field: returns INL when not an object" (
    fn _=>
       let open JsonCvt
           val op == = Assert.eq PolyML.makestring
           val input = "42"
           val p = field "age" int
           val result = decodeString p input
       in result == INL "No field 'age' in: 42"
       end)
 ,It "field: returns INL when field missing" (
    fn _=>
       let open JsonCvt
           val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":\"b\"}"
           val p = field "siblings" int
           val result = decodeString p input
       in result == INL "No field 'siblings' in: {\"a\":\"b\"}"
       end)
 ,It "field: returns INL when field parser fails" (
    fn _=>
       let open JsonCvt
           val op == = Assert.eq PolyML.makestring
           val input = "{\"name\":\"Tom\",\"age\":\"forty\"}"
           val p = field "age" int
           val result = decodeString p input
       in result == INL "Not a number: \"forty\""
       end)
 (* primitive parsers *)
 ,It "raw: passes through the raw Json.t" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":null}"
           val result = JsonCvt.decodeString JsonCvt.raw input
           val resultS = Either.mapRight Json.toString result
       in resultS == INR input
       end)
 ,It "null: successfully decodes NULL into the provided sml value" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":null}"
           val pNull = JsonCvt.null (1,2,3)
           val p = JsonCvt.field "a" pNull
           val result = JsonCvt.decodeString p input
       in result == INR (1,2,3)
       end)
 ,It "null: fails if NULL not encountered" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":1}"
           val pNull = JsonCvt.null (1,2,3)
           val p = JsonCvt.field "a" pNull
           val result = JsonCvt.decodeString p input
       in result == INL "Not null: 1"
       end)
 ,It "string: decodes JSON string" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":\"alphabet\"}"
           val p = JsonCvt.field "a" JsonCvt.string
           val result = JsonCvt.decodeString p input
       in result == INR "alphabet"
       end)
 ,It "string: fails on non-JSON string" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":1}"
           val p = JsonCvt.field "a" JsonCvt.string
           val result = JsonCvt.decodeString p input
       in result == INL "Not a string: 1"
       end)
 ,It "int: decodes small-enough numbers" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":2147483646}"
           val p = JsonCvt.field "a" JsonCvt.int
           val result = JsonCvt.decodeString p input
       in result == INR 2147483646
       end)
 ,It "int: fails on overflowing numbers" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"a\":21474836462147483646}"
           val p = JsonCvt.field "a" JsonCvt.int
           val result = JsonCvt.decodeString p input
       in result == INL "Can't convert to int: 21474836462147483646"
       end)
 ,It "bool: decodes JSON bools" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"t\":true}"
           val p = JsonCvt.field "t" JsonCvt.bool
           val result = JsonCvt.decodeString p input
       in result == INR true
       end)
 ,It "bool: fails on on non-JSON bools" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"t\":\"true\"}"
           val p = JsonCvt.field "t" JsonCvt.bool
           val result = JsonCvt.decodeString p input
       in result == INL "Not a bool: \"true\""
       end)
 ,It "real: decodes JSON floats" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"f\":3.14}"
           val p = JsonCvt.field "f" JsonCvt.real
           val result = JsonCvt.decodeString p input </ Either.mapRight Real.toString
       in result == INR "3.14"
       end)
 ,It "real: decodes JSON numbers to floats" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"f\":3}"
           val p = JsonCvt.field "f" JsonCvt.real
           val result = JsonCvt.decodeString p input </ Either.mapRight Real.toString
       in result == INR "3.0"
       end)
 ,It "real: fails to decode non-JSON numbers" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           val input = "{\"f\":[]}"
           val p = JsonCvt.field "f" JsonCvt.real
           val result = JsonCvt.decodeString p input </ Either.mapRight Real.toString
       in result == INL "Not a number: []"
       end)

]

(* list : Decoder a -> Decoder (List a)

Decode a JSON array into an Elm List.

decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
decodeString (list bool) "[true,false]" == Ok [True,False

*)

val complexParserTests = [
  It "parses lists" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[1,2,3]}"
           val p = field "f" (list int)
           val result = decodeString p input
       in result == INR [1,2,3]
       end)
 ,It "list parsing failure" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[1,2,3,false]}"
           val p = field "f" (list int)
           val result = decodeString p input
       in result == INL "Failed to parse list: [1, 2, 3, false]"
       end)

]


val composeTests = [
  It "can compose two field parsers" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 (field "a" int) (field "b" string)
               val result = JsonCvt.decodeString p input
           in result == INR (21,  "hello")
           end)
 ,It "can compose three field parsers" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\",\"v\":true}"
               val p = map3 (field "a" int) (field "b" string) (field "v" bool)
               val result = JsonCvt.decodeString p input
           in result == INR (21, "hello", true)
           end)
 ,It "can compose two field parsers: failure of the first" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 (field "a" string) (field "b" int)
               val result = JsonCvt.decodeString p input
           in result == INL "Not a string: 21"
           end)
 ,It "can compose two field parsers: failure of the second" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 (field "a" int) (field "b" int)
               val result = JsonCvt.decodeString p input
           in result == INL "Not a number: \"hello\""
           end)
  ,It "can map3~9" (
     fn _=> let val op == = Assert.eq PolyML.makestring
                open JsonCvt
                val input = "{\"a\":1,\"b\":2,\"c\":3,\"d\":4,\"e\":5,\"f\":6,\"g\":7,\"h\":8,\"i\":9}"
                fun p (fieldName) = field fieldName int
                val p3 = map3 (p "a") (p "b") (p "c")
                val p4 = map4 (p "a") (p "b") (p "c") (p "d")
                val p5 = map5 (p "a") (p "b") (p "c") (p "d") (p "e")
                val p6 = map6 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                val p7 = map7 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                              (p "g")
                val p8 = map8 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                              (p "g") (p "h")
                val p9 = map9 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                              (p "g") (p "h") (p "i")
                val doit = fn parser => decodeString parser input
                val _ = INR (1,2,3) =?= doit p3
                val _ = INR (1,2,3,4) =?= doit p4
                val _ = INR (1,2,3,4,5) =?= doit p5
                val _ = INR (1,2,3,4,5,6) =?= doit p6
                val _ = INR (1,2,3,4,5,6,7) =?= doit p7
                val _ = INR (1,2,3,4,5,6,7,8) =?= doit p8
                val _ = INR (1,2,3,4,5,6,7,8,9) =?= doit p9
            in Assert.succeed "all parsers worked"
            end)

]


val nestedTests = [
  It "can nest a parser" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":{\"c\":22,\"d\":23}}"
               val inner = map2 (field "c" int) (field "d" int)
               val p = map2 (field "a" int) (field "b" inner)
               val result = JsonCvt.decodeString p input
           in result == INR (21, (22, 23))
           end)
]

val allTests = decodeValueTests
               @ decodeStringTests
               @ complexParserTests
               @ composeTests
               @ nestedTests

fun main () =
    runTestsWith \> allTests \> CommandLine.arguments()

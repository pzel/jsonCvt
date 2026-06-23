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

val complexParserTests = [
  It "parses nullables successfully" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":1}"
           val p = field "f" (nullable int)
           val result = decodeString p input
       in result == INR (SOME 1)
       end)
  ,It "parses nullables successfully (null)" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":null}"
           val p = field "f" (nullable int)
           val result = decodeString p input
       in result == INR NONE
       end)
  ,It "failes parsing nullables" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":false}"
           val p = field "f" (nullable int)
           val result = decodeString p input
       in result == INL "Not a number: false"
       end)
 ,It "parses lists" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[1,2,3]}"
           val p = field "f" (list int)
           val result = decodeString p input
       in result == INR [1,2,3]
       end)
 ,It "list parsing: inner failure" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[1,2,3,false]}"
           val p = field "f" (list int)
           val result = decodeString p input
       in result == INL "Failed to parse list: [1, 2, 3, false]"
       end)
 ,It "list parsing: outer failure" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":false}"
           val p = field "f" (list int)
           val result = decodeString p input
       in result == INL "Not a list: false"
       end)
 ,It "traverses paths with 'at'" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":{\"g\":{\"h\":1,\"hh\":2},\"gg\":3}}"
           val p = at ["f", "g", "h"] int
           val result = decodeString p input
       in result == INR 1
    end)
 ,It "at: reports which field was wrong" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":{\"g\":{\"h\":1,\"hh\":2},\"gg\":3}}"
           val p = at ["f", "g", "x"] int
           val result = decodeString p input
       in result == INL "No field 'x' in: {\"h\":1, \"hh\":2}"
    end)
 ,It "index returns item at index" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[10,11,12]}"
           val p = field "f" (index 0 int)
           val result = decodeString p input
       in result == INR 10
    end)
 ,It "index returns oob at overly large index" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":[10,11,12]}"
           val p = field "f" (index 10 int)
           val result = decodeString p input
       in result == INL "Index out of bounds: idx=10 len=3"
    end)
 ,It "index unindexable value" (
    fn _=>
       let val op == = Assert.eq PolyML.makestring
           open JsonCvt
           val input = "{\"f\":111111}"
           val p = field "f" (index 10 int)
           val result = decodeString p input
       in result == INL "Not indexable: 111111"
    end)
]


fun tup2 a b = (a,b)
fun tup3 a b c = (a,b,c)
fun tup4 a b c d = (a,b,c,d)
fun tup5 a b c d e = (a,b,c,d,e)
fun tup6 a b c d e f = (a,b,c,d,e,f)
fun tup7 a b c d e f g = (a,b,c,d,e,f,g)
fun tup8 a b c d e f g h = (a,b,c,d,e,f,g,h)
fun tup9 a b c d e f g h i = (a,b,c,d,e,f,g,h,i)


val mapTests = [
  It "can map a fun over a parser" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21}"
               val p = map (fn i => i * 10) (field "a" int)
               val result = JsonCvt.decodeString p input
           in result == INR 210
           end)
 ,It "can map a fun over a parser: failure" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21}"
               val p = map String.size (field "a" string)
               val result = JsonCvt.decodeString p input
           in result == INL "Not a string: 21"
           end)
 ,It "can compose two field parsers" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 tup2 (field "a" int) (field "b" string)
               val result = JsonCvt.decodeString p input
           in result == INR (21,  "hello")
           end)
 ,It "can compose three field parsers" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\",\"v\":true}"
               val p = map3 tup3 (field "a" int) (field "b" string) (field "v" bool)
               val result = JsonCvt.decodeString p input
           in result == INR (21, "hello", true)
           end)
 ,It "can compose two field parsers: failure of the first" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 tup2 (field "a" string) (field "b" int)
               val result = JsonCvt.decodeString p input
           in result == INL "Not a string: 21"
           end)
 ,It "can compose two field parsers: failure of the second" (
    fn _=> let val op == = Assert.eq PolyML.makestring
               open JsonCvt
               val input = "{\"a\":21,\"b\":\"hello\"}"
               val p = map2 tup2 (field "a" int) (field "b" int)
               val result = JsonCvt.decodeString p input
           in result == INL "Not a number: \"hello\""
           end)
  ,It "can map3 3~9" (
     fn _=> let val op == = Assert.eq PolyML.makestring
                open JsonCvt
                val input = "{\"a\":1,\"b\":2,\"c\":3,\"d\":4,\"e\":5,\"f\":6,\"g\":7,\"h\":8,\"i\":9}"
                fun p (fieldName) = field fieldName int
                val p3 = map3 tup3 (p "a") (p "b") (p "c")
                val p4 = map4 tup4 (p "a") (p "b") (p "c") (p "d")
                val p5 = map5 tup5 (p "a") (p "b") (p "c") (p "d") (p "e")
                val p6 = map6 tup6 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                val p7 = map7 tup7 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                              (p "g")
                val p8 = map8 tup8 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
                              (p "g") (p "h")
                val p9 = map9 tup9 (p "a") (p "b") (p "c") (p "d") (p "e") (p "f")
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
               val inner = map2 tup2 (field "c" int) (field "d" int)
               val p = map2 tup2 (field "a" int) (field "b" inner)
               val result = JsonCvt.decodeString p input
           in result == INR (21, (22, 23))
           end)
]

val allTests = decodeValueTests
               @ decodeStringTests
               @ complexParserTests
               @ mapTests
               @ nestedTests

fun main () =
    runTestsWith \> allTests \> CommandLine.arguments()

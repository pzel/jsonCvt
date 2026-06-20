val decodeValueTests = [
  It "can succeed" (
  fn _=>
     let val jv = Json.fromString "{}"
         val p = JsonCvt.succeed "hello"
         val result = JsonCvt.decodeValue p jv
     in result == INR "hello"
     end),

  It "can fail" (
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
]


val allTests = decodeValueTests @ decodeStringTests


fun main () =
    CommandLine.arguments() >| runTestsWith allTests

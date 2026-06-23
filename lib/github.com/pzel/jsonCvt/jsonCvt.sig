signature JSON_CVT =
sig

  type 'a decoder = Json.t -> (string,'a) either
  type cvtError = string
  type 'a result = (cvtError, 'a) either


  val decodeValue : 'a decoder -> Json.t -> 'a result
  val decodeString : 'a decoder -> string -> 'a result

  val fail : string -> 'a decoder
  val succeed : 'a -> 'a decoder
  val raw : Json.t decoder

  val null : 'a -> 'a decoder
  val int : int decoder
  val real : real decoder
  val bool : bool decoder
  val string : string decoder

  val field : string -> 'a decoder -> 'a decoder
  val list : 'a decoder -> 'a list decoder
  val nullable : 'a decoder -> 'a option decoder

  val at : string list -> 'a decoder -> 'a decoder
  val index : int -> 'a decoder -> 'a decoder

  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  val map2 : ('a -> 'b -> 'c) -> 'a decoder -> 'b decoder
             -> 'c decoder
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a decoder -> 'b decoder -> 'c decoder
             -> 'd decoder
  val map4: ('a -> 'b -> 'c -> 'd -> 'e)
            -> 'a decoder -> 'b decoder -> 'c decoder -> 'd decoder
            -> 'e decoder
  val map5: ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
            'a decoder -> 'b decoder -> 'c decoder ->
            'd decoder -> 'e decoder
            -> 'f decoder
  val map6: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
           'a decoder -> 'b decoder -> 'c decoder ->
           'd decoder -> 'e decoder -> 'f decoder
           -> 'g decoder

  val map7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
           'a decoder -> 'b decoder -> 'c decoder -> 'd decoder ->
           'e decoder -> 'f decoder -> 'g decoder
            -> 'h decoder

  val map8 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) ->
           'a decoder -> 'b decoder -> 'c decoder -> 'd decoder ->
           'e decoder -> 'f decoder -> 'g decoder -> 'h decoder
           -> 'i decoder

  val map9: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j) ->
           'a decoder -> 'b decoder -> 'c decoder -> 'd decoder ->
           'e decoder -> 'f decoder -> 'g decoder -> 'h decoder
           -> 'i decoder
           -> 'j decoder

end

open Result

type error =
  | InvalidCharacters

module Group : sig
  type t 
  val of_string : string option -> (t, error) result
  val to_string : t -> string option
end = struct
  
  let allowed_chars = Re.str "[0-9a-zA-Z\\-]+" |> Re.compile
  
  type t = string option
  
  let of_string = function
    | None -> Ok None
    | Some s -> (match Re.execp allowed_chars s with
      | true -> Ok (Some s)
      | false -> Error InvalidCharacters)
  
  let to_string g = g

end

module Name : sig
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
  val of_string : string -> (t, error) result
  val to_string : t -> string
end = struct
  
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
  
  let of_string t = 
    Ok (match String.uppercase_ascii t with
    | "SOURCE" -> SOURCE | "KIND" -> KIND | "FN" -> FN | "N" -> N | "NICKNAME" -> NICKNAME
    | "PHOTO" -> PHOTO | "BDAY" -> BDAY | "ANNIVERSARY" -> ANNIVERSARY | "GENDER" -> GENDER
    | "ADR" -> ADR | "TEL" -> TEL | "EMAIL" -> EMAIL | "IMPP" -> IMPP | "LANG" -> LANG
    | "TZ" -> TZ | "GEO" -> GEO | "TITLE" -> TITLE | "ROLE" -> ROLE | "LOGO" -> LOGO 
    | "ORG" -> ORG | "MEMBER" -> MEMBER | "RELATED" -> RELATED | "CATEGORIES" -> CATEGORIES
    | "NOTE" -> NOTE | "PRODID" -> PRODID | "REV" -> REV | "SOUND" -> SOUND | "UID" -> UID
    | "CLIENTPIDMAP" -> CLIENTPIDMAP | "URL" -> URL | "KEY" -> KEY | "FBURL" -> FBURL 
    | "CALADRURI" -> CALADRURI | "CALURI" -> CALURI | "XML" -> XML | "BIRTHPLACE" -> BIRTHPLACE
    | "DEATHPLACE" -> DEATHPLACE | "DEATHDATE" -> DEATHDATE | "EXPERTISE" -> EXPERTISE
    | "HOBBY" -> HOBBY | "INTEREST" -> INTEREST | "ORG_DIRECTORY" -> ORG_DIRECTORY
    | name -> X_NAME(name))

  let to_string = function
    |SOURCE -> "SOURCE" | KIND -> "KIND" | FN -> "FN" | N -> "N" | NICKNAME -> "NICKNAME" 
    | PHOTO -> "PHOTO" | BDAY -> "BDAY" | ANNIVERSARY -> "ANNIVERSARY" | GENDER -> "GENDER" 
    | ADR  -> "ADR" | TEL -> "TEL" | EMAIL -> "EMAIL" | IMPP -> "IMPP" | LANG -> "LANG" 
    | TZ -> "TZ" | GEO -> "GEO" | TITLE -> "TITLE" | ROLE -> "ROLE" | LOGO -> "LOGO" | ORG -> "ORG" 
    | MEMBER -> "MEMBER" | RELATED -> "RELATED" | CATEGORIES -> "CATEGORIES" | NOTE -> "NOTE"
    | PRODID -> "PRODID" | REV -> "REV" | SOUND -> "SOUND" | UID -> "UID" 
    | CLIENTPIDMAP -> "CLIENTPIDMAP" | URL -> "URL" | KEY -> "KEY" | FBURL -> "FBURL" 
    | CALADRURI -> "CALADRURI" | CALURI -> "CALURI" | XML -> "XML" | BIRTHPLACE -> "BIRTHPLACE"
    | DEATHPLACE -> "DEATHPLACE" | DEATHDATE -> "DEATHDATE" | EXPERTISE -> "EXPERTISE" 
    | HOBBY -> "HOBBY" | INTEREST -> "INTEREST" | ORG_DIRECTORY -> "ORG_DIRECTORY" | X_NAME xname -> xname
  
end

module Parameter : sig
  type t = { 
    name : string;
    values : string list;
  }
  val of_parsed : Syntax.parameter -> (t, error) result
end = struct
  
  type t = { 
    name : string;
    values : string list;
  }
  
  let of_parsed (parameter : Syntax.parameter ) = Ok {
    name = parameter.Syntax.name;
    values = parameter.Syntax.values;
  }
  
end

module Value : sig
  type t
  val of_string : string -> (t, error) result
  val to_string : t -> string
end = struct
  
  type t = string
  
  let of_string t = Ok t
  
  let to_string t = t 
  
end

module M :sig
    val all : ('a, 'b) result list -> ('a list, 'b) result
    val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
end = struct
  let bind_result e f =
      match e with
      | Error _ as err -> err
      | Ok x -> f x
  let (>>=) = bind_result
  let all results =
    let rec _all acc xs =
      match xs with
      | [] -> Ok (List.rev acc)
      | Error _ as err :: _ -> err
      | Ok x::rest -> _all (x::acc) rest
    in
    _all [] results
end

module Content_line = struct
  type t = {
    group : Group.t;
    name : Name.t;
    parameters : Parameter.t list;
    value : Value.t;
  }

  let of_parsed parsed = 
    let (>>=) = M.(>>=) in
    Group.of_string parsed.Syntax.group >>= fun group ->
    Name.of_string parsed.Syntax.name >>= fun name ->
      
    parsed.Syntax.parameters |> List.map Parameter.of_parsed |> M.all >>= fun parameters -> 
      
    Value.of_string parsed.Syntax.value >>= fun value ->      
    Ok {group = group; name = name; parameters = parameters; value = value;}

end

type t = {
  content_lines : Content_line.t list;
}

let of_parsed parsed = 
  let (>>=) = M.(>>=) in
  parsed.Syntax.content_lines |> List.map Content_line.of_parsed |> M.all >>= fun cls ->
  Ok { content_lines = cls; }
  

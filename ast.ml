type expr = 
    | Comment of string
    | Asymbol of string
    | Adigit of int
    | Cinst of (string option * string * string option)
    | Label of string

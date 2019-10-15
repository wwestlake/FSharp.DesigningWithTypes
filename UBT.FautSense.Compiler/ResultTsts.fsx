

let test a b =
    match a = b with
    | true -> Ok "a = b"
    | false -> Error "A != B"


let check = test 1 2


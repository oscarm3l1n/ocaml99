let rec last (lst: 'a list) =
    match lst with
    | [] -> None
    | [x] -> Some x
    | hd :: tl -> last tl

let rec last_two = function
    | [] -> None
    | [x] -> None
    | fst :: snd :: [] -> Some (fst, snd)
    | hd :: tl -> last_two tl

let rec at i = function
    | [] -> failwith "Nth"
    | hd :: tl ->
        if i = 0 then Some hd
        else at (i-1) tl

let length lst =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (acc+1) tl
    in
    aux 0 lst

let rev lst =
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (hd :: acc) tl
    in
    aux [] lst

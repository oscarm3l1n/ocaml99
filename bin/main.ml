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

let is_palindrome lst =
    List.rev lst = lst

type 'a node =
    | One of 'a
    | Many of 'a node list

let flatten lst =
    let rec aux acc = function
        | [] -> acc
        | One x :: t -> aux (x :: acc) t
        | Many l :: t -> aux (aux acc l) t
    in
    aux [] lst

let compress lst =
    let rec aux acc = function
        | [] -> acc
        | hd :: (snd :: _ as tl) ->
            if hd = snd then
                aux acc tl
            else
                aux (hd :: acc) tl
        | h :: t -> aux (h :: acc) t
    in

    List.rev (aux [] lst)

let rec compress' = function
    | a :: (b :: _ as t) -> if a = b then compress' t else a :: compress' t
    | smaller -> smaller

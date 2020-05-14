module Practice3

let mutable dataSet = [2; 1; 4; 3]
let index = 0;

let addToList lst element = lst @ [element]

let removeFirstElement (lst: byref<'T>) = 
    lst <- match lst with
            | [] -> []
            | h::t -> t

let putElementsToBack (lst: byref<'T>) amount =
    for i in 1..amount do
        lst <- match lst with
                    | [] -> []
                    | h::t -> addToList t h

let output = 
    let amount = ref 1;
    let mutable sorted = [];
    let length = dataSet.Length;

    while sorted.Length < length do
        putElementsToBack &dataSet amount.Value;
        sorted <- addToList sorted dataSet.[0]
        removeFirstElement &dataSet

        incr amount
    
    sorted
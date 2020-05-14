module Practice2

    //iterate in an array of arrays
    module Array =
        let iteri2 f = Array.iteri (fun i -> Array.iteri (f i))

    //input
    let dataSet = 
        [|[|2; 4; 3|];[|1; 0; 9|];[|9; 6; 8|]|]

    let innerRank = ref 1;    
    let succesCounter = ref 0;
    let uniquenessParam = 10;

    //checks the rank by making combinations with the previous and next value
    let rankCheck previousVal currentVal nextVal = 
        let minusCheck amp = previousVal - amp;
        let plusCheck amp = previousVal + amp;
        let timesCheck amp = previousVal * amp;
        let divisionCheck amp = 
            if previousVal > 0 && amp > 0 then
                (previousVal |> single) / (currentVal |> single)
            else -1.0f;

        //if next val is not -1 check with next val
        if nextVal <> -1 then
            match nextVal with
            | x when minusCheck currentVal = x -> true
            | x when plusCheck currentVal = x  -> true
            | x when timesCheck currentVal = x -> true
            | x when divisionCheck currentVal = (x |> single) -> true
            | _ -> false

        //else check if you can make combinations with the previous val and number
        else
(*            for i in 1 .. uniquenessParam do
                match currentVal with
                | x when minusCheck i = x -> true
                | x when plusCheck i = x -> true
                | x when timesCheck i = x -> true
                | x when divisionCheck i = (x |> single) -> true
                |> ignore*)
            false

    type matchValues(previousIn, nextIn) = 
        member val previous = previousIn with get
        member val next = nextIn with get

    //get the previous and next values if possible
    let getMatchValues r c : matchValues =
        if r > 1 then
            matchValues(dataSet.[r-2].[c], dataSet.[r-1].[c])
        else
            matchValues(dataSet.[r-1].[c], -1)

    //get the rank of the dataset matrix
    let rank = 
        dataSet
        |> Array.iteri2 (fun r c n -> 
            if r > 0 && r < dataSet.Length then
                let currentMatch = getMatchValues r c;
                if rankCheck currentMatch.previous n currentMatch.next = false then
                   incr succesCounter
                   if succesCounter.Value >= 3 then incr innerRank; succesCounter := 0
                else succesCounter := 0)
        innerRank.Value

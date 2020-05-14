module Practice1
    let dataSet = 
        [|4, 5; 5, 3; 6, 5; 7, 6; 8, 2;|] 
        |> Seq.map (fun (a, b) -> (a - 1) * (10 - b));
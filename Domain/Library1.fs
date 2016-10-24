namespace Domain

module Model =
    type Dependency<'a> = { From: 'a; To: 'a list }

    let print all = 
        let rec sprintOne ltr =
            let sprintChildren = sprintfn "parent %s has children %s" ltr

    [  
        { From = 'B'; To = [ 'C'; 'E' ] };
        { From = 'A'; To = [ 'B'; 'C' ] };
        { From = 'C'; To = [ 'G' ] };
        { From = 'D'; To = [ 'A'; 'F' ] };
        { From = 'E'; To = [ 'F' ] };  
        { From = 'F'; To = [ 'H' ] };
    ]
    |> calculate
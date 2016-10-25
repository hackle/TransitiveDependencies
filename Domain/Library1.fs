namespace Domain

module Model =
    type Dependency<'a> = { From: 'a; To: 'a list }

    let findDependencies all = 
        let rec findForOne carry key =
            let found = all |> List.tryFind (fun d -> d.From = key)
            match found with
            | None -> carry
            | Some f -> f.To |> List.fold findForOne (f.To @ carry)

        all |> List.map (fun d -> { From = d.From; To = d.From |> findForOne [] })

    let findWithFoldBack all =
        let rec findForOne (key: 'a) (cont: 'a list -> 'a list) =
            let found = all |> List.tryFind (fun d -> d.From = key)
            match found with
            | None -> cont []
            | Some f ->             
                let folder racc cur =
                    findForOne cur (fun rcur -> rcur @ racc)
                f.To |> List.fold folder f.To

        all |> List.map (fun d -> { 
                                    From = d.From; 
                                    To = findForOne d.From id 
                                            |> List.distinct 
                                            |> List.sort
                                    })

    [  
        { From = 'B'; To = [ 'C'; 'E' ] };
        { From = 'A'; To = [ 'B'; 'C' ] };
        { From = 'C'; To = [ 'G' ] };
        { From = 'D'; To = [ 'A'; 'F' ] };
        { From = 'E'; To = [ 'F' ] };  
        { From = 'F'; To = [ 'H' ] };
    ]
    |> findWithFoldBack
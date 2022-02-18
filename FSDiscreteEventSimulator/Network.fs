
namespace FSDiscreteEventSimulator

module Network =

    open Common


    type DESNetwork<'TUser when 'TUser :> IUser> =
        DESNetwork of IDESComponentInstantiator<'TUser> list

    type DESBuiltNetwork<'TUser when 'TUser :> IUser> =
        private DESFinalisedNetwork of IDESComponentInstantiator<'TUser> list

    let empty =
        DESNetwork []

    let addInstantiator instantiator (DESNetwork network) =
        if network |> List.contains instantiator then
            failwith <| sprintf "Cannot add the same instantiator ('%s') more than once." instantiator.Name
        else
            DESNetwork (instantiator :: network)

    let finalise (DESNetwork network) =
        let rec inner (ordered: IDESComponentInstantiator<_> list) newOrdered tried unordered =
            match newOrdered, tried, unordered with
            | _, _, u::us ->
                match (u: IDESComponentInstantiator<_>) with
                | :? IDESUserSenderInstantiator<_> as u' ->
                    if ordered |> List.contains u'.Target then
                        inner ordered (u::newOrdered) tried us
                    else
                        inner ordered newOrdered (u::tried) us
                | _ ->
                    inner ordered (u::newOrdered) tried us
            | [], [], [] ->
                 ordered |> List.rev
            | [], _, [] ->
                failwith "Cycle detected."
            | _, _, [] ->                
                inner (newOrdered @ ordered) [] [] tried

        DESFinalisedNetwork <| inner [] [] [] network

    let simulate (DESFinalisedNetwork network) =
        let components =
            network
            |> List.map (fun c -> c.Create())

        let instantiatorMapping =
            components
            |> Seq.zip network
            |> Seq.toDictionary

        let rec inner (ctx: SimulationContext) =
            let nextEvents =
                components
                |> Seq.collect (fun c ->
                    c.NextEvents() |> Seq.map (function
                        | { Timestamp = Immediate; Request = request } ->
                            { Timestamp = ctx.Timestamp; Requestor = c; Request = request }
                        | { Timestamp = Relative o; Request = request } when o >= 0.0 ->
                            { Timestamp = ctx.Timestamp + o; Requestor = c; Request = request }
                        | { Timestamp = Absolute t; Request = request } when t >= ctx.Timestamp ->
                            { Timestamp = t; Requestor = c; Request = request }
                        | { Timestamp = ts; } ->
                            failwith <| sprintf "Unable to process event raised by '%s' with timestamp '%A'."
                                c.Name ts))
                |> Seq.toList
                            
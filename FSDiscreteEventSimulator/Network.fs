
namespace FSDiscreteEventSimulator

module Network =

    open System.Collections.Immutable
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

    let private processUserCreate
            (requestor: IDESComponent<_>)
            newUser
            (ctx: SimulationContext<_>) =

        let usersForComponent =
            ctx.Users[requestor]     

        if usersForComponent.Contains newUser then
            failwith <| sprintf "Unable to create user '%s' as already exists in '%s'."
                newUser.Id requestor.Name

        let newUsersForComponent =
            usersForComponent.Add(newUser)

        let (requestor': IDESUserCreator<_>) =
            downcast requestor   

        requestor'.HasCreated (ctx.Timestamp, newUser)

        Successful { ctx with Users = ctx.Users.SetItem(requestor, newUsersForComponent) }

    let private processUserMoveWithMap
            (instantiatorMapping: ImmutableDictionary<IDESComponentInstantiator<'TUser>, IDESComponent<'TUser>>)
            (requestor: IDESComponent<'TUser>)
            (userBefore: 'TUser)
            (userAfter: 'TUser)
            (targetInst: IDESUserRecieverInstantiator<'TUser>)
            (ctx: SimulationContext<'TUser>) =

        let usersForComponent =
            ctx.Users[requestor]

        if not <| usersForComponent.Contains(userBefore) then
            failwith <| sprintf "Cannot move user '%s' from '%s' to '%s' as not allocated to component."
                userBefore.Id requestor.Name targetInst.Name

        if not <| instantiatorMapping.ContainsKey(targetInst) then
            failwith <| sprintf "Target '%s' does not exist in network." targetInst.Name

        let target =
            instantiatorMapping[targetInst]

        let targetUsers =
            ctx.Users[target]

        if targetUsers.Contains(userAfter) then
            failwith <| sprintf "Cannot move user '%s' from '%s' to '%s' as already exists."
                userAfter.Id requestor.Name target.Name

        let newSourceUsers =
            usersForComponent.Remove(userBefore)

        let (requestor': IDESUserSender<_>) =
            downcast requestor

        requestor'.HasSent (ctx.Timestamp, userBefore)

        let ctx =
            { ctx with Users = ctx.Users.SetItem(requestor, newSourceUsers) }
            
        let (target': IDESUserReciever<_>) =
            downcast target

        if target'.CanRecieve (ctx.Timestamp, userAfter) then

            let newTargetUsers =
                targetUsers.Add(userAfter)

            Successful { ctx with Users = ctx.Users.SetItem(target, newTargetUsers) }

        else
            UserDropped ctx
        
    let private processUserDestroy
            (requestor: IDESComponent<'TUser>)
            (user: 'TUser)
            (ctx: SimulationContext<'TUser>) =      

        let usersForComponent =
            ctx.Users[requestor]

        if not <| usersForComponent.Contains user then
            failwith <| sprintf "Unable to destroy user '%s' as not present in '%s'."
                user.Id requestor.Name

        let newUsersForComponent =
            usersForComponent.Remove(user)

        let (requestor': IDESUserDestroyer<_>) =
            downcast requestor  

        requestor'.HasDestroyed (ctx.Timestamp, user)

        Successful { ctx with Users = ctx.Users.SetItem(requestor, newUsersForComponent) }

    let private processEvent
            (instantiatorMapping: ImmutableDictionary<IDESComponentInstantiator<'TUser>, IDESComponent<'TUser>>)
            (ctx: SimulationContext<'TUser>)
            { Requestor = requestor; Request = request } =

        match request with
        | CreateUser newUser ->
            processUserCreate requestor newUser ctx

        | MoveUser (user, targetInst) ->
            processUserMoveWithMap instantiatorMapping requestor user user targetInst ctx

        | MoveUserWithMap (userBefore, userAfter, targetInst) ->
            processUserMoveWithMap instantiatorMapping requestor userBefore userAfter targetInst ctx

        | DestroyUser user ->
            processUserDestroy requestor user ctx

    let simulate (DESFinalisedNetwork network) =
        let components =
            network
            |> List.map (fun c -> c.Create())

        let instantiatorMapping =
            components
            |> Seq.zip network
            |> Seq.toImmutableDictionary

        let startingContext =
            { Timestamp = 0.0
              Users =
                components
                |> Seq.map (fun u -> u, ImmutableHashSet.Empty)
                |> Seq.toImmutableDictionary }                        

        let rec inner ctx =
            let nextEvents =
                components
                |> Seq.collect (fun c ->
                    let usersForComponent = ctx.Users[c]

                    c.NextEvents (ctx.Timestamp, usersForComponent) |> Seq.map (function
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

            if nextEvents.IsEmpty then
                Seq.empty

            else
                let nextTimestamp =
                    nextEvents
                    |> Seq.map (fun { Timestamp = ts } -> ts)
                    |> Seq.min

                let nextEventsToAction =
                    nextEvents
                    |> Seq.filter (fun { Timestamp = ts } -> ts = nextTimestamp)

                let mutable ctx =
                    { ctx with Timestamp = nextTimestamp }

                seq {
                    for e in nextEventsToAction do
                        let ctx' =
                            processEvent instantiatorMapping ctx e

                        ctx <-
                            match ctx' with
                            | Successful ctx'' | UserDropped ctx'' ->
                                ctx''

                        let e' =
                            match ctx' with
                            | Successful _ -> Successful e
                            | UserDropped _ -> UserDropped e

                        yield e'

                    yield! inner ctx
                }

        inner startingContext    
                            

namespace FSDiscreteEventSimulator

module Network =

    open System.Collections.Generic
    open System.Collections.Immutable
    open Common


    type DESNetwork<'TUser when 'TUser :> IUser> =
        DESNetwork of IDESComponentInstantiator<'TUser> list

    type DESBuiltNetwork<'TUser when 'TUser :> IUser> =
        private DESFinalisedNetwork of IDESComponentInstantiator<'TUser> list

    type private DESInstantiatorDetails<'TUser when 'TUser :> IUser> =
        { Instantiator: IDESComponentInstantiator<'TUser>
          Instance: IDESComponent<'TUser>
          Users: HashSet<'TUser> }


    let empty =
        DESNetwork []


    let addInstantiator instantiator (DESNetwork network) =
        if network |> List.contains instantiator then
            failwith <| sprintf "Cannot add the same instantiator ('%s') more than once." instantiator.Name
        else
            DESNetwork (instantiator :: network)


    // This carries out a topological sort of the network. Sink nodes are effectively processed first with
    // all others being processed in a sensible order afterwards.
    let finalise (DESNetwork network) =
        let rec inner (ordered: IDESComponentInstantiator<_> list) newOrdered tried unordered =
            match newOrdered, tried, unordered with
            | _, _, u::us ->
                match (u: IDESComponentInstantiator<_>) with
                | :? IDESUserSenderInstantiator<_> as u' ->
                    u'.Targets
                    // Check to see if all of the targets are within the supplied list of instantiators.
                    |> Seq.tryFind (fun t -> not (network |> List.contains t))
                    |> function
                        | Some t ->
                            failwith <| sprintf "Component '%s' has target '%s' which does not exist in network."
                                u.Name t.Name
                        | None ->
                            if u'.Targets |> Seq.forall (fun t -> ordered |> List.contains t) then
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
            (instantiatorDetailMappings: ImmutableDictionary<IDESComponentInstantiator<'TUser>, DESInstantiatorDetails<'TUser>>)
            (requestor: IDESComponentInstantiator<'TUser>)
            (newUser: 'TUser)
            timestamp = 

        let requestorDetails =
            instantiatorDetailMappings[requestor]

        if not <| requestorDetails.Users.Add(newUser) then
            failwith <| sprintf "Unable to create user '%s' as already exists in '%s'."
                newUser.Id requestor.Name

        let (requestor': IDESUserCreator<_>) =
            downcast requestorDetails.Instance   

        requestor'.HasCreated (timestamp, newUser)

        Successful


    let private processUserMoveWithMap
            (instantiatorDetailMappings: ImmutableDictionary<IDESComponentInstantiator<'TUser>, DESInstantiatorDetails<'TUser>>)
            (requestor: IDESComponentInstantiator<'TUser>)
            (userBefore: 'TUser)
            (userAfter: 'TUser)
            (targetInst: IDESUserRecieverInstantiator<'TUser>)
            timestamp =

        let requestorDetails =
            instantiatorDetailMappings[requestor]

        if not <| instantiatorDetailMappings.ContainsKey(targetInst) then
            failwith <| sprintf "Target '%s' does not exist in network." targetInst.Name

        if not <| requestorDetails.Users.Remove(userBefore) then
            failwith <| sprintf "Cannot move user '%s' from '%s' to '%s' as not allocated to component."
                userBefore.Id requestor.Name targetInst.Name

        let (requestor': IDESUserSender<_>) =
            downcast requestorDetails.Instance

        requestor'.HasSent (timestamp, userBefore)        

        let targetDetails =
            instantiatorDetailMappings[targetInst]
          
        let (target': IDESUserReciever<_>) =
            downcast targetDetails.Instance

        if target'.CanRecieve (timestamp, userAfter) then

            if not <| targetDetails.Users.Add(userAfter) then
                failwith <| sprintf "Cannot move user '%s' from '%s' to '%s' as already exists."
                    userAfter.Id requestor.Name targetInst.Name

            target'.HasRecieved (timestamp, userAfter)

            Successful

        else
            UserDropped

        
    let private processUserDestroy
            (instantiatorDetailMappings: ImmutableDictionary<IDESComponentInstantiator<'TUser>, DESInstantiatorDetails<'TUser>>)
            (requestor: IDESComponentInstantiator<'TUser>)
            (user: 'TUser)
            timestamp =
            
        let requestorDetails =
            instantiatorDetailMappings[requestor]

        if not <| requestorDetails.Users.Remove (user) then
            failwith <| sprintf "Unable to destroy user '%s' as not present in '%s'."
                user.Id requestor.Name

        let (requestor': IDESUserDestroyer<_>) =
            downcast requestorDetails.Instance  

        requestor'.HasDestroyed (timestamp, user)

        Successful


    let private processEvent instantiatorDetailMappings timestamp { Requestor = requestor; User = user; Type = reqType } =
        match reqType with
        | CreateUser ->
            processUserCreate instantiatorDetailMappings requestor user timestamp

        | MoveUser targetInst ->
            processUserMoveWithMap instantiatorDetailMappings requestor user user targetInst timestamp

        | MoveUserWithMap (userAfter, targetInst) ->
            processUserMoveWithMap instantiatorDetailMappings requestor user userAfter targetInst timestamp

        | DestroyUser ->
            processUserDestroy instantiatorDetailMappings requestor user timestamp


    // Creates a mapping between the original instantiators and the resulting instances.
    // This also injects proxy targets into those instances as required.
    // Note that proxy targets are used so that one component instance in a network does not
    // have direct sight of another instance.
    let private buildInstanceMapping randomizer network =
        let rec inner (instantiatorMappings: ImmutableDictionary<_,_>) (proxyMappings: ImmutableDictionary<_,_>) =
            function
            | i::is ->
                // This will (eventually) contain the users assigned to this component.
                let userSet = new HashSet<_>()

                // Create a new instance for our instantiator.
                let newInstance =
                    match (i: IDESComponentInstantiator<'TUser>) with
                    // Check to see if we have a sender instantiator. If so, we need
                    // to track the targets so that we can get the corresponding target proxies.
                    | :? IDESUserSenderInstantiator<'TUser> as sender ->
                        let targetProxies =
                            sender.Targets
                            |> List.map (fun t -> proxyMappings[t])

                        i.Create(randomizer, userSet :> IReadOnlySet<_>, targetProxies)

                    // Otherwise, create without any target proxies specified.
                    | _ ->
                        i.Create(randomizer, userSet :> IReadOnlySet<_>, [])

                let newInstantiatorDetails =
                    { Instantiator = i; Instance = newInstance; Users = userSet }

                let newInstantiatorMappings =
                    instantiatorMappings.Add(i, newInstantiatorDetails)

                let newProxyMappings =
                    match i, newInstance with
                    // Check to see if we have a reciever instantiator and corresponding reciever instance.
                    // If so, we need to create a target proxy for the new instance and track for future use.
                    | (:? IDESUserRecieverInstantiator<'TUser> as rI), (:? IDESUserReciever<'TUser> as r) ->
                        let newProxy =
                            { new IDESTargetProxy<_> with
                                member this.UserList = userSet :> IReadOnlySet<_>
                                member _.CanRecieve (timestamp, user) = r.CanRecieve (timestamp, user)
                                member _.Instantiator = rI }

                        proxyMappings.Add(rI, newProxy)

                    | (:? IDESUserRecieverInstantiator<'TUser>), _ ->
                        failwith <| sprintf "Instantiator '%s' marked as a reciever but instance is a non-reciever?"
                            i.Name

                    | _ -> proxyMappings

                inner newInstantiatorMappings newProxyMappings is

            | [] -> instantiatorMappings

        inner ImmutableDictionary.Empty ImmutableDictionary.Empty network


    // This is called each time a simulation of a finalised network is required.
    let simulate randomizer (DESFinalisedNetwork network) =
        let instantiatorDetailMappings =
            buildInstanceMapping randomizer network
            
        let instantiatorDetails =
            instantiatorDetailMappings.Values
            |> List.ofSeq

        let rec inner timestamp =
            // Build a complete list of events from all components in the network.
            // TODO - This could be improved to short-circuit as soon as an Immediate request is posted.
            let nextEvents =
                instantiatorDetails
                |> Seq.collect (fun { Instantiator = i; Instance = c } ->
                    // Here we need to convert the local requests into complete requests that include
                    // details of the requestor (instance).
                    c.NextEvents (timestamp) |> Seq.map (function
                        | { Timestamp = Immediate; User = user; Type = reqType } ->
                            { Timestamp = timestamp; Requestor = i; User = user; Type = reqType }
                        | { Timestamp = Relative o; User = user; Type = reqType } when o >= 0.0 ->
                            { Timestamp = timestamp + o; Requestor = i; User = user; Type = reqType }
                        | { Timestamp = Absolute t; User = user; Type = reqType } when t >= timestamp ->
                            { Timestamp = t; Requestor = i; User = user; Type = reqType }
                        | { Timestamp = ts; } ->
                            failwith <| sprintf "Unable to process event raised by '%s' with timestamp '%A'."
                                c.Name ts))
                |> Seq.toList

            if nextEvents.IsEmpty then
                Seq.empty

            else
                // Get the next timestamp that we need to roll-forward to.
                let nextTimestamp =
                    nextEvents
                    |> Seq.map (fun { Timestamp = ts } -> ts)
                    |> Seq.min

                // Get the first component which has an event at that timestamp.
                let nextComponentToAction =
                    nextEvents
                    |> List.pick (function
                        | { Requestor = requestor; Timestamp = ts }
                            when ts = nextTimestamp -> Some requestor
                        | _ -> None)

                // Get all other events for that same component and that same timestamp.
                let nextEventsToAction =
                    nextEvents
                    |> Seq.filter (function
                        | { Requestor = requestor; Timestamp = ts } ->
                            obj.ReferenceEquals(requestor, nextComponentToAction) && ts = nextTimestamp)

                seq {                  
                    for event in nextEventsToAction do
                        let outcome =
                            processEvent instantiatorDetailMappings nextTimestamp event

                        yield (outcome, event)

                    yield! inner nextTimestamp
                }

        inner 0.0
                            
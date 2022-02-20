
namespace FSDiscreteEventSimulator

module Component =

    open Common

    let createUserSource<'TUser when 'TUser :> IUser>
            name (WaitSampler sampler) (userFactory: unit -> 'TUser) target =

        let arrivalTimes =
            sampler.GetEnumerator()

        let mutable nextUser =
            if arrivalTimes.MoveNext() then
                Some (arrivalTimes.Current, userFactory ())
            else
                None

        let mutable userToSend = None

        let sourceInstance =
            { new IDESUserCreator<_> with
                member _.Name = name

                member _.HasCreated (time, user) =
                    match nextUser, userToSend with
                    | Some (nextArrival', nextUser'), None
                        when time = nextArrival' && obj.ReferenceEquals(user, nextUser') ->
                            userToSend <- nextUser
                            nextUser <- None

                    | Some (_, nextUser'), None
                        when not <| obj.ReferenceEquals(user, nextUser') ->
                            failwith <| sprintf "Expected creation of '%s' but notified of '%s'."
                                nextUser'.Id user.Id

                    | Some (nextArrival', _), None ->
                            failwith <| sprintf "Expected creation of '%s' at %f but instead created at %f."
                                user.Id nextArrival' time

                    | Some _, Some (_, pendingUser) ->
                        failwith <| sprintf "User '%s' created but have yet to send on '%s'."
                            user.Id pendingUser.Id                           

                    | None, _ ->
                        failwith <| sprintf "User '%s' created but none expected." user.Id

                member _.HasSent (time, user) =
                    match nextUser, userToSend with
                    | None, Some (nextArrival', nextUser')
                        when time = nextArrival' && obj.ReferenceEquals(user, nextUser') ->
                            nextUser <-
                                if arrivalTimes.MoveNext () then
                                    Some (arrivalTimes.Current, userFactory ())
                                else
                                    None

                            userToSend <- None

                    | None, Some (_, nextUser')
                        when not <| obj.ReferenceEquals(user, nextUser') ->
                            failwith <| sprintf "Expected to send on '%s' but notified of '%s'."
                                nextUser'.Id user.Id

                    | None, Some (nextArrival', _) ->
                        failwith <| sprintf "Expected to send on '%s' at %f but instead sent on at %f."
                            user.Id nextArrival' time

                    | Some (_, nextUser'), Some _ ->
                        failwith <| sprintf "User '%s' sent on but have already created '%s'."
                            user.Id nextUser'.Id

                    | _, None ->
                        failwith <| sprintf "User '%s' sent on but none expected." user.Id

                member _.NextEvents (timestamp, _) =
                    match nextUser, userToSend with
                    | Some (nextArrival, nextUser), None
                        when timestamp <= nextArrival ->
                            [{ Timestamp = Absolute nextArrival
                               Request = CreateUser nextUser }]

                    | Some (nextArrival, _), None ->
                        failwith <| sprintf "Current timestamp of %f has moved beyond creation time of %f."
                            timestamp nextArrival

                    | None, Some (nextArrival, nextUser) 
                        when timestamp <= nextArrival ->
                            [{ Timestamp = Absolute nextArrival
                               Request = MoveUser (nextUser, target) }]

                    | None, Some (nextArrival, _) ->
                        failwith <| sprintf "Current timestamp of %f has moved beyond send time of %f."
                            timestamp nextArrival

                    | None, None ->
                        []

                    | Some (_, nextUser'), Some (_, nextUser'') ->
                        failwith <| sprintf "Unknown error... Have both user '%s' due for creation and '%s' due for sending."
                            nextUser'.Id nextUser''.Id }
                   
        { new IDESUserSenderInstantiator<_> with
            member _.Name = name
            member _.Create() = sourceInstance
            member _.Target = target }


    let createUserSink<'TUser when 'TUser :> IUser> name =

        let sinkInstance () =
            { new IDESUserDestroyer<'TUser> with
                member _.Name = name

                member _.CanRecieve (_, _) = true

                member _.HasRecieved (_, _) = ()

                member _.HasDestroyed (_, _) = ()

                member _.NextEvents (_, users) =
                    users
                    |> Seq.map (fun u ->
                        { Timestamp = Immediate
                          Request = DestroyUser u })
                    |> List.ofSeq }

        { new IDESUserRecieverInstantiator<'TUser> with
            member _.Name = name
            member _.Create() = sinkInstance () }


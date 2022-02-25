
namespace FSDiscreteEventSimulator.Component

module UserQueue =

    open System.Collections.Generic
    open FSDiscreteEventSimulator.Common


    type private UserQueue<'TUser when 'TUser :> IUser>
        (name, maxCapacity, targets: IDESTargetProxy<'TUser> list) =

        let userQueue = new Queue<'TUser>()


        interface IDESComponent<'TUser> with
            member _.Name = name

            member _.NextEvents (timestamp) =
                match userQueue.TryPeek() with
                | true, nextUser ->
                    match targets |> List.tryFind (fun t -> t.CanRecieve (timestamp, nextUser)) with
                    | Some target ->
                        [{ Timestamp = Immediate
                           User = nextUser
                           Type = MoveUser target.Instantiator }]
                    | None ->
                        []
                | _ ->
                    []

        interface IDESUserReciever<'TUser> with
            member _.CanRecieve (_, _) =
                match maxCapacity with
                | None -> true
                | Some max when max > userQueue.Count -> true
                | Some _ -> false

            member this.HasRecieved (_, user) =
                userQueue.Enqueue (user)

        interface IDESUserSender<'TUser> with
            member this.HasSent (_, user) =
                match userQueue.Dequeue() with
                | user' when obj.ReferenceEquals(user, user') -> ()
                | user' ->
                    failwith <| sprintf "Expected to send user '%s' but user '%s' sent instead."
                        user'.Id user.Id


    let create<'TUser when 'TUser :> IUser>
        name maxCapacity targets =

        { new IDESUserProcessorInstantiator<'TUser> with
            member _.Name = name

            member _.Description =
                sprintf "%s<'%s'>" typedefof<UserQueue<_>>.Name name

            member _.Create (_, _, targetProxies) =
                UserQueue (name, maxCapacity, targetProxies)

            member _.Targets = targets }

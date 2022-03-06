
namespace FSDiscreteEventSimulator.Component

module UserProcessor =

    open System.Collections.Generic
    open FSDiscreteEventSimulator.Common


    type private UserProcessor<'TUser when 'TUser :> IUser>
        (name: string, users: IReadOnlySet<'TUser>, waitTimeForUser: 'TUser -> float, target) =

            let mutable userBeingProcessed: (float * 'TUser) option = None

            interface IDESComponent<'TUser> with
                member _.Name = name

                member _.NextEvents (timestamp) =
                    match userBeingProcessed with
                    | None when users.Count = 0 -> []

                    | None ->
                        failwith <| sprintf "No user being processed but %i user(s) assigned."
                            users.Count

                    | Some (finishTime, _) when timestamp > finishTime ->
                        failwith <| sprintf "Current timestamp of %f has moved beyond creation time of %f."
                            timestamp finishTime

                    | Some (_, user') when users.Count <> 1 ->
                        failwith <| sprintf "Currently processing user '%s' but %i user(s) assigned."
                            user'.Id users.Count

                    | Some (_, user') when not <| users.Contains (user') ->
                        failwith <| sprintf "Currently processing user '%s' but '%s' assigned."
                            user'.Id (users |> Seq.exactlyOne).Id

                    | Some (finishTime, user') ->
                        [{ Timestamp = Absolute finishTime
                           User = user'
                           Type = MoveUser target }]
                               

            interface IDESUserReciever<'TUser> with
                member _.CanRecieve (_, _) = 
                    userBeingProcessed.IsNone                 

                member _.HasRecieved (timestamp, user) =
                    userBeingProcessed <-
                        match userBeingProcessed with
                        | None ->  
                            Some (timestamp + waitTimeForUser user, user)

                        | Some (_, user') ->
                            failwith <| sprintf "Cannot recieve '%s' as still processing '%s'."
                                user.Id user'.Id
                                

            interface IDESUserSender<'TUser> with
                member _.HasSent (timestamp, user) =
                    userBeingProcessed <-
                        match userBeingProcessed with
                        | Some (finishTime, _) when timestamp <> finishTime->
                            failwith <| sprintf "Expected to send user '%s' at %f but was sent intead at %f."
                                user.Id finishTime timestamp  

                        | Some (_, user') when not <| obj.ReferenceEquals(user, user') ->
                            failwith <| sprintf "Expected to send '%s' but asked to send '%s' instead."
                                user'.Id user.Id

                        | Some _ ->
                            None                                                   

                        | None ->
                            failwith <| sprintf "User '%s' set but none have been processed." user.Id                            


    let create<'TUser when 'TUser :> IUser>
            name waitTimeFactory target =

        { new IDESUserProcessorInstantiator<'TUser> with
            member _.Name = name

            member _.Description =
                sprintf "%s<'%s'>" typedefof<UserProcessor<'TUser>>.Name name

            member _.Create (randomizer, userCollection, targetProxies) =
                let waitTimeGenerator =
                    waitTimeFactory randomizer

                match targetProxies with
                | [targetProxy] ->
                    UserProcessor (name, userCollection, waitTimeGenerator, targetProxy.Instantiator)
                | _ ->
                    failwith "Unable to create user processor as multiple target proxies supplied (only one expected!)."

            member _.Targets = [ target ] }    

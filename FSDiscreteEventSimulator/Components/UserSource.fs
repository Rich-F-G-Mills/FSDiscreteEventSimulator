
namespace FSDiscreteEventSimulator.Component

module UserSource =

    open System.Collections.Generic
    open FSDiscreteEventSimulator.Common


    type private CreatedUser<'TUser when 'TUser :> IUser> =        
        | ReadyToCreate of ArrivalTime: float * NewUser: 'TUser
        | ReadyToSendOn of SendAt: float * NewUser: 'TUser
        | NoMoreUsers


    type private UserSource<'TUser when 'TUser :> IUser>
        (name, users: IReadOnlySet<'TUser>, sampler: WaitSampler, userFactory: unit -> 'TUser, target) =

            let waitTimes =
                let (WaitSampler sampler') =
                    sampler

                sampler'.GetEnumerator()

            let mutable sourceState =
                if waitTimes.MoveNext() then
                    ReadyToCreate (waitTimes.Current, userFactory ())
                else
                    NoMoreUsers


            interface IDESUserCreator<'TUser> with
                member _.Name = name
                
                member _.HasCreated (timestamp, user) =
                    sourceState <-
                        match sourceState with
                        | ReadyToCreate (nextArrival', _) when timestamp <> nextArrival' ->
                            failwith <| sprintf "Expected creation of '%s' at %f but was created at %f."
                                user.Id nextArrival' timestamp

                        | ReadyToCreate (_, nextUser') when not (user =|= nextUser') ->
                            failwith <| sprintf "Expected creation of '%s' but was notified of '%s'."
                                nextUser'.Id user.Id

                        | ReadyToCreate (nextArrival', nextUser') ->                        
                            ReadyToSendOn (nextArrival', nextUser')               
                
                        | ReadyToSendOn _ | NoMoreUsers ->
                            failwith <| sprintf "User '%s' created but none expected." user.Id
                
                member _.HasSent (timestamp, user) =
                    sourceState <-
                        match sourceState with
                        | ReadyToSendOn (nextArrival, _) when timestamp <> nextArrival ->
                            failwith <| sprintf "Expected to send on '%s' at %f but instead sent on at %f."
                                user.Id nextArrival timestamp

                        | ReadyToSendOn (_, nextUser) when not (user =|= nextUser) ->
                            failwith <| sprintf "Expected to send on '%s' but was notified of '%s'."
                                nextUser.Id user.Id

                        | ReadyToSendOn _ ->                        
                            if waitTimes.MoveNext () then
                                ReadyToCreate (timestamp + waitTimes.Current, userFactory ())
                            else
                                NoMoreUsers
                
                        | ReadyToCreate (_, nextUser) ->
                            failwith <| sprintf "Awaiting to create '%s' but notified that '%s' was sent on."
                                nextUser.Id user.Id 
                
                        | NoMoreUsers ->
                            failwith <| sprintf "User '%s' sent on but none expected." user.Id
                
                member _.NextEvents _ =
                    match sourceState with
                    | ReadyToCreate _ when users.Count <> 0 ->
                        failwith <| sprintf "Sink has %i user(s) assigned but none expected."
                            users.Count

                    | ReadyToCreate (nextArrival, nextUser) ->
                        [{ Timestamp = Absolute nextArrival
                           User = nextUser
                           Type = CreateUser }]
                
                    | ReadyToSendOn (nextArrival, nextUser) ->
                        [{ Timestamp = Absolute nextArrival
                           User = nextUser
                           Type = MoveUser target }]                            
                
                    | NoMoreUsers ->
                        []


    let create<'TUser when 'TUser :> IUser>
            name (samplerFactory: System.Random -> WaitSampler) (userFactory: System.Random -> unit -> 'TUser) target =           
        
        { new IDESUserSenderInstantiator<'TUser> with
            member _.Name = name

            member _.Description =
                sprintf "%s<'%s'>" typedefof<UserSource<'TUser>>.Name name

            member _.Create (randomizer, userCollection, targetProxies) =
                let waitSampler =
                    samplerFactory randomizer

                let userFactory' =
                    userFactory randomizer

                match targetProxies with
                | [targetProxy] ->
                    UserSource (name, userCollection, waitSampler, userFactory', targetProxy.Instantiator)
                | _ ->
                    failwith "Unable to create user source as multiple target proxies supplied (only one expected!)."

            member _.Targets = [ target ] }

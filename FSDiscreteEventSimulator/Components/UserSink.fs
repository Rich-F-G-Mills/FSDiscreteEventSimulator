
namespace FSDiscreteEventSimulator.Component

module UserSink =

    open System.Collections.Generic
    open FSDiscreteEventSimulator.Common


    type private UserSink<'TUser when 'TUser :> IUser>
        (name, users: IReadOnlySet<'TUser>) =

        interface IDESUserDestroyer<'TUser> with
            member _.Name = name
            
            member _.CanRecieve (_, _) = true
            
            member _.HasRecieved (_, _) = ()
            
            member _.HasDestroyed (_, _) = ()
            
            member _.NextEvents _ =
                users
                |> Seq.map (fun u ->
                    { Timestamp = Immediate; User = u; Type = DestroyUser })
                |> List.ofSeq


    let create<'TUser when 'TUser :> IUser> name =    
        { new IDESUserRecieverInstantiator<'TUser> with
            member _.Name = name

            member _.Description =
                sprintf "%s<'%s'>" typedefof<UserSink<_>>.Name name

            member _.Create (_, userCollection, targetProxies) =
                match targetProxies with
                | [] ->
                    UserSink (name, userCollection)
                | _ ->
                    failwith "Unable to create user sink as target proxies supplied (none expected!)." }

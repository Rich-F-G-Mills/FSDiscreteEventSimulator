
namespace FSDiscreteEventSimulator

module Component =

    open Common

    let createUserSource<'TUser when 'TUser :> IUser>
        name (WaitSampler sampler) (userFactory: unit -> 'TUser) target =

        let arrivalTimes =
            sampler
            |> Seq.map (fun time -> (time, userFactory ()))
            |> Seq.cache

        let sourceInstance =
            { new IDESComponent<'TUser> with
              member _.Name = name
              member _.NextEvents ctx =
                arrivalTimes
                |> Seq.tryFind (fun (time, _) -> time >= ctx.Timestamp)
                |> function
                    | Some (time, newUser) ->
                        [{ Timestamp = Absolute time
                           Order = 0
                           Request = CreateUserAndMove (newUser, target) }]
                    | None ->
                        [] }

        { new IDESUserSenderInstantiator<'TUser> with
          member _.Name = name
          member _.Create() = sourceInstance
          member _.Target = target }


    let createUserSink<'TUser when 'TUser :> IUser> name =

        let sinkInstance =
            { new IDESUserReciever<'TUser> with
              member _.Name = name
              member _.CanRecieve _ = true
              member _.HasRecieved ({ Timestamp = ts }, user) =
                [{ Timestamp = Immediate
                   Order = 0
                   Request = DestroyUser user }]
              member _.NextEvents _ = [] }            

        { new IDESUserRecieverInstantiator<'TUser> with
          member _.Name = name
          member _.Create() = sinkInstance }


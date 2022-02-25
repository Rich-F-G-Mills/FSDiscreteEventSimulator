
namespace FSDiscreteEventSimulator

module Common =

    open System.Collections.Generic
    open System.Collections.Immutable


    module Seq =
        open System.Linq

        let toDictionary (s: seq<'K * 'V>) =
            s.ToDictionary(fst, snd)

        let toImmutableDictionary (s: seq<'K * 'V>) =
            s.ToImmutableDictionary(fst, snd)

    type WaitSampler =
        WaitSampler of Sampler: float seq

    type IUser =
        interface
            abstract member Id: string with get
        end

    type Outcome =
        | Successful
        | UserDropped
    
    type SimulationContext<'TUser when 'TUser :> IUser> =
        { Timestamp: float
          Users: ImmutableDictionary<IDESComponent<'TUser>, ImmutableHashSet<'TUser>> }

    and DESTimestamp =
        | Immediate
        | Relative of Offset: float
        | Absolute of Timestamp: float

    and DESLocalRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: DESTimestamp
          User: 'TUser
          Type: DESRequestType<'TUser> }

    and DESRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: float
          Requestor: IDESComponentInstantiator<'TUser>
          User: 'TUser
          Type: DESRequestType<'TUser> }          
    
    and [<StructuredFormatDisplayAttribute("{Formatted}")>]
        DESRequestType<'TUser when 'TUser :> IUser> =
        | CreateUser
        | MoveUser of To: IDESUserRecieverInstantiator<'TUser>
        | MoveUserWithMap of MapTo: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | DestroyUser

        member this.Formatted =
            match this with
            | CreateUser ->
                sprintf "CreateUser"
            | MoveUser target ->
                sprintf "MoveUser to '%s'" target.Name
            | MoveUserWithMap (userAfter, target) ->
                sprintf "MoveUserWithMap %A to '%s'"
                    userAfter target.Name
            | DestroyUser ->
                sprintf "DestroyUser"

        override this.ToString() =
            this.Formatted


    and IDESComponent<'TUser when 'TUser :> IUser> =
        interface
            abstract member Name: string with get
            abstract member NextEvents: float -> DESLocalRequest<'TUser> list
        end

    and IDESUserReciever<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponent<'TUser>
            abstract member CanRecieve: float * 'TUser -> bool
            abstract member HasRecieved: float * 'TUser -> unit
        end

    and IDESUserSender<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponent<'TUser>
            abstract member HasSent: float * 'TUser -> unit
        end

    and IDESUserCreator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESUserSender<'TUser>
            abstract member HasCreated: float * 'TUser -> unit
        end

    and IDESUserDestroyer<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESUserReciever<'TUser>
            abstract member HasDestroyed: float * 'TUser -> unit
        end

    and IDESComponentInstantiator<'TUser when 'TUser :> IUser> =        
        interface            
            abstract member Name: string with get
            abstract member Description: string with get
            abstract member Create: System.Random * IReadOnlySet<'TUser> * IDESTargetProxy<'TUser> list -> IDESComponent<'TUser>
        end

    and IDESUserRecieverInstantiator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponentInstantiator<'TUser>       
        end

    and IDESUserSenderInstantiator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponentInstantiator<'TUser>
            abstract member Targets: IDESUserRecieverInstantiator<'TUser> list with get
        end

    and IDESUserProcessorInstantiator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESUserRecieverInstantiator<'TUser>
            inherit IDESUserSenderInstantiator<'TUser>
        end

    and IDESTargetProxy<'TUser when 'TUser :> IUser> =
        interface
            abstract member CanRecieve: float * 'TUser -> bool
            abstract member UserList: IReadOnlySet<'TUser>
            abstract member Instantiator: IDESUserRecieverInstantiator<'TUser>
        end
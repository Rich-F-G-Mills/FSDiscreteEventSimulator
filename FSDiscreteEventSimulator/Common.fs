
namespace FSDiscreteEventSimulator

open System.Collections.Generic

module Common =

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

    type Outcome<'T> =
        | Successful of 'T
        | UserDropped of 'T
    
    type SimulationContext<'TUser when 'TUser :> IUser> =
        { Timestamp: float
          Users: ImmutableDictionary<IDESComponent<'TUser>, ImmutableHashSet<'TUser>> }

    and DESTimestamp =
        | Immediate
        | Relative of Offset: float
        | Absolute of Timestamp: float

    and DESLocalRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: DESTimestamp
          Request: DESRequestType<'TUser> }

    and [<StructuredFormatDisplay("{Formatted}")>]
        DESRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: float
          Requestor: IDESComponent<'TUser>
          Request: DESRequestType<'TUser> }

        member this.Formatted =            
            sprintf "%-40s   %A"
                (sprintf "'%s' @ %.3f:" this.Requestor.Name this.Timestamp)
                this.Request            
    
    and [<StructuredFormatDisplayAttribute("{Formatted}")>]
        DESRequestType<'TUser when 'TUser :> IUser> =
        | CreateUser of User: 'TUser
        | MoveUser of User: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | MoveUserWithMap of UserBefore: 'TUser * UserAfter: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | DestroyUser of User: 'TUser

        member this.Formatted =
            match this with
            | CreateUser user ->
                sprintf "CreateUser %A" user
            | MoveUser (user, target) ->
                sprintf "MoveUser %A to '%s'" user target.Name
            | MoveUserWithMap (userBefore, userAfter, target) ->
                sprintf "MoveUserWithMap %A -> %A to '%s'"
                    userBefore userAfter target.Name
            | DestroyUser user ->
                sprintf "DestroyUser %A" user

        override this.ToString() =
            this.Formatted


    and IDESComponent<'TUser when 'TUser :> IUser> =
        interface
            abstract member Name: string with get
            abstract member NextEvents: float * 'TUser seq -> DESLocalRequest<'TUser> list
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
            abstract member Create: unit -> IDESComponent<'TUser>
        end

    and IDESUserRecieverInstantiator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponentInstantiator<'TUser>       
        end

    and IDESUserSenderInstantiator<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponentInstantiator<'TUser>
            abstract member Target: IDESUserRecieverInstantiator<'TUser> with get
        end
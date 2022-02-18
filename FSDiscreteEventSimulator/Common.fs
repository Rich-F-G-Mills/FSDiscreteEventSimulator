
namespace FSDiscreteEventSimulator

module Common =

    module Seq =
        open System.Linq

        let toDictionary (s: seq<'K * 'V>) =
            s.ToDictionary(fst, snd)

    type WaitSampler =
        WaitSampler of Sampler: float seq

    type SimulationContext =
        { Timestamp: float }

    type IUser =
        interface
            abstract member Id: string with get
        end

    type DESRequestHeader<'TUser when 'TUser :> IUser> =
        { Requestor: IDESComponent<'TUser>
          Header: DESLocalRequest<'TUser> }

    and DESTimestamp =
        | Immediate
        | Relative of Offset: float
        | Absolute of Timestamp: float

    and DESLocalRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: DESTimestamp
          Request: DESRequest<'TUser> }

    and DESRequest<'TUser when 'TUser :> IUser> =
        { Timestamp: float
          Requestor: IDESComponent<'TUser>
          Request: DESRequestType<'TUser> }

    and DESRequestType<'TUser when 'TUser :> IUser> =
        | CreateUser of User: 'TUser
        | MoveUser of User: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | CreateUserAndMove of User: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | MoveUserWithMap of UserBefore: 'TUser * UserAfter: 'TUser * To: IDESUserRecieverInstantiator<'TUser>
        | DestroyUser of User: 'TUser

    and IDESComponent<'TUser when 'TUser :> IUser> =
        interface
            abstract member Name: string with get
            abstract member NextEvents: SimulationContext -> DESLocalRequest<'TUser> list
        end

    and IDESUserReciever<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponent<'TUser>
            abstract member CanRecieve: SimulationContext -> bool
            abstract member HasRecieved: SimulationContext * 'TUser -> DESLocalRequest<'TUser> list
        end

    and IDESUserSender<'TUser when 'TUser :> IUser> =
        interface
            inherit IDESComponent<'TUser>
            abstract member HasSent: SimulationContext * 'TUser -> DESLocalRequest<'TUser> list
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
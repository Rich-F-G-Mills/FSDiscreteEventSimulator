
open System
open MathNet.Numerics.Distributions

open FSDiscreteEventSimulator
open FSDiscreteEventSimulator.Common
open FSDiscreteEventSimulator.Component

    
let createSamplerFromDist (distributionFactory: System.Random -> IContinuousDistribution) count randomizer =
    let distribution =
        distributionFactory randomizer

    let distSamples =
        Seq.initInfinite (fun _ -> distribution.Sample ())

    match count with
    | Some count' when count' > 0 ->
        distSamples
        |> Seq.take count'
        |> WaitSampler

    | _ ->
        WaitSampler distSamples


[<StructuredFormatDisplay("DefaultUser<'{Id}'>")>]
type DefaultUser (Id: string) =
    interface IUser with
        member _.Id = Id

    member this.Id = (this :> IUser).Id

    static member createRandomInstance () =
        DefaultUser <| (Guid.NewGuid()).ToString().Substring(0, 6)
          
  
let alertSink =
    UserSink.create "Processed Alerts"

let alertProcessor1 =
    let waitForUser randomizer =
        let sampler =
            new Exponential (0.1, randomizer)

        fun _ ->
            sampler.Sample()       

    UserProcessor.create "Alert Processor 1" waitForUser alertSink

let alertProcessor2 =
    let waitForUser randomizer =
        let sampler =
            new Exponential (0.1, randomizer)

        fun _ ->
            sampler.Sample()       

    UserProcessor.create "Alert Processor 2" waitForUser alertSink

let alertQueue =
    UserQueue.create "Alert Queue" None [alertProcessor1; alertProcessor2]

let userGenerator _ _ =
    DefaultUser.createRandomInstance ()

let alertSource1 =
    let alertSampler =
        createSamplerFromDist (fun r -> new Exponential (0.5, r)) (Some 50)

    UserSource.create "Alert Generator 1" alertSampler userGenerator alertQueue

let alertSource2 =
    let alertSampler =
        createSamplerFromDist (fun r -> new Exponential (1.0, r)) (Some 50)

    UserSource.create "Alert Generator 2" alertSampler userGenerator alertQueue

let desNetwork =
    Network.empty
    |> Network.addInstantiator alertSource1
    |> Network.addInstantiator alertSource2
    |> Network.addInstantiator alertProcessor1
    |> Network.addInstantiator alertProcessor2
    |> Network.addInstantiator alertQueue
    |> Network.addInstantiator alertSink
    |> Network.finalise

let events =
    Seq.init 1000 (fun _ ->        
        desNetwork
        |> Network.simulate (new System.Random())
        |> Seq.last
        |> fun (_, { Timestamp = ts }) -> ts)
    |> Seq.toList

printfn "Min = %f   Max = %f   Mean = %f"
    (events |> List.min) (events |> List.max) (events |> List.average)

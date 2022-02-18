
open System
open System.Linq
open MathNet.Numerics.Distributions

open DiscreteEventSimulator
open DiscreteEventSimulator.Common
open DiscreteEventSimulator.Component





let randomWaitTime distSampler start count =
    let samples =
        distSampler
        |> Seq.scan (+) start
        |> Seq.tail

    match count with
    | Some count' when count' > 0 ->
        samples
        |> Seq.take count'

    | _ -> samples
    
let createSamplerFromDist (distribution: IContinuousDistribution) start count =    
    let distSamples =
        Seq.initInfinite (fun _ -> distribution.Sample ())
        |> Seq.scan (+) start
        |> Seq.tail

    match count with
    | Some count' when count' > 0 ->
        distSamples
        |> Seq.take count'
        |> WaitSampler

    | _ ->
        WaitSampler distSamples




type DefaultUser (Id: string) =
    interface IUser with
        member _.Id = Id

    static member createRandomInstance () =
        DefaultUser <| (Guid.NewGuid()).ToString()


                    
                
  
let alertSink =
    createUserSink "Process alerts"

let alertSource1 =
    let alertSampler =
        createSamplerFromDist (new Exponential (0.5)) 0.0 None

    createUserSource "Alert Generator 1" alertSampler DefaultUser.createRandomInstance alertSink

let alertSource2 =
    let alertSampler =
        createSamplerFromDist (new Exponential (1.0)) 0.0 None

    createUserSource "Alert Generator 2" alertSampler DefaultUser.createRandomInstance alertSink

let desNetwork =
    Network.empty
    |> Network.addInstantiator alertSink
    |> Network.addInstantiator alertSource1
    |> Network.addInstantiator alertSource2
    |> Network.finalise

printfn "TEST"

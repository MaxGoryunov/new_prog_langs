open System
open System.Threading.Tasks
open FSharp.Collections

let countElementsNonParallel (predicate: int -> bool) (list: int list) : int =
    list |> List.filter predicate |> List.length

let countElementsParallel (predicate: int -> bool) arr : int =
    arr |> Array.Parallel.choose (fun x -> if predicate x then Some x else None) |> Array.length


let timeAction (action: unit -> 'T) : (float * 'T) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let result = action()
    sw.Stop()
    (sw.Elapsed.TotalMilliseconds, result)

[<EntryPoint>]
let main argv =
    let size = 1000 * 1000 * 20
    let nonParallelTime, nonParallelResult = timeAction (fun () -> countElementsNonParallel (fun x -> x % 2 = 0) [1 .. size])
    printfn "Non-Parallel Count: %d took %f ms" nonParallelResult nonParallelTime
    let parallelTime, parallelResult = timeAction (fun () -> countElementsParallel (fun x -> x % 2 = 0) (Array.ofList [1 .. size]))
    printfn "Parallel Count: %d took %f ms" parallelResult parallelTime
    0 // return an integer exit code

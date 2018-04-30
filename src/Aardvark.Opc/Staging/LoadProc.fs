namespace Aardvark.Base.Incremental 

open System
open System.IO
open System.Threading
open System.Collections.Generic
open System.Reactive.Subjects

open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Monads.State
open Aardvark.Base.Incremental

type ILoadTask<'a> =
    inherit IMod<Option<'a>>
    abstract member Tag : obj with get,set
    abstract member IsRunning : bool
    abstract member Start : unit -> unit
    abstract member Stop : unit -> unit


type LoadState =
    {
        undo : list<unit -> unit>
    }

type Load<'a> = { run : Proc<LoadState, 'a> }

module Load =

    let map (f : 'a -> 'b) (m : Load<'a>) =
        { run =
            proc {
                let! r = m.run
                return f r
            }
        }

    let onCancel (f : unit -> unit) : Load<unit> =
        { run =
            proc {
                do! State.modify (fun s -> { s with undo = f :: s.undo})    
            }
        }

    let disposeOnCancel<'a when 'a :> IDisposable> (d : 'a) =
        onCancel (fun () -> (d :> IDisposable).Dispose())

    let ofTask (t : System.Threading.Tasks.Task<'a>) =
        { run = Proc.ofTask t }

    let bind (f : 'a -> Load<'b>) (m : Load<'a>) =
        { run = Proc.bind (fun v -> (f v).run) m.run }

    let append (l : Load<unit>) (r : Load<'a>) =
        { run = Proc.append l.run r.run }

    let value (a : 'a) =
        { run = Proc.value a }


    type private IRunner =
        abstract member Step : Time -> bool

    type private Runner<'a>(initialState : LoadState, proc : Proc<LoadState, 'a>, dirty : unit -> unit) as this =
        inherit Mod.AbstractMod<Option<'a>>()

        let subscriptions = Dictionary<IObservable, IDisposable>()
        let queue = Queue<IObservable * obj * Time>()
        let mutable pending : Option<Pattern * (obj -> Proc<LoadState, 'a>)> = None
        let mutable state = { time = Time.Now; userState = initialState }
        let mutable dependsOnTime = false
        let mutable result = None
        let mutable refCount = 0

        let mutable tag : obj = null

        static let dummy = System.Threading.Tasks.Task.FromResult 10 |> Pattern.ofTask

        let push (source : IObservable) (value : obj) =
            lock queue (fun () -> 
                queue.Enqueue(source, value, Time.Now)
                dirty()
            )

        let processEvent (source : IObservable) (value : obj) =
            match pending with
                | Some (p,cont) ->
                    let mutable dt = false
                    let mutable newPending = None

                    let finished = 
                        match p.MatchUntyped(state,source,value) with
                            | Some v -> 
                                let c = cont v
                                match c.run.Run(&state) with
                                    | Continue(p,c) ->
                                        newPending <- Some (p,c)
                                        dt <- dt || p.DependsOnTime
                                        false
                                    | Finished v ->
                                        result <- Some v
                                        true
                            | None ->
                                newPending <- Some (p,cont)
                                dt <- dt || p.DependsOnTime
                                false

                    if finished then
                        dependsOnTime <- false
                        subscriptions.Values |> Seq.iter (fun d -> d.Dispose())
                        subscriptions.Clear()
                        queue.Clear()

                        true

                    else 
                        let added, removed =
                            match pending, newPending with
                                | Some (o,_), Some (n,_) ->
                                    let added = n.Relevant |> Seq.filter (fun v -> HSet.contains v o.Relevant |> not) |> Seq.toList
                                    let removed = o.Relevant |> Seq.filter (fun v -> HSet.contains v n.Relevant |> not) |> Seq.toList
                                    added, removed

                                | None, Some (n,_) ->
                                    Seq.toList n.Relevant, []

                                | Some (o,_), None ->
                                    [], Seq.toList o.Relevant

                                | None, None ->
                                    [], []

                        for a in added do
                            subscriptions.[a] <- a.Subscribe(fun o -> push a o)

                        for r in removed do
                            match subscriptions.TryGetValue r with
                                | (true, s) -> 
                                    subscriptions.Remove r |> ignore
                                    s.Dispose()
                                | _ -> ()

                        pending <- newPending
                        dependsOnTime <- dt
                        false

                | None -> 
                    dependsOnTime <- false
                    subscriptions.Values |> Seq.iter (fun d -> d.Dispose())
                    subscriptions.Clear()
                    queue.Clear()
                    true

        let setTime (t : Time) =
            if t > state.time then
                state <- { state with time = t }
                if dependsOnTime then processEvent null null
                else false
            else
                false

        let step (tTarget : Time) =
            let mutable finished = false
            while queue.Count > 0 do
                let (s,e,t) = queue.Dequeue()
                finished <- finished || setTime t
                finished <- finished || processEvent s e

            finished || setTime tTarget
                    
        let kill() =
            dependsOnTime <- false
            subscriptions.Values |> Seq.iter (fun d -> d.Dispose())
            subscriptions.Clear()
            queue.Clear()
            result <- None 
            for u in state.userState.undo do u()
            state <- { time = Time.Now; userState = initialState }


        member x.Start () =
            if Interlocked.Increment(&refCount) = 1 then

                pending <- Some (dummy :> Pattern,(fun _ -> proc))
                state <- { time = Time.Now; userState = initialState }

                queue.Enqueue(dummy.Relevant |> Seq.head, 10 :> obj, Time.Now)
                //transact (fun () -> x.MarkOutdated())
                dirty()


        member x.Stop() =
            if Interlocked.Decrement(&refCount) = 0 then
                dirty ()

        member x.Step(t : Time) =
            if refCount <= 0 then
                kill ()
                transact (fun () -> this.MarkOutdated())
                true
            else
                let shouldMark = step t
                if refCount <= 0 then 
                    kill()
                    transact (fun () -> this.MarkOutdated())
                    true
                else
                    if shouldMark then transact (fun () -> this.MarkOutdated())
                    false

        member x.IsRunning = 
            refCount > 0

        override x.Compute(token) =
            result

        interface IRunner with
            member x.Step t = x.Step t

        interface ILoadTask<'a> with
            member x.Tag  
                with get () = tag
                and set v = tag <- v

            member x.Start() = x.Start()
            member x.Stop() = x.Stop()
            member x.IsRunning = x.IsRunning

    type Runner(threads : int, getLock  : int -> IDisposable) =
        let dirtyRunners = ConcurrentHashQueue<IRunner>()
        let sem = new SemaphoreSlim(0)
        let cancel = new CancellationTokenSource()

        [<VolatileField>]
        let mutable remaining = threads

        let mainLoop id =
            let token = cancel.Token
            let runner () =
                use l = lock sem (fun () -> getLock id)
                while true do
                    sem.Wait(token)
                    match dirtyRunners.TryDequeue() with
                        | (true, r) ->
                            r.Step(Time.Now) |> ignore
                        | _ ->
                            ()

            async {
                try
                    do! Async.SwitchToNewThread()
                    runner ()
                with :? OperationCanceledException ->
                    Interlocked.Decrement(&remaining) |> ignore
            }

        do for i in 0 .. threads - 1 do Async.Start (mainLoop i)

        new() = new Runner(1, fun i -> { new IDisposable with member x.Dispose() = () } )
        new(cnt) = new Runner(cnt, fun i -> { new IDisposable with member x.Dispose() = () } )

        member x.Enqueue (p : Load<'a>) : ILoadTask<'a> =
            let mutable self = Unchecked.defaultof<_>
            let dirty() =
                if dirtyRunners.Enqueue (self :> IRunner) then
                    sem.Release() |> ignore

            self <- Runner<'a>({ undo = [] }, p.run, dirty)
            self :> ILoadTask<'a>

        member x.Dispose() =
            cancel.Cancel()
            while remaining > 0 do ()
            sem.Dispose()
            cancel.Dispose()
            dirtyRunners.Clear()

        interface IDisposable with
            member x.Dispose() = x.Dispose()

[<AutoOpen>]
module ``Load Builder`` =
    type LoadBuilder() =

        member x.Using<'a, 'b when 'a :> IDisposable> (v : 'a, f : 'a -> Load<'b>) : Load<'b> =
            let c = Load.disposeOnCancel v
            let body =
                v |> f |> Load.map(fun r ->
                    (v :> IDisposable).Dispose()
                    r
                )

            Load.append c body


        member x.Bind(m : System.Threading.Tasks.Task<'a>, f : 'a -> Load<'b>) =
            Load.bind f (Load.ofTask m)

        member x.Bind(m : Async<'a>, f : 'a -> Load<'b>) =
            Load.bind f (Load.ofTask (Async.StartAsTask m))

        member x.Bind(m : Load<'a>, f : 'a -> Load<'b>) =
            Load.bind f m

        member x.Return(v : 'a) = 
            Load.value v

        member x.Combine(l : Load<unit>, r : Load<'a>) =
            { run =
                Proc.append l.run r.run
            }

        member x.Delay(f : unit -> Load<'a>) =
            { run = Proc.delay (fun () -> f().run)}

        member x.Zero() =
            Load.value ()

        member x.For(s : seq<'a>, f : 'a -> Load<unit>) =
            { run =
                proc {
                    for e in s do
                        do! (f e).run
                }
            }

        member x.While(guard : unit -> bool, body : Load<unit>) =
            { run =
                proc {
                    while guard() do
                        do! body.run
                }
            }

    let load = LoadBuilder() 


[<AutoOpen>]
module ``RuntimeLoad Extensions`` =
    type IRuntime with
        member x.CreateLoadRunner(threads : int) =
            match x with
                | :? Aardvark.Rendering.GL.Runtime as runtime -> 
                    let loadContexts = Aardvark.Rendering.GL.ContextHandle.createContexts false threads |> Array.ofList
                    let getLock i = 
//                        runtime.Context.ResourceLock
                        let ctx = loadContexts.[i]
                        runtime.Context.RenderingLock(ctx)

                    new Load.Runner(2, getLock)
                | _ -> failwith "cannot create load runner with non GL runtime."

module Test =

    open System.Runtime.InteropServices

    let loadTest() =
        use r = new Load.Runner(4)
        let mutable allocated = 0

        let myLoad (file : string) =
            load {
                use stream = File.OpenRead(file)
                
                let buffer = Array.zeroCreate 4096
                let mutable remaining = stream.Length

                let ptr = Marshal.AllocHGlobal (int stream.Length)
                Interlocked.Increment(&allocated) |> ignore
                do! Load.onCancel (fun () -> 
                    Interlocked.Decrement(&allocated) |> ignore
                    Marshal.FreeHGlobal ptr
                )


                let mutable c = ptr
                while remaining > 0L do
                    let! r = stream.ReadAsync(buffer, 0, min buffer.Length (int remaining))
                    Marshal.Copy(buffer, 0, c, int r)
                    remaining <- remaining - int64 r
                    c <- c + nativeint r

                return ptr
            }


        let load = myLoad @"C:\Users\schorsch\Desktop\clipper_ver6.2.1.zip"
        let tasks = Array.init 4096 (fun _ -> r.Enqueue load)

        for t in tasks do
            t.Start()

        Console.ReadLine() |> ignore

        for t in tasks do
            t.Stop()

        printfn "allocated: %A" allocated




        ()
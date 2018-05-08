namespace Aardvark.Opc.Utilities

open Aardvark.Base
open Aardvark.Base.IO
open Aardvark.Base.Incremental

module Statistics = 

    type Stats = { loadedObjects : int; elapsed : MicroTime; elapsedCreate : MicroTime; 
                   elapsedIndex   : MicroTime }

    let empty = { loadedObjects = 0; elapsed = MicroTime.Zero; elapsedCreate = MicroTime.Zero; elapsedIndex = MicroTime.Zero }
    
    let private lockObj = obj()
    let mutable private stats = empty
    let mutable private initialized = false

    let start () =
        async {
            do! Async.SwitchToNewThread()
            while true do
                System.Threading.Thread.Sleep 1000
                let old = lock lockObj (fun () -> let r = stats in stats <- empty; r)
                ()
                //printfn "Status: %A, resulting in %A per object %A" old (old.elapsed / float old.loadedObjects) (old.elapsedCreate / float old.loadedObjects) 
        } |> Async.Start 

    let emitLoaded (timeSpan : MicroTime) (createTime : MicroTime) (elapsedIndex : MicroTime) =  
        lock lockObj (fun () -> 

            if not initialized then initialized <- true; start() 
            stats <- 
                { stats with 
                    loadedObjects  = stats.loadedObjects + 1
                    elapsed        = stats.elapsed       + timeSpan
                    elapsedCreate  = stats.elapsedCreate + createTime
                    elapsedIndex   = stats.elapsedIndex  + elapsedIndex
                }
        )

[<AutoOpen>]
module Utilitites =

    let computeScreenSpaceArea (b : Box3d) (viewProj : Trafo3d) =
        let viewSpacePoints =
            b.ComputeCorners()
                |> Array.map (fun v ->
                    viewProj.Forward.TransformPosProj v
                )

        let poly = viewSpacePoints |> Array.map Vec.xy |> Polygon2d

        let clipped = 
            poly.ComputeConvexHullIndexPolygon().ToPolygon2d()//.ConvexClipped(Box2d(-V2d.II, V2d.II))
    
        let bounds = Box3d(viewSpacePoints)
        if Box3d(-V3d.III, V3d.III).Intersects(bounds) |> not then 0.0
        else clipped.ComputeArea()

    let hsv2rgb (h : float) (s : float) (v : float) =
        let s = clamp 0.0 1.0 s
        let v = clamp 0.0 1.0 v

        let h = h % 360.0
        let h = if h < 0.0 then h + 360.0 else h
        let hi = floor ( h / 60.0 ) |> int
        let f = h / 60.0 - float hi
        let p = v * (1.0 - s)
        let q = v * (1.0 - s * f)
        let t = v * (1.0 - s * ( 1.0 - f ))
        match hi with
            | 1 -> C4f(q,v,p)
            | 2 -> C4f(p,v,t)
            | 3 -> C4f(p,q,v)
            | 4 -> C4f(t,p,v)
            | 5 -> C4f(v,p,q)
            | _ -> C4f(v,t,p)

    let timed f =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let r = f ()
        sw.Stop()
        r, sw.MicroTime
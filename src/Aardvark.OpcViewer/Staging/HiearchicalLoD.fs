namespace Aardvark.Rendering.SceneGraph.HierarchicalLoD

open System
open System.Threading
open System.Threading.Tasks
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

type ILodData =
    abstract member BoundingBox : Box3d
    abstract member Traverse : (LodDataNode -> (LodDataNode -> list<'a>) -> 'a) -> 'a
    abstract member GetData : node : LodDataNode -> Async<Option<IndexedGeometry>>
                  

type LodNode( data : ILodData, fboSig : IFramebufferSignature) =
    interface ISg
    member x.Data = data
    member x.FboSig = fboSig

[<AutoOpen>]
module Helpers =
    let disposeOnCancel<'a when 'a :> IDisposable> (f : unit -> 'a) : Async<'a> =
        async {
            let mutable call = None
            let! _ = Async.OnCancel(fun () -> call |> Option.iter Disposable.dispose)
            let r = f()
            call <- Some (r :> IDisposable)
            return r
        }

type RoseTree<'a> =
    | Empty
    | Leaf of 'a
    | Node of 'a * list<RoseTree<'a>>

module RoseTree =
    open System.Collections.Generic

    let rec traverse<'a, 'b> (equal : 'b -> 'a -> bool) (create : 'a -> 'b) (destroy : 'b -> unit) (ref : RoseTree<'b>) (t : RoseTree<'a>) : RoseTree<'b> =
        let traverse = traverse equal create destroy

        match ref, t with
            | Empty, Empty -> 
                Empty

            | Empty, Leaf v ->
                v |> create |> Leaf

            | Empty, Node(v, children) ->
                let n = v |> create
                Node(n, children |> List.map (traverse Empty))

            | Leaf v, Empty ->
                destroy v
                Empty

            | Leaf l, Leaf r ->
                if equal l r then 
                    Leaf l
                else 
                    destroy l
                    r |> create |> Leaf

            | Leaf l, Node(r, children) ->
                if equal l r then
                    Node(l, children |> List.map (traverse Empty))
                else
                    destroy l
                    let n = r |> create
                    Node(n, children |> List.map (traverse Empty))

            | Node(v,c), Empty ->
                destroy v
                c |> List.iter (fun c -> traverse c Empty |> ignore)
                Empty

            | Node(v,c), Leaf r ->
                c |> List.iter (fun c -> traverse c Empty |> ignore)
                if equal v r then
                    Leaf v
                else
                    destroy v
                    r |> create |> Leaf
                                            
            | Node(lv,lc), Node(rv,rc) ->
                if equal lv rv then
                    Node(lv, List.map2 traverse lc rc)
                else
                    destroy lv
                    let nv = rv |> create
                    Node(nv, List.map2 traverse lc rc)
    
    let filter (godeeper : IAdaptiveObject -> 'a -> bool) (tree : IMod<RoseTree<'a>>) : IMod<RoseTree<'a>> =
        Mod.custom (fun self ->
            let rec traverse (t : RoseTree<'a>) =
                match t with
                    | Empty -> Empty
                    | Leaf v -> Leaf v
                    | Node(v,children) ->
                        if godeeper self v then
                            Node(v,List.map traverse children)
                        else
                            Leaf v

            let t = tree.GetValue self
            traverse t
        )
           

    let mapAsync<'a,'b when 'b :> IDisposable and 'a : equality> (create : 'a -> ILoadTask<'b>) (tree : IMod<RoseTree<'a>>) : IMod<RoseTree<Option<'b>>> =
        let current = ref Empty

        let l = Mod.custom (fun self ->

            let equal (l : ILoadTask<'b>) (r : 'a) =
                System.Object.Equals(l.Tag,r)
                
            let create (v : 'a) = 
                let r = create v
                r.Tag <- v
                r.Start()
                r

            let destroy (v : ILoadTask<'b>) =
                v.Stop()

            let t = tree.GetValue self

            let n = traverse equal create destroy !current t
            current := n
            n
        )

        let current = ref Empty
        Mod.custom (fun self ->
                    
            let lt = l.GetValue(self)

            let allReady (c : list<RoseTree<ILoadTask<'b>>>) =
                c |> List.forall (fun t ->
                    match t with
                        | Empty -> true
                        | Leaf v -> Option.isSome (v.GetValue(self))
                        | Node(v,_) -> Option.isSome (v.GetValue(self))
                )

            let rec peek (t : RoseTree<ILoadTask<'b>>) =
                match t with
                    | Empty -> Empty
                    | Leaf l ->
                        match l.GetValue(self) with
                            | Some v -> Leaf (Some v)
                            | None -> Empty
                    | Node(v, children) ->
                        match v.GetValue(self) with
                            | Some v ->
                                if allReady children then
                                    Node(None, List.map peek children)
                                else
                                    Leaf(Some v)
                            | None ->
                                Empty

            peek lt

        )

    let flatten mapF (f : IMod<RoseTree<Option<'a>>>) =
        let old = ref Empty
        ASet.custom (fun self -> 
            let current = f.GetValue self
            let deltas = List<_>()
            let create v = 
                match v with
                    | Some v -> deltas.Add(Add (mapF v))
                    | None -> ()
                v
            let destroy v =
                match v with
                    | Some v -> deltas.Add (Rem (mapF v))
                    | None -> ()

            let u = traverse (=) create destroy !old current
            old := u
            deltas |> CSharpList.toList
        )

    let rec map (f : 'a -> 'b) (t : RoseTree<'a>) =
        match t with
            | Empty -> Empty
            | Leaf v -> Leaf (f v)
            | Node(v,c) -> Node(f v, List.map (map f) c)   
namespace PRo3D

module AnnotationGroupsImporter = 
    open System
    open System.Xml.Linq
    open System.Xml

    open Aardvark.Base
    open Aardvark.UI
    open PRo3D.Groups

    let xname s = XName.Get(s)

    //let rec flatten (g:Node) : hset<Guid> =
    //    match ((PList.toList g.leaves ), (PList.toList g.subNodes)) with
    //        | ([], []) -> HSet.empty
    //        | (c, [] ) -> c |> HSet.ofList
    //        | ([], sg) -> sg |> HSet.ofList |> HSet.collect flatten
    //        | (c, sg)  -> 
    //            let sga = 
    //                sg |> HSet.ofList |> HSet.collect flatten
    //            HSet.union (c |> HSet.ofList) sga

    let rec getGroups (m:XElement) : (Node * hmap<Guid,Leaf>) = 
        let name = (m.Element(xname "Name")).Value.ToString()
        let visible = (m.Element(xname "IsVisible")).Value.ToBool()
                           
        let annotations = (m.Elements(xname "Measurements").Elements(xname "object")) 
                            |> List.ofSeq
                            |> List.map (fun x -> MeasurementsImporter.getAnnotation x)                           
                            |> List.map(fun x -> Leaf.Annotations x)

        let flat' = annotations |> List.map(fun x -> 
                                    Log.warn "added key %A" x.id
                                    x.id, x) |> HMap.ofList

        let bla = (m.Elements(xname "SubGroups")).Elements(xname "MeasurementGroup") |> List.ofSeq

        let subGroupsNFlat = bla |> List.map (fun x -> getGroups x)
        
        let flat' = 
            subGroupsNFlat 
              |> List.map (fun (_,x) -> x)             
              |> List.fold (fun acc x -> HMap.union acc x) flat'

        let g = {
                key = Guid.NewGuid()
                name = name
                leaves = annotations |> Seq.map(fun x -> x.id) |> PList.ofSeq
                subNodes = subGroupsNFlat |> Seq.map(fun (x,_) -> x) |> PList.ofSeq
                visible = visible
                expanded = true
                }
        (g, flat')

    type XmlReader with
    /// Returns a lazy sequence of XElements matching a given name.
        member reader.StreamElements(name, ?namespaceURI) =
            let readOp =
                match namespaceURI with
                | None    -> fun () -> reader.ReadToFollowing(name)
                | Some ns -> fun () -> reader.ReadToFollowing(name, ns)
            seq {
                while readOp() do
                    match XElement.ReadFrom reader with
                    | :? XElement as el -> yield el
                    | _ -> ()
            }

    let startImporter (path:string) =
        let reader = XmlReader.Create path
        let root = reader.StreamElements("MeasurementGroups") 
        let xGroups = (root.Elements(xname "MeasurementGroup")).ToListOfT<XElement>()
        printfn "%A" xGroups.Count
        let groupsNFlat = xGroups |> Seq.map (fun x -> getGroups x)

        let flat = 
            groupsNFlat 
              |> Seq.map (fun (_,x) -> x) 
              |> Seq.toList 
              |> List.fold (fun acc x -> HMap.union acc x) HMap.empty

        let groups = 
            groupsNFlat |> Seq.map (fun (x,_) -> x) |> PList.ofSeq  

        groups, flat


    //type Action =
    //    | ImportGroups    of string
        
    //let update (model : AnnotationGroupsImporterModel) (act : Action) =
    //    match act with
    //        | ImportGroups s         -> 
    //            let groups, flat = startImporter s
    //            { model with rootGroupI = groups ; flatI = flat  }

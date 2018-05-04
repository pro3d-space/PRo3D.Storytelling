namespace PRo3D

module AnnotationGroupsImporter = 
    open System
    open System.Xml.Linq
    open System.Xml

    open Aardvark.Base
    open Aardvark.UI
    open PRo3D.Groups
    open PRo3D.ReferenceSystem

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

    let rec getGroups up north (m:XElement) : (Node * hmap<Guid,Leaf> * hmap<Guid,string>) = 
      let name    = (m.Element(xname "Name")).Value.ToString()
      let visible = (m.Element(xname "IsVisible")).Value.ToBool()
                         
      let annotations = 
        (m.Elements(xname "Measurements").Elements(xname "object")) 
          |> List.ofSeq
          |> List.map MeasurementsImporter.getAnnotation
          |> List.map(fun x -> 
              let dns = 
                x.points 
                  |> DipAndStrike.calculateDipAndStrikeResults (up) (north)

              let results = 
                Annotation.calculateAnnotationResults x up

              { x with dnsResults = dns; results = Some results} )
          |> List.map Leaf.Annotations

      let flat' = 
        annotations 
          |> List.map(fun x -> x.id, x) 
          |> HMap.ofList
      
      let subGroupsNFlatNLookup = 
        (m.Elements(xname "SubGroups")).Elements(xname "MeasurementGroup") 
          |> List.ofSeq 
          |> List.map(fun x -> getGroups up north x)     
      
      let flat' = 
        subGroupsNFlatNLookup 
          |> List.map (fun (_,x,_) -> x)             
          |> List.fold (fun acc x -> HMap.union acc x) flat'

      let nodes = 
        subGroupsNFlatNLookup 
          |> List.map (fun (x,_,_) -> x)             
          |> PList.ofList

      // collect lookups from subnodes
      let lookUp = 
        subGroupsNFlatNLookup 
          |> List.map (fun (_,_,x) -> x)             
          |> List.fold (fun acc x -> HMap.union acc x) HMap.empty

      // add current annotations to lookup
      let lookUp' =
        annotations 
          |> List.map(fun x -> HMap.add x.id name HMap.empty)
          |> List.fold(fun a b -> HMap.union a b ) lookUp

      let g = 
        {
          key      = Guid.NewGuid()
          name     = name
          leaves   = annotations |> List.map(fun x -> x.id) |> PList.ofList
          subNodes = nodes
          visible  = visible
          expanded = true
        }

      (g, flat', lookUp')

    type XmlReader with
    /// Returns a lazy sequence of XElements matching a given name to start from
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

    let startImporter (path:string) (refSys:ReferenceSystem) =
        let reader = XmlReader.Create path
        let root = reader.StreamElements("MeasurementGroups") 
        let xGroups = (root.Elements(xname "MeasurementGroup")).ToListOfT<XElement>()
        printfn "%A" xGroups.Count
        let groupsNFlat = 
          xGroups 
            |> Seq.toList
            |> List.map(getGroups refSys.up.value refSys.north.value)

        let flat = 
          groupsNFlat 
            |> List.map (fun (_,x,_) -> x)
            |> List.fold (fun acc x -> HMap.union acc x) HMap.empty

        let groups = 
          groupsNFlat |> List.map (fun (x,_,_) -> x) |> PList.ofList

        
        let lookup = 
          groupsNFlat 
            |> List.map (fun (_,_,x) -> x)
            |> List.fold (fun acc x -> HMap.union acc x) HMap.empty        

        groups, flat, lookup


    //type Action =
    //    | ImportGroups    of string
        
    //let update (model : AnnotationGroupsImporterModel) (act : Action) =
    //    match act with
    //        | ImportGroups s         -> 
    //            let groups, flat = startImporter s
    //            { model with rootGroupI = groups ; flatI = flat  }

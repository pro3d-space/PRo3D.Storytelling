namespace PRo3D

module AnnotationGroupsImporter = 
    open System
    open System.Xml.Linq
    open System.Xml

    open Aardvark.Base
    open Aardvark.UI
    open PRo3D.Groups

    let xname s = XName.Get(s)

    let rec getGroups (m:XElement) = 
        let name = (m.Element(xname "Name")).Value.ToString()
        let visible = (m.Element(xname "IsVisible")).Value.ToBool()
                           
        let annotations = (m.Elements(xname "Measurements").Elements(xname "object")) 
                            |> Seq.map (fun x -> MeasurementsImporter.getAnnotation x)
                            |> PList.ofSeq  
                            |> PList.map(fun x -> Child.Annotations x)

        let subGroups = (m.Elements(xname "SubGroups")).Elements(xname "MeasurementGroup")
                            |> Seq.map (fun x -> getGroups x)
                            |> PList.ofSeq
        
        let g = {
                key = Guid.NewGuid()
                name = name
                childList = failwith "annotations"
                subGroups = subGroups
                visible = visible
                expanded = true
                }
        g

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
        let groups = (root.Elements(xname "MeasurementGroup")) 
                        |> Seq.map (fun x -> getGroups x)
                        |> PList.ofSeq  
        groups


    type Action =
        | ImportGroups    of string
        
    let update (model : AnnotationGroupsImporterModel) (act : Action) =
        match act with
            | ImportGroups s         -> { model with rootGroupI = startImporter s }

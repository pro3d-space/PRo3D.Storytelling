namespace PRo3D

module MeasurementsImporter = 
    open System
    open System.Xml.Linq
    open System.Xml

    open Aardvark.Base
    open Aardvark.UI

    let xname s = XName.Get(s)

    let thickness (a:XElement) = {
        value = (float)(a.Element(xname "object").Element(xname "LineThickness").Value)
        min = 0.5
        max = 10.0
        step = 0.1
        format = ""
        }

    let getStyle (a:XElement) = {
        color = C4b.Parse(a.Element(xname "object").Element(xname "Color").Value)
        thickness = thickness a
        }

    let getTrafo (t:XElement) = 
        let strafo = t.Value.NestedBracketSplitLevelOne()
                        |> Seq.map(fun x ->  M44d.Parse(x))
                        |> List.ofSeq
        let trafo = Trafo3d(strafo.[0], strafo.[1])
        trafo

    let parsePointsLevelOne (points:XElement) = points.Value.NestedBracketSplitLevelOne()
                                                    |> Seq.map (fun x -> V3d.Parse(x))
                                                    |> PList.ofSeq
      

    let parsePointsLevelZero (points:XElement) = points.Value.NestedBracketSplit(0) 
                                                    |> Seq.map (fun x -> V3d.Parse(x))
                                                    |> PList.ofSeq  
       
    
    let getPoints (points:XElement, aType:string) =
        let l = match points with
                    | null -> PList.empty 
                    | _-> match aType with
                            | "Exomars.Base.Geology.Point" ->  PList.ofList [V3d.Parse(points.Value)]
                            | "Exomars.Base.Geology.Line"  ->  parsePointsLevelOne points
                            | _ -> parsePointsLevelZero points
        l
      
                        
    let parseSegments (segments:XElement) = segments.Elements(xname "V3d_Array")
                                                |> Seq.map (fun x -> parsePointsLevelZero x)
                                                |> PList.ofSeq

    let parseSegment (seg:XElement) =  PList.ofList [parsePointsLevelZero seg]
        

    let getSegments (m:XElement, aType:string) = 
        let segments = match aType with
                        |"Exomars.Base.Geology.Line"            -> let segment = m.Element(xname "Segment")
                                                                   match segment.FirstAttribute with
                                                                    | null -> PList.empty
                                                                    | _ -> parseSegment segment
                        |"Exomars.Base.Geology.Polyline"   
                        |"Exomars.Base.Geology.DipAndStrike"    -> let segments = m.Element(xname "Segments")
                                                                   match segments with
                                                                    | null -> PList.empty
                                                                    | _ -> parseSegments segments
                        |_ -> PList.empty
        segments

    let getGeometry (aType:string, closed:bool) = 
        let geometry = if closed then Geometry.Polygon
                        else match aType with
                                |"Exomars.Base.Geology.Point"           -> Geometry.Point
                                |"Exomars.Base.Geology.Line"            -> Geometry.Line
                                |"Exomars.Base.Geology.Polyline"        -> Geometry.Polyline
                                |"Exomars.Base.Geology.DipAndStrike"    -> Geometry.DnS
                                |_ -> Geometry.Point
        geometry

    let getAnnotation (m:XElement) = 
        let anType = (m.Attribute(xname "type").Value.ToString().Split ',').[0]
        let closed = match m.Element(xname "Closed") with
                        | null -> false
                        | _ -> m.Element(xname "Closed").Value.ToBool()
        let style = (getStyle (m.Element(xname "LineAttributes")))
        let trafo = getTrafo (m.Element(xname "LocalToGlobal"))
        let an = {
                key = Guid.NewGuid()
                geometry = (getGeometry (anType, closed)) 
                projection = Projection.Linear
                semantic = Semantic.Horizon0
                points = (getPoints (m.Element(xname "Points"), anType))
                segments = (getSegments (m, anType))
                color = style.color
                thickness = style.thickness
                dnsResults = None
                modelTrafo = trafo
                visible = true 
                text = ""
        }
        an
        

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
        let measurements = reader.StreamElements("Measurements").Elements(xname "object")
        let annotations = measurements 
                            |> Seq.map (fun x -> getAnnotation x)
                            |> PList.ofSeq  
        annotations


    type Action =
        | Import    of string
        
    let update (model : MeasurementsImporterModel) (act : Action) =
        match act with
            | Import s         -> { model with annotations = startImporter s }
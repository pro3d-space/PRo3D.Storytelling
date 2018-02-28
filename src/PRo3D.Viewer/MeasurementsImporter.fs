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

    let textsize  = {
        value = 0.05
        min = 0.01
        max = 5.0
        step = 0.01
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


    let parsePointsLevelOne (points:XElement) = 
        points.Value.NestedBracketSplitLevelOne()
            |> Seq.map (fun x -> V3d.Parse(x))
            |> Seq.toArray
      

    let parsePointsLevelZero (points:XElement) = 
        points.Value.NestedBracketSplit(0) 
            |> Seq.map (fun x -> V3d.Parse(x))
            |> Seq.toArray
       
    
    let getPoints (points:XElement) (aType:string) =
        match points with
            | null -> [||]
            | _-> match aType with
                    | "Exomars.Base.Geology.Point" ->  [| V3d.Parse(points.Value) |]
                    | "Exomars.Base.Geology.Line"  ->  parsePointsLevelOne points
                    | _ -> parsePointsLevelZero points
      
    let segmentOfArray (points : array<_>) = 
        if points.Length > 0 then 
            Some { startPoint = points.[0]; endPoint = points.[points.Length - 1]; points = PList.ofArray points }
        else None
                        
    let parseSegments (segments:XElement) = 
        segments.Elements(xname "V3d_Array")
            |> Seq.map (fun x -> x |> parsePointsLevelZero |> segmentOfArray)
            |> Seq.toList

    let parseSegment (seg:XElement) = 
        parsePointsLevelZero seg |> segmentOfArray 
        
    let getSegments (m:XElement, aType:string) = 
        let segments = 
            match aType with
                |"Exomars.Base.Geology.Line"  -> 
                    let segment = m.Element(xname "Segment")
                    match segment.FirstAttribute with
                        | null -> []
                        | _ -> [parseSegment segment]
                | "Exomars.Base.Geology.Polyline"  | "Exomars.Base.Geology.DipAndStrike" -> 
                    let segments = m.Element(xname "Segments")
                    match segments with
                        | null -> []
                        | _ -> parseSegments segments
                | _ -> []
        List.choose id segments // drop all empty segments (semgents being None)


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
        let id = (m.Element(xname "Id")).Value |> Guid.Parse
        let trafo = getTrafo (m.Element(xname "LocalToGlobal"))
        let points = 
            getPoints (m.Element(xname "Points")) anType 
                |> PList.ofArray 
                |> PList.map (fun x -> trafo.Forward.TransformPos x)

        let segments = 
            getSegments (m, anType) 
                |> PList.ofList 
                |> PList.map (
                    fun s -> 
                        let ps = s.points |> PList.map (fun x -> trafo.Forward.TransformPos x)
                        let startPoint = s.startPoint |> trafo.Forward.TransformPos 
                        let endPoint = s.endPoint |> trafo.Forward.TransformPos 
                        {s with startPoint = startPoint; endPoint = endPoint; points = ps}
                )

        Log.line "TrafoImporter: Found %A in xml" id

        let an = {
                key = id
                geometry = getGeometry (anType, closed)
                projection = Projection.Linear
                semantic = Semantic.Horizon0
                points = points
                segments = segments
                color = { c = style.color }
                thickness = style.thickness
                results = None
                dnsResults = None
                modelTrafo = trafo
                visible = true 
                showDns = false
                text = ""
                textsize = textsize
                surfaceName = ""
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
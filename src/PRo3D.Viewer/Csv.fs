namespace PRo3D

module Csv =
    
    open System.IO
    open Microsoft.FSharp.Reflection
    open Aardvark.UI
    open Aardvark.Base
    open System

    type ExportDnS = {
        dipAngle    : float
        dipAzimuth  : float
        strikeAzimuth : float
        }
   
    type ExportAnnotation = {
        key         : Guid
        geometry    : PRo3D.Geometry
        projection  : Projection
        semantic    : Semantic
        color       : string
        thickness   : float
        //results     : Option<AnnotationResults>
        height      : float
        heightDelta : float
        length      : float
        wayLength   : float
        //dnsResults  : Option<DipAndStrikeResults>
        dipAngle    : float
        dipAzimuth  : float
        strikeAzimuth : float
        text        : string
        x           : double
        y           : double
        z           : double
    }

    let exportAnnotation (a:Annotation) =
        let results = match a.results with
                        | Some r -> r
                        | _ ->  { height = Double.NaN;
                                  heightDelta = Double.NaN;
                                  length = Double.NaN;
                                  wayLength = Double.NaN;
                                  }
        let dnsResults = match a.dnsResults with
                            | Some x -> { dipAngle = x.dipAngle;
                                          dipAzimuth = x.dipAzimuth;
                                          strikeAzimuth = x.strikeAzimuth;
                                        }
                            | _ -> { dipAngle = Double.NaN;
                                     dipAzimuth = Double.NaN;
                                     strikeAzimuth = Double.NaN;
                                     }
        let points = a.points |> PList.map (fun x -> a.modelTrafo.Forward.TransformPos x )  |> PList.toArray
        let c = (new Box3d(points)).Center
        {   key        =  a.key;
            geometry   = a.geometry;
            projection = a.projection;
            semantic   = a.semantic;
            color      = a.color.ToString();
            thickness  = a.thickness.value;
            //results    = a.results;
            height     = results.height;
            heightDelta = results.heightDelta;
            length      = results.length;
            wayLength   = results.wayLength;
            //dnsResults = a.dnsResults;
            dipAngle    = dnsResults.dipAngle;
            dipAzimuth  = dnsResults.dipAzimuth;
            strikeAzimuth = dnsResults.strikeAzimuth;
            text       = a.text;
            x          = c.X;
            y          = c.Y;
            z          = c.Z;
        }


    let getAnnorationExportData (annotations: list<Annotation>) =
        annotations |> List.map(fun x -> exportAnnotation x)


    // Code from http://www.fssnip.net/3U/title/CSV-writer ----------------------------------------------------------------
    type Array =
        static member join delimiter xs = 
            xs 
            |> Array.map (fun x -> x.ToString())
            |> String.concat delimiter

    type Seq =
        static member write (path:string) (data:seq<'a>): 'result = 
            use writer = new StreamWriter(path)
            data
            |> Seq.iter writer.WriteLine 

        static member csv (separator:string) (useEnclosure:bool) (headerMapping:string -> string) ( data:seq<'a>) =
            seq {
                let dataType = typeof<'a>
                let stringSeqDataType = typeof<System.Collections.Generic.IEnumerable<string>>
                let inline enclose s =
                    match useEnclosure with
                    | true -> "\"" + (string s) + "\""
                    | false -> string s

                let header = 
                    match dataType with
                    | ty when FSharpType.IsRecord ty ->
                        FSharpType.GetRecordFields dataType
                        |> Array.map (fun info -> headerMapping info.Name)                    
                    | ty when FSharpType.IsTuple ty -> 
                        FSharpType.GetTupleElements dataType
                        |> Array.mapi (fun idx info -> headerMapping(string idx) )
                    | ty when ty.IsAssignableFrom stringSeqDataType ->
                        data :?> seq<seq<string>> |> Seq.head
                        |> Seq.toArray
                    | _ -> dataType.GetProperties()
                        |> Array.map (fun info -> headerMapping info.Name)

                yield header |> Array.map enclose |> Array.join separator
                                    
                let lines =
                    match dataType with 
                    | ty when FSharpType.IsRecord ty -> 
                        data |> Seq.map FSharpValue.GetRecordFields
                    | ty when FSharpType.IsTuple ty ->
                        data |> Seq.map FSharpValue.GetTupleFields
                    | ty when ty.IsAssignableFrom stringSeqDataType ->
                        data :?> seq<seq<string>> |> Seq.tail
                        |> Seq.map (fun ss -> Seq.toArray ss |> Array.map (fun s -> s :> obj) )
                    | _ -> 
                        let props = dataType.GetProperties()
                        data |> Seq.map ( fun line -> 
                                    props |> Array.map ( fun prop ->
                                    prop.GetValue(line, null) ))                                     
                    |> Seq.map (Array.map enclose)
                yield! lines |> Seq.map (Array.join separator)
            }


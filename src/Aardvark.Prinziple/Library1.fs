namespace Aardvark.Prinziple

open System
open System.IO
open System.Xml
open System.Xml.Linq
open Aardvark.Base
open Aardvark.Base.Incremental
open ICSharpCode.SharpZipLib
open ICSharpCode.SharpZipLib.Zip

module Prinziple = 

  let private readAllBytes' (reader : BinaryReader) =

    let bufferSize = 4096;     
    use ms = new MemoryStream()
        
    let buffer = Array.init bufferSize (fun _ -> byte(0))
    let mutable count = 0
    
    while (count <- reader.Read(buffer, 0, buffer.Length); count <> 0) do
        ms.Write(buffer, 0, count);

    ms.ToArray()
    
  //"D:\_WORK\Aardwork\Exomars\_Scenes\_NEWVIEWER\TestScene\Surfaces\Cape_Desire_RGB\OPC_000_000"
  //"hierarchy.cache"
  //check if path contains zipfile (lookup in zipTable)
  //yes:
  //split path in zipfile and entry
  //no:
  //normal file read

  let mutable private zipTable : hmap<string,string> = HMap.empty

  let registerIfZipped dir =
    let dir   = Path.GetFullPath(dir)
    let zPath = Path.ChangeExtension(dir, ".zip")
    if File.Exists zPath then
      zipTable <- HMap.add dir dir zipTable
    
    dir

  let loadBytesFromZip zipPath (entryPath:string) =
    let entryPath = entryPath.Replace("\\","/")
    let file = new ZipFile(File.OpenRead(zipPath))
    let e = file.GetEntry(entryPath)
    use s = file.GetInputStream(e)    
    use r = new BinaryReader(s)
    r |> readAllBytes'

  let loadXmlFromZip zipPath (entryPath : string) =
    let entryPath = entryPath.Replace("\\","/")
    let file = new ZipFile(File.OpenRead(zipPath))
    let e = file.GetEntry(entryPath) //"Patches\\patchhierarchy.xml" {Patches/patchhierarchy.xml}
    use s = file.GetInputStream(e)    
    XDocument.Load(s)

  let splitPath (path : string) =
    
    let p = zipTable |> HMap.filter(fun _ v -> path.StartsWith v) |> HMap.toList |> List.map snd |> List.tryHead
    
    match p with
    | Some zipped -> Some (Path.ChangeExtension(zipped, ".zip"), path.[zipped.Length+1..path.Length-1])
    | None -> None
    
  let readXml path =    
    let split = path |> Path.GetFullPath |> splitPath
    match split with
    | Some(zip,entry) -> loadXmlFromZip zip entry
    | None -> XDocument.Load(path)

  let readXml2 (path:string) =
    let doc   = new XmlDocument() 
    doc.Load path    
    doc
  
  let readAllBytes path =    
    let split = path |> Path.GetFullPath |> splitPath
    match split with
    | Some(zip,entry) -> loadBytesFromZip zip entry
    | None -> File.readAllBytes path
    
  let loadFromZip' path =
    let file = new ZipFile(File.OpenRead(@"D:\_WORK\Aardwork\Exomars\_Scenes\_NEWVIEWER\TestScene\Surfaces\Cape_Desire_RGB\OPC_000_000.zip"))
    let e = file.GetEntry("hierarchy.cache")
    use s = file.GetInputStream(e)
    //s.Seek(2L, SeekOrigin.Begin) |> ignore

    use r = new BinaryReader(s)
    r |> readAllBytes'


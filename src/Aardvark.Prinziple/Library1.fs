namespace Aardvark.Prinziple

open System.IO
open ICSharpCode.SharpZipLib
open ICSharpCode.SharpZipLib.Zip


module Blarg = 

  let register path =
    0

  let loadFromZip path =
    let file = new ZipFile(File.OpenRead(@"D:\Development\template\src\a.zip"))
    let e = file.GetEntry("asdasd/New Text Document.txt")
    use s = file.GetInputStream(e)
    //s.Seek(2L, SeekOrigin.Begin) |> ignore

    use r = new StreamReader(s)
    r.ReadToEnd() |> printfn "%A"    


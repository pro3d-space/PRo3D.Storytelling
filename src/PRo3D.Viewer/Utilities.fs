namespace PRo3D

open System
open System.IO
open System.Windows.Forms

open MBrace.FsPickler

open Aardvark.Base

open PRo3D

module Serialization =
    
    let binarySerializer = FsPickler.CreateBinarySerializer()

    let save path (d : 'a) =    
        let arr = binarySerializer.Pickle d     
        File.WriteAllBytes(path, arr);
        d

    let loadAs<'a> path : 'a =
        let arr = File.ReadAllBytes(path)
        binarySerializer.UnPickle arr

    let writeToFile path (contents : string) =
        System.IO.File.WriteAllText(path, contents)

module Dialogs =
    open Microsoft.WindowsAPICodePack.Dialogs    

    let saveDialog  (w : Form)  =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new SaveFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r
    
    let openDialog (w : Form) =
        let mutable r = Unchecked.defaultof<_>
        w.Invoke(Action(fun _ -> 
            let dialog = new OpenFileDialog()
            if dialog.ShowDialog() = DialogResult.OK then
                r <- Some dialog.FileName
            else r <- None
        )) |> ignore
        r

    let openDialog2 (w : Form)(isFolder : bool)(isMulti : bool) =
        let mutable r = []
        w.Invoke(Action(fun _ -> 
            let dialog = new CommonOpenFileDialog()
            dialog.Multiselect <- isMulti
          //  dialog.InitialDirectory <- "C:\\Users"
            dialog.IsFolderPicker <- isFolder
            if dialog.ShowDialog() = CommonFileDialogResult.Ok then
                r <- dialog.FileNames |> Seq.toList
            else r <- []
        )) |> ignore
        r
      
    let openFolders (w : Form) =
        openDialog2 w true true

    let openFolder (w : Form) =
        openDialog2 w true false

module Box3d =
    let extendBy (box:Box3d) (b:Box3d) =
        box.ExtendBy(b)
        box

    let ofSeq (bs:seq<Box3d>) =
        let box = 
            match bs |> Seq.tryHead with
                | Some b -> b
                | None -> failwith "box sequence must not be empty"
                    
        for b in bs do
            box.ExtendBy(b)
        box

module Mod =
    open Aardvark.Base.Incremental

    let bindOption (m : IMod<Option<'a>>) (defaultValue : 'b) (project : 'a -> IMod<'b>)  : IMod<'b> =
        m |> Mod.bind (function | None   -> Mod.constant defaultValue       
                                | Some v -> project v)

module Copy =
    let rec copyAll' (source : DirectoryInfo) (target : DirectoryInfo) skipExisting =
        
        // Check if the target directory exists, if not, create it.
        if not(Directory.Exists target.FullName) then
            Directory.CreateDirectory target.FullName |> ignore

        // Copy each file into it's new directory.
        for fi in source.GetFiles() do
             let sourceFile = fi.FullName
             let targetFile = Path.Combine(target.FullName, fi.Name)

             if ((sourceFile.ToLower() == targetFile.ToLower()) || (skipExisting && File.Exists(targetFile))) then
                Log.warn "Skipping %s, already exists" targetFile
             else
                Log.line "Copying to %s" targetFile
                fi.CopyTo(Path.Combine((target.ToString()), fi.Name), true) |> ignore      
                
        // Copy each subdirectory using recursion.
        let bla = source.GetDirectories()
        for srcSubDir in bla do
            let nextTgtSubDir = target.CreateSubdirectory(srcSubDir.Name)
            copyAll' srcSubDir nextTgtSubDir skipExisting

    let copyAll source target skipExisting=
        let s = DirectoryInfo(source)
        let t = DirectoryInfo(target)

        copyAll' s t skipExisting
    
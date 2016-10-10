#nowarn "9"
namespace Aardvark.Rendering.GL

open System
open System.IO

open System.Runtime.InteropServices
open Aardvark.Base
open Aardvark.Rendering.GL
open OpenTK.Graphics.OpenGL4
open FSharp.NativeInterop

module private NativeSupport =
    [<DllImport("kernel32.dll", SetLastError=true)>]
    extern bool ReadFile(Microsoft.Win32.SafeHandles.SafeFileHandle handle, IntPtr buffer, uint32 numBytesToRead, int* numBytesRead, IntPtr overlapped);

    let ReadFile2 (file : string) =
        let info = FileInfo(file)
        let f = File.OpenRead file

        let mutable ptr = Marshal.AllocHGlobal(int info.Length)
        let mutable remaining = int info.Length
        let buffer = Array.zeroCreate<byte> (1 <<< 22)
        while remaining > 0 do
            let s = f.Read(buffer, 0, buffer.Length)
            Marshal.Copy(buffer, 0, ptr, s)
            ptr <- ptr + nativeint s
            remaining <- remaining - s
        ptr

module CompressedImage =

    let loadAndCache (runtime : IRuntime) (fullPath : string) = 

        let config = { wantMipMaps = true; wantSrgb = false; wantCompressed = true }
        let sourceTex = FileTexture(fullPath, config) :> ITexture
        let prepared = runtime.PrepareTexture(sourceTex)
        let handle = prepared.Handle |> unbox<int>

        Log.startTimed "caching image of size: %A" prepared.Size

        GL.BindTexture(TextureTarget.Texture2D, handle)

        let ifmt = unbox<PixelInternalFormat> prepared.Format
        let log2 a = log a / log 2.0
        let mipLevels = 
            if config.wantMipMaps then 
                int ( 1.0 + floor (log2 ( max (float prepared.Size.X) (float prepared.Size.Y))) ) 
             else 1
//            let mutable autoGen = 0
//            GL.GetTexParameterI(TextureTarget.Texture2D, GetTextureParameter.GenerateMipmap, &autoGen)
//            if autoGen = 1 then
//                int ( 1.0 + floor (log ( max (float prepared.Size.X) (float prepared.Size.Y))) )
//            else
//                let mutable mipLevels = 0
//                GL.GetTexParameterI(TextureTarget.Texture2D, OpenTK.Graphics.OpenGL4.All.TextureImmutableLevels |> unbox, &mipLevels)
//                max 1 mipLevels

        let mipSizes = 
            [ 0 .. mipLevels - 1 ] 
                |> List.map (fun l -> 
                    let mutable size = 0
                    GL.GetTexLevelParameter(TextureTarget.Texture2D,l,GetTextureParameter.TextureCompressedImageSize,&size)
                    size
                )


        let totalSize = List.sum mipSizes
        let originalName = Path.GetFileName(fullPath)
        let cacheFileName = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cache" originalName)
        let cacheFileInfo = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cacheInfo" originalName)
                                    
        use infoFile = new System.IO.BinaryWriter(File.OpenWrite(cacheFileInfo))
        infoFile.Write(11)
        infoFile.Write(totalSize)
        infoFile.Write(prepared.Size.X)
        infoFile.Write(prepared.Size.Y)
        infoFile.Write(ifmt |> int)
        infoFile.Write(mipLevels)

        let basePtr = Marshal.AllocHGlobal(totalSize)
        let mutable ptr = basePtr
        let mutable level = 0
        let mutable offset = 0
        for size in mipSizes do
            infoFile.Write(offset)
            infoFile.Write(size)
            let mutable w = 0
            let mutable h = 0
            GL.GetTexLevelParameter(TextureTarget.Texture2D, level, GetTextureParameter.TextureWidth, &w)
            GL.GetTexLevelParameter(TextureTarget.Texture2D, level, GetTextureParameter.TextureHeight, &h)
            infoFile.Write(w)
            infoFile.Write(h)
            GL.Check "get size"
            GL.GetCompressedTexImage(TextureTarget.Texture2D, level, ptr)
            GL.Check "get compressed"
            ptr <- ptr + nativeint size
            offset <- offset + size
            level <- level + 1
        GL.Check "download compressed image failed"
        GL.BindTexture(TextureTarget.Texture2D, 0)

        use cacheFileStream = File.OpenWrite(cacheFileName)
        use ustream = new UnmanagedMemoryStream(basePtr |> NativePtr.ofNativeInt, int64 totalSize)
        ustream.CopyTo(cacheFileStream)
        cacheFileStream.Close()
        infoFile.Close()
        Marshal.FreeHGlobal(basePtr)
        GL.Sync()
        Log.stop()
        prepared :> ITexture, { new IDisposable with member x.Dispose() = GL.DeleteTexture(handle) }

    let loadFromCache (runtime : IRuntime) (cacheFileName : string) (infoFileName : string) (config : TextureParams) =

        if File.Exists infoFileName then
            use infoFile = new BinaryReader(File.OpenRead infoFileName)

            let version = infoFile.ReadInt32()
            if version = 11 then
                let mutable totalSize = infoFile.ReadInt32()
                let width = infoFile.ReadInt32()
                let height = infoFile.ReadInt32()
                let format = infoFile.ReadInt32() |> unbox<PixelInternalFormat>
                let mips = infoFile.ReadInt32() 
                let offsets = [ for i in 0 .. mips - 1 do yield infoFile.ReadInt32(), infoFile.ReadInt32(), infoFile.ReadInt32(), infoFile.ReadInt32() ]
                infoFile.Close()


                use cacheStream = File.OpenRead(cacheFileName)
                let sourcePtr = Marshal.AllocHGlobal totalSize
                let mutable bytesRead = NativePtr.alloc 4 
                let ok = NativeSupport.ReadFile(cacheStream.SafeFileHandle,sourcePtr,uint32 totalSize,bytesRead,IntPtr.Zero)
                let read = NativePtr.read bytesRead 
                if read <> totalSize then failwith ""

                let runtime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>

                let handle = GL.GenTexture()
                GL.BindTexture(TextureTarget.Texture2D, handle)
                GL.TexParameterI(TextureTarget.Texture2D,TextureParameterName.TextureMaxLevel, [| mips - 1 |] )
                //GL.TexStorage2D(TextureTarget2d.Texture2D, mips, unbox<_> (int format), width, height)
                let mutable level = 0
                let mutable width = width
                let mutable height = height
                for offset,size,w,h in offsets do
                    let pbo = GL.GenBuffer()
                    GL.BindBuffer(BufferTarget.PixelUnpackBuffer, pbo)
                    GL.BufferStorage(BufferTarget.PixelUnpackBuffer, nativeint size, 0n, BufferStorageFlags.MapWriteBit)
                    let ptr = GL.MapBuffer(BufferTarget.PixelUnpackBuffer, BufferAccess.WriteOnly)
                    GL.Check "map buffer"
                    let actualSource = nativeint offset + sourcePtr

                    Marshal.Copy(actualSource, ptr, size)
                    GL.UnmapBuffer(BufferTarget.PixelUnpackBuffer) |> ignore

                    GL.Check "copied to buffer"

//                    if level = 0 then 
                    GL.CompressedTexImage2D(TextureTarget.Texture2D, level, format, w, h, 0, size, 0n)
//                    else
//                    let te = unbox<TextureFormat> (int format)
//                    let imageFormat = TextureExtensions.toUntypedPixelFormat te
////                        let blubber = unbox<_> (int format)
//                    GL.CompressedTexSubImage2D(TextureTarget.Texture2D, level, 0, 0, width, height, imageFormat.Value, size, 0n)
                    GL.Check (sprintf "compressedTexImage: %s, level: %d" cacheFileName level)
                    GL.BindBuffer(BufferTarget.PixelUnpackBuffer, 0)
                    GL.DeleteBuffer(pbo)
                    level <- level + 1
                    width <- width / 2
                    height <- height / 2
                GL.BindTexture(TextureTarget.Texture2D, 0)

                Marshal.FreeHGlobal sourcePtr

                let dispose = 
                    { new IDisposable with
                        member x.Dispose() =
                            use ctx = (runtime :> IRuntime).ContextLock
                            Aardvark.Rendering.GL.TextureExtensions.removeTexture runtime.Context (int64 totalSize)
                            GL.DeleteTexture(handle)
                    }

                let glRuntime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>

                let t = 
                    Texture(glRuntime.Context, handle, TextureDimension.Texture2D, mips,
                            1, V3i(width,height,1), 1, format |> unbox<TextureFormat>,
                            int64 totalSize, false)

                Aardvark.Rendering.GL.TextureExtensions.addTexture runtime.Context (int64 totalSize)

                Some ( t :> ITexture, dispose )
            else
                infoFile.Close()
                None
        else None

    let hasCache (originalTexture : string) =
        let cacheFileInfo = Path.Combine(Path.GetDirectoryName(originalTexture), sprintf "%s.cacheInfo" originalTexture)
        let cacheFileName = Path.Combine(Path.GetDirectoryName(originalTexture), sprintf "%s.cache" originalTexture)
        if File.Exists cacheFileInfo && File.Exists cacheFileName then
            use infoFile = new BinaryReader(File.OpenRead cacheFileInfo)

            let version = infoFile.ReadInt32()
            if version = 11 then
                true
            else false
        else false

    let ensureCache (runtime : IRuntime) (originalTexture : string) =
        if hasCache originalTexture then 
            ()
        else 
            let _,d = loadAndCache runtime originalTexture
            d.Dispose()

    let load (runtime : IRuntime) (fullPath : string) (config : TextureParams) (overrideCache : bool) =
        let cacheFileInfo = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cacheInfo" fullPath)
        let cacheFileName = Path.Combine(Path.GetDirectoryName(fullPath), sprintf "%s.cache" fullPath)

        if overrideCache then loadAndCache runtime fullPath 
        else
            match loadFromCache runtime cacheFileName cacheFileInfo config with
                | Some v -> v
                | None -> loadAndCache runtime fullPath 
                
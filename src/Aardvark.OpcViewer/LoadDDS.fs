namespace BLA
//
//                                    Examples.TextureLoaders.ImageDDS.LoadFromDisk(fullPath,&textureHandle,&dimension)
//                                    GL.Check "Load IL Image"
//                                    GL.BindTexture(TextureTarget.Texture2D, textureHandle)
//                                    GL.Check "Load IL Image"
//                                    let mutable width = 0
//                                    let mutable height = 0
//                                    let mutable ifmt = 0
//                                    let mutable maxLevel = 0
//                                    GL.GetTexLevelParameter(TextureTarget.Texture2D, 0, GetTextureParameter.TextureWidth, &width)
//                                    GL.GetTexLevelParameter(TextureTarget.Texture2D, 0, GetTextureParameter.TextureHeight, &height)
//                                    GL.Check "Get Image Size"
//                                    GL.GetTexParameterI(TextureTarget.Texture2D, GetTextureParameter.TextureInternalFormat, &ifmt)
//                                    GL.GetTexParameterI(TextureTarget.Texture2D, GetTextureParameter.TextureMaxLevel, &maxLevel)
//                                    GL.Check "Get Image properties"
//
//                                    let size = V3i(width, height, 1)
//                                    let levels = maxLevel + 1
//                                    let internalFormat = unbox<PixelInternalFormat> ifmt
//                                    let fmt = unbox<TextureFormat> ifmt
//
//                                    let sizeInBytes = int64 <| ((InternalFormat.getSizeInBits internalFormat) * size.X * size.Y) / 8
//                                    let sizeInBytes =  if createMipMaps then (sizeInBytes * 3L) / 2L else sizeInBytes
//
//                                    if createMipMaps && levels <= 1 then
//                                        GL.GenerateMipmap(GenerateMipmapTarget.Texture2D)
//
//                                    GL.BindTexture(TextureTarget.Texture2D, 0)
//                                    let glRuntime = runtime |> unbox<Aardvark.Rendering.GL.Runtime>
//                                    
//
//                                    let dispose = 
//                                        { new IDisposable with
//                                            member x.Dispose() =
//                                               use ctx = runtime.ContextLock
//                                               GL.DeleteTexture(textureHandle)
//                                        }
//
//                                    let t = Texture(
//                                                glRuntime.Context,
//                                                int textureHandle,
//                                                TextureDimension.Texture2D,
//                                                levels,
//                                                1,
//                                                size,
//                                                1,
//                                                fmt,
//                                                sizeInBytes,
//                                                false)
//                                    t :> ITexture, dispose
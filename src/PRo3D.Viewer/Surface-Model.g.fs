namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open PRo3D.Surfaces

[<AutoOpen>]
module Mutable =

    
    
    type MScalarLayer(__initial : PRo3D.Surfaces.ScalarLayer) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.ScalarLayer> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.ScalarLayer>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.ScalarLayer>
        let _label = ResetMod.Create(__initial.label)
        let _actualRange = ResetMod.Create(__initial.actualRange)
        let _definedRange = ResetMod.Create(__initial.definedRange)
        let _index = ResetMod.Create(__initial.index)
        let _colorLegend = PRo3D.Mutable.MFalseColorsModel.Create(__initial.colorLegend)
        
        member x.label = _label :> IMod<_>
        member x.actualRange = _actualRange :> IMod<_>
        member x.definedRange = _definedRange :> IMod<_>
        member x.index = _index :> IMod<_>
        member x.colorLegend = _colorLegend
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.ScalarLayer) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_label,v.label)
                ResetMod.Update(_actualRange,v.actualRange)
                ResetMod.Update(_definedRange,v.definedRange)
                ResetMod.Update(_index,v.index)
                PRo3D.Mutable.MFalseColorsModel.Update(_colorLegend, v.colorLegend)
                
        
        static member Create(__initial : PRo3D.Surfaces.ScalarLayer) : MScalarLayer = MScalarLayer(__initial)
        static member Update(m : MScalarLayer, v : PRo3D.Surfaces.ScalarLayer) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.ScalarLayer> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ScalarLayer =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let label =
                { new Lens<PRo3D.Surfaces.ScalarLayer, System.String>() with
                    override x.Get(r) = r.label
                    override x.Set(r,v) = { r with label = v }
                    override x.Update(r,f) = { r with label = f r.label }
                }
            let actualRange =
                { new Lens<PRo3D.Surfaces.ScalarLayer, Aardvark.Base.Range1d>() with
                    override x.Get(r) = r.actualRange
                    override x.Set(r,v) = { r with actualRange = v }
                    override x.Update(r,f) = { r with actualRange = f r.actualRange }
                }
            let definedRange =
                { new Lens<PRo3D.Surfaces.ScalarLayer, Aardvark.Base.Range1d>() with
                    override x.Get(r) = r.definedRange
                    override x.Set(r,v) = { r with definedRange = v }
                    override x.Update(r,f) = { r with definedRange = f r.definedRange }
                }
            let index =
                { new Lens<PRo3D.Surfaces.ScalarLayer, System.Int32>() with
                    override x.Get(r) = r.index
                    override x.Set(r,v) = { r with index = v }
                    override x.Update(r,f) = { r with index = f r.index }
                }
            let colorLegend =
                { new Lens<PRo3D.Surfaces.ScalarLayer, PRo3D.FalseColorsModel>() with
                    override x.Get(r) = r.colorLegend
                    override x.Set(r,v) = { r with colorLegend = v }
                    override x.Update(r,f) = { r with colorLegend = f r.colorLegend }
                }
    
    
    type MColorCorrection(__initial : PRo3D.Surfaces.ColorCorrection) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.ColorCorrection> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.ColorCorrection>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.ColorCorrection>
        let _contrast = Aardvark.UI.Mutable.MNumericInput.Create(__initial.contrast)
        let _useContrast = ResetMod.Create(__initial.useContrast)
        let _brightness = Aardvark.UI.Mutable.MNumericInput.Create(__initial.brightness)
        let _useBrightn = ResetMod.Create(__initial.useBrightn)
        let _gamma = Aardvark.UI.Mutable.MNumericInput.Create(__initial.gamma)
        let _useGamma = ResetMod.Create(__initial.useGamma)
        let _color = Aardvark.UI.Mutable.MColorInput.Create(__initial.color)
        let _useColor = ResetMod.Create(__initial.useColor)
        let _useGrayscale = ResetMod.Create(__initial.useGrayscale)
        
        member x.contrast = _contrast
        member x.useContrast = _useContrast :> IMod<_>
        member x.brightness = _brightness
        member x.useBrightn = _useBrightn :> IMod<_>
        member x.gamma = _gamma
        member x.useGamma = _useGamma :> IMod<_>
        member x.color = _color
        member x.useColor = _useColor :> IMod<_>
        member x.useGrayscale = _useGrayscale :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.ColorCorrection) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Mutable.MNumericInput.Update(_contrast, v.contrast)
                ResetMod.Update(_useContrast,v.useContrast)
                Aardvark.UI.Mutable.MNumericInput.Update(_brightness, v.brightness)
                ResetMod.Update(_useBrightn,v.useBrightn)
                Aardvark.UI.Mutable.MNumericInput.Update(_gamma, v.gamma)
                ResetMod.Update(_useGamma,v.useGamma)
                Aardvark.UI.Mutable.MColorInput.Update(_color, v.color)
                ResetMod.Update(_useColor,v.useColor)
                ResetMod.Update(_useGrayscale,v.useGrayscale)
                
        
        static member Create(__initial : PRo3D.Surfaces.ColorCorrection) : MColorCorrection = MColorCorrection(__initial)
        static member Update(m : MColorCorrection, v : PRo3D.Surfaces.ColorCorrection) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.ColorCorrection> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ColorCorrection =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let contrast =
                { new Lens<PRo3D.Surfaces.ColorCorrection, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.contrast
                    override x.Set(r,v) = { r with contrast = v }
                    override x.Update(r,f) = { r with contrast = f r.contrast }
                }
            let useContrast =
                { new Lens<PRo3D.Surfaces.ColorCorrection, System.Boolean>() with
                    override x.Get(r) = r.useContrast
                    override x.Set(r,v) = { r with useContrast = v }
                    override x.Update(r,f) = { r with useContrast = f r.useContrast }
                }
            let brightness =
                { new Lens<PRo3D.Surfaces.ColorCorrection, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.brightness
                    override x.Set(r,v) = { r with brightness = v }
                    override x.Update(r,f) = { r with brightness = f r.brightness }
                }
            let useBrightn =
                { new Lens<PRo3D.Surfaces.ColorCorrection, System.Boolean>() with
                    override x.Get(r) = r.useBrightn
                    override x.Set(r,v) = { r with useBrightn = v }
                    override x.Update(r,f) = { r with useBrightn = f r.useBrightn }
                }
            let gamma =
                { new Lens<PRo3D.Surfaces.ColorCorrection, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.gamma
                    override x.Set(r,v) = { r with gamma = v }
                    override x.Update(r,f) = { r with gamma = f r.gamma }
                }
            let useGamma =
                { new Lens<PRo3D.Surfaces.ColorCorrection, System.Boolean>() with
                    override x.Get(r) = r.useGamma
                    override x.Set(r,v) = { r with useGamma = v }
                    override x.Update(r,f) = { r with useGamma = f r.useGamma }
                }
            let color =
                { new Lens<PRo3D.Surfaces.ColorCorrection, Aardvark.UI.ColorInput>() with
                    override x.Get(r) = r.color
                    override x.Set(r,v) = { r with color = v }
                    override x.Update(r,f) = { r with color = f r.color }
                }
            let useColor =
                { new Lens<PRo3D.Surfaces.ColorCorrection, System.Boolean>() with
                    override x.Get(r) = r.useColor
                    override x.Set(r,v) = { r with useColor = v }
                    override x.Update(r,f) = { r with useColor = f r.useColor }
                }
            let useGrayscale =
                { new Lens<PRo3D.Surfaces.ColorCorrection, System.Boolean>() with
                    override x.Get(r) = r.useGrayscale
                    override x.Set(r,v) = { r with useGrayscale = v }
                    override x.Update(r,f) = { r with useGrayscale = f r.useGrayscale }
                }
    
    
    type MTransformations(__initial : PRo3D.Surfaces.Transformations) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Transformations> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.Transformations>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Transformations>
        let _useTranslationArrows = ResetMod.Create(__initial.useTranslationArrows)
        let _translation = Aardvark.UI.Mutable.MV3dInput.Create(__initial.translation)
        let _trafo = ResetMod.Create(__initial.trafo)
        
        member x.useTranslationArrows = _useTranslationArrows :> IMod<_>
        member x.translation = _translation
        member x.trafo = _trafo :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.Transformations) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_useTranslationArrows,v.useTranslationArrows)
                Aardvark.UI.Mutable.MV3dInput.Update(_translation, v.translation)
                ResetMod.Update(_trafo,v.trafo)
                
        
        static member Create(__initial : PRo3D.Surfaces.Transformations) : MTransformations = MTransformations(__initial)
        static member Update(m : MTransformations, v : PRo3D.Surfaces.Transformations) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.Transformations> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Transformations =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let useTranslationArrows =
                { new Lens<PRo3D.Surfaces.Transformations, System.Boolean>() with
                    override x.Get(r) = r.useTranslationArrows
                    override x.Set(r,v) = { r with useTranslationArrows = v }
                    override x.Update(r,f) = { r with useTranslationArrows = f r.useTranslationArrows }
                }
            let translation =
                { new Lens<PRo3D.Surfaces.Transformations, Aardvark.UI.V3dInput>() with
                    override x.Get(r) = r.translation
                    override x.Set(r,v) = { r with translation = v }
                    override x.Update(r,f) = { r with translation = f r.translation }
                }
            let trafo =
                { new Lens<PRo3D.Surfaces.Transformations, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
                }
    
    
    type MSurface(__initial : PRo3D.Surfaces.Surface) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Surface> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.Surface>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.Surface>
        let _guid = ResetMod.Create(__initial.guid)
        let _name = ResetMod.Create(__initial.name)
        let _importPath = ResetMod.Create(__initial.importPath)
        let _opcNames = ResetMod.Create(__initial.opcNames)
        let _opcPaths = ResetMod.Create(__initial.opcPaths)
        let _relativePaths = ResetMod.Create(__initial.relativePaths)
        let _fillMode = ResetMod.Create(__initial.fillMode)
        let _cullMode = ResetMod.Create(__initial.cullMode)
        let _isVisible = ResetMod.Create(__initial.isVisible)
        let _isActive = ResetMod.Create(__initial.isActive)
        let _quality = Aardvark.UI.Mutable.MNumericInput.Create(__initial.quality)
        let _priority = Aardvark.UI.Mutable.MNumericInput.Create(__initial.priority)
        let _triangleSize = Aardvark.UI.Mutable.MNumericInput.Create(__initial.triangleSize)
        let _scaling = Aardvark.UI.Mutable.MNumericInput.Create(__initial.scaling)
        let _preTransform = ResetMod.Create(__initial.preTransform)
        let _scalarLayers = MMap.Create(__initial.scalarLayers, (fun v -> MScalarLayer.Create(v)), (fun (m,v) -> MScalarLayer.Update(m, v)), (fun v -> v))
        let _selectedScalar = MOption.Create(__initial.selectedScalar, (fun v -> MScalarLayer.Create(v)), (fun (m,v) -> MScalarLayer.Update(m, v)), (fun v -> v))
        let _textureLayers = MList.Create(__initial.textureLayers)
        let _selectedTexture = MOption.Create(__initial.selectedTexture)
        let _surfaceType = ResetMod.Create(__initial.surfaceType)
        let _colorCorrection = MColorCorrection.Create(__initial.colorCorrection)
        let _homePosition = MOption.Create(__initial.homePosition)
        
        member x.guid = _guid :> IMod<_>
        member x.name = _name :> IMod<_>
        member x.importPath = _importPath :> IMod<_>
        member x.opcNames = _opcNames :> IMod<_>
        member x.opcPaths = _opcPaths :> IMod<_>
        member x.relativePaths = _relativePaths :> IMod<_>
        member x.fillMode = _fillMode :> IMod<_>
        member x.cullMode = _cullMode :> IMod<_>
        member x.isVisible = _isVisible :> IMod<_>
        member x.isActive = _isActive :> IMod<_>
        member x.quality = _quality
        member x.priority = _priority
        member x.triangleSize = _triangleSize
        member x.scaling = _scaling
        member x.preTransform = _preTransform :> IMod<_>
        member x.scalarLayers = _scalarLayers :> amap<_,_>
        member x.selectedScalar = _selectedScalar :> IMod<_>
        member x.textureLayers = _textureLayers :> alist<_>
        member x.selectedTexture = _selectedTexture :> IMod<_>
        member x.surfaceType = _surfaceType :> IMod<_>
        member x.colorCorrection = _colorCorrection
        member x.homePosition = _homePosition :> IMod<_>
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.Surface) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                ResetMod.Update(_guid,v.guid)
                ResetMod.Update(_name,v.name)
                ResetMod.Update(_importPath,v.importPath)
                ResetMod.Update(_opcNames,v.opcNames)
                ResetMod.Update(_opcPaths,v.opcPaths)
                ResetMod.Update(_relativePaths,v.relativePaths)
                ResetMod.Update(_fillMode,v.fillMode)
                ResetMod.Update(_cullMode,v.cullMode)
                ResetMod.Update(_isVisible,v.isVisible)
                ResetMod.Update(_isActive,v.isActive)
                Aardvark.UI.Mutable.MNumericInput.Update(_quality, v.quality)
                Aardvark.UI.Mutable.MNumericInput.Update(_priority, v.priority)
                Aardvark.UI.Mutable.MNumericInput.Update(_triangleSize, v.triangleSize)
                Aardvark.UI.Mutable.MNumericInput.Update(_scaling, v.scaling)
                ResetMod.Update(_preTransform,v.preTransform)
                MMap.Update(_scalarLayers, v.scalarLayers)
                MOption.Update(_selectedScalar, v.selectedScalar)
                MList.Update(_textureLayers, v.textureLayers)
                MOption.Update(_selectedTexture, v.selectedTexture)
                ResetMod.Update(_surfaceType,v.surfaceType)
                MColorCorrection.Update(_colorCorrection, v.colorCorrection)
                MOption.Update(_homePosition, v.homePosition)
                
        
        static member Create(__initial : PRo3D.Surfaces.Surface) : MSurface = MSurface(__initial)
        static member Update(m : MSurface, v : PRo3D.Surfaces.Surface) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.Surface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Surface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let guid =
                { new Lens<PRo3D.Surfaces.Surface, System.Guid>() with
                    override x.Get(r) = r.guid
                    override x.Set(r,v) = { r with guid = v }
                    override x.Update(r,f) = { r with guid = f r.guid }
                }
            let name =
                { new Lens<PRo3D.Surfaces.Surface, System.String>() with
                    override x.Get(r) = r.name
                    override x.Set(r,v) = { r with name = v }
                    override x.Update(r,f) = { r with name = f r.name }
                }
            let importPath =
                { new Lens<PRo3D.Surfaces.Surface, System.String>() with
                    override x.Get(r) = r.importPath
                    override x.Set(r,v) = { r with importPath = v }
                    override x.Update(r,f) = { r with importPath = f r.importPath }
                }
            let opcNames =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Collections.List<System.String>>() with
                    override x.Get(r) = r.opcNames
                    override x.Set(r,v) = { r with opcNames = v }
                    override x.Update(r,f) = { r with opcNames = f r.opcNames }
                }
            let opcPaths =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Collections.List<System.String>>() with
                    override x.Get(r) = r.opcPaths
                    override x.Set(r,v) = { r with opcPaths = v }
                    override x.Update(r,f) = { r with opcPaths = f r.opcPaths }
                }
            let relativePaths =
                { new Lens<PRo3D.Surfaces.Surface, System.Boolean>() with
                    override x.Get(r) = r.relativePaths
                    override x.Set(r,v) = { r with relativePaths = v }
                    override x.Update(r,f) = { r with relativePaths = f r.relativePaths }
                }
            let fillMode =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Rendering.FillMode>() with
                    override x.Get(r) = r.fillMode
                    override x.Set(r,v) = { r with fillMode = v }
                    override x.Update(r,f) = { r with fillMode = f r.fillMode }
                }
            let cullMode =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Rendering.CullMode>() with
                    override x.Get(r) = r.cullMode
                    override x.Set(r,v) = { r with cullMode = v }
                    override x.Update(r,f) = { r with cullMode = f r.cullMode }
                }
            let isVisible =
                { new Lens<PRo3D.Surfaces.Surface, System.Boolean>() with
                    override x.Get(r) = r.isVisible
                    override x.Set(r,v) = { r with isVisible = v }
                    override x.Update(r,f) = { r with isVisible = f r.isVisible }
                }
            let isActive =
                { new Lens<PRo3D.Surfaces.Surface, System.Boolean>() with
                    override x.Get(r) = r.isActive
                    override x.Set(r,v) = { r with isActive = v }
                    override x.Update(r,f) = { r with isActive = f r.isActive }
                }
            let quality =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.quality
                    override x.Set(r,v) = { r with quality = v }
                    override x.Update(r,f) = { r with quality = f r.quality }
                }
            let priority =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.priority
                    override x.Set(r,v) = { r with priority = v }
                    override x.Update(r,f) = { r with priority = f r.priority }
                }
            let triangleSize =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.triangleSize
                    override x.Set(r,v) = { r with triangleSize = v }
                    override x.Update(r,f) = { r with triangleSize = f r.triangleSize }
                }
            let scaling =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.UI.NumericInput>() with
                    override x.Get(r) = r.scaling
                    override x.Set(r,v) = { r with scaling = v }
                    override x.Update(r,f) = { r with scaling = f r.scaling }
                }
            let preTransform =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.Trafo3d>() with
                    override x.Get(r) = r.preTransform
                    override x.Set(r,v) = { r with preTransform = v }
                    override x.Update(r,f) = { r with preTransform = f r.preTransform }
                }
            let scalarLayers =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.hmap<System.Int32,PRo3D.Surfaces.ScalarLayer>>() with
                    override x.Get(r) = r.scalarLayers
                    override x.Set(r,v) = { r with scalarLayers = v }
                    override x.Update(r,f) = { r with scalarLayers = f r.scalarLayers }
                }
            let selectedScalar =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.Option<PRo3D.Surfaces.ScalarLayer>>() with
                    override x.Get(r) = r.selectedScalar
                    override x.Set(r,v) = { r with selectedScalar = v }
                    override x.Update(r,f) = { r with selectedScalar = f r.selectedScalar }
                }
            let textureLayers =
                { new Lens<PRo3D.Surfaces.Surface, Aardvark.Base.plist<PRo3D.Surfaces.TextureLayer>>() with
                    override x.Get(r) = r.textureLayers
                    override x.Set(r,v) = { r with textureLayers = v }
                    override x.Update(r,f) = { r with textureLayers = f r.textureLayers }
                }
            let selectedTexture =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.Option<PRo3D.Surfaces.TextureLayer>>() with
                    override x.Get(r) = r.selectedTexture
                    override x.Set(r,v) = { r with selectedTexture = v }
                    override x.Update(r,f) = { r with selectedTexture = f r.selectedTexture }
                }
            let surfaceType =
                { new Lens<PRo3D.Surfaces.Surface, PRo3D.Surfaces.SurfaceType>() with
                    override x.Get(r) = r.surfaceType
                    override x.Set(r,v) = { r with surfaceType = v }
                    override x.Update(r,f) = { r with surfaceType = f r.surfaceType }
                }
            let colorCorrection =
                { new Lens<PRo3D.Surfaces.Surface, PRo3D.Surfaces.ColorCorrection>() with
                    override x.Get(r) = r.colorCorrection
                    override x.Set(r,v) = { r with colorCorrection = v }
                    override x.Update(r,f) = { r with colorCorrection = f r.colorCorrection }
                }
            let homePosition =
                { new Lens<PRo3D.Surfaces.Surface, Microsoft.FSharp.Core.Option<Aardvark.Base.CameraView>>() with
                    override x.Get(r) = r.homePosition
                    override x.Set(r,v) = { r with homePosition = v }
                    override x.Update(r,f) = { r with homePosition = f r.homePosition }
                }
    
    
    type MSgSurface(__initial : PRo3D.Surfaces.SgSurface) =
        inherit obj()
        let mutable __current : Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.SgSurface> = Aardvark.Base.Incremental.EqModRef<PRo3D.Surfaces.SgSurface>(__initial) :> Aardvark.Base.Incremental.IModRef<PRo3D.Surfaces.SgSurface>
        let _trafo = Aardvark.UI.Trafos.Mutable.MTransformation.Create(__initial.trafo)
        let _globalBB = ResetMod.Create(__initial.globalBB)
        let _sceneGraph = ResetMod.Create(__initial.sceneGraph)
        let _picking = ResetMod.Create(__initial.picking)
        let _transformation = MTransformations.Create(__initial.transformation)
        
        member x.surface = __current.Value.surface
        member x.trafo = _trafo
        member x.globalBB = _globalBB :> IMod<_>
        member x.sceneGraph = _sceneGraph :> IMod<_>
        member x.picking = _picking :> IMod<_>
        member x.transformation = _transformation
        
        member x.Current = __current :> IMod<_>
        member x.Update(v : PRo3D.Surfaces.SgSurface) =
            if not (System.Object.ReferenceEquals(__current.Value, v)) then
                __current.Value <- v
                
                Aardvark.UI.Trafos.Mutable.MTransformation.Update(_trafo, v.trafo)
                ResetMod.Update(_globalBB,v.globalBB)
                ResetMod.Update(_sceneGraph,v.sceneGraph)
                ResetMod.Update(_picking,v.picking)
                MTransformations.Update(_transformation, v.transformation)
                
        
        static member Create(__initial : PRo3D.Surfaces.SgSurface) : MSgSurface = MSgSurface(__initial)
        static member Update(m : MSgSurface, v : PRo3D.Surfaces.SgSurface) = m.Update(v)
        
        override x.ToString() = __current.Value.ToString()
        member x.AsString = sprintf "%A" __current.Value
        interface IUpdatable<PRo3D.Surfaces.SgSurface> with
            member x.Update v = x.Update v
    
    
    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SgSurface =
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module Lens =
            let surface =
                { new Lens<PRo3D.Surfaces.SgSurface, System.Guid>() with
                    override x.Get(r) = r.surface
                    override x.Set(r,v) = { r with surface = v }
                    override x.Update(r,f) = { r with surface = f r.surface }
                }
            let trafo =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.UI.Trafos.Transformation>() with
                    override x.Get(r) = r.trafo
                    override x.Set(r,v) = { r with trafo = v }
                    override x.Update(r,f) = { r with trafo = f r.trafo }
                }
            let globalBB =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.Base.Box3d>() with
                    override x.Get(r) = r.globalBB
                    override x.Set(r,v) = { r with globalBB = v }
                    override x.Update(r,f) = { r with globalBB = f r.globalBB }
                }
            let sceneGraph =
                { new Lens<PRo3D.Surfaces.SgSurface, Aardvark.SceneGraph.ISg>() with
                    override x.Get(r) = r.sceneGraph
                    override x.Set(r,v) = { r with sceneGraph = v }
                    override x.Update(r,f) = { r with sceneGraph = f r.sceneGraph }
                }
            let picking =
                { new Lens<PRo3D.Surfaces.SgSurface, PRo3D.Surfaces.Picking>() with
                    override x.Get(r) = r.picking
                    override x.Set(r,v) = { r with picking = v }
                    override x.Update(r,f) = { r with picking = f r.picking }
                }
            let transformation =
                { new Lens<PRo3D.Surfaces.SgSurface, PRo3D.Surfaces.Transformations>() with
                    override x.Get(r) = r.transformation
                    override x.Set(r,v) = { r with transformation = v }
                    override x.Update(r,f) = { r with transformation = f r.transformation }
                }

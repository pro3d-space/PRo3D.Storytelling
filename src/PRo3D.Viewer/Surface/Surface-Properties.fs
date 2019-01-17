namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.SceneGraph.Opc
open Aardvark.VRVis
open Aardvark.SceneGraph

open PRo3D

module SurfaceProperties =
    open Aardvark.UI.Html.SemUi
    open FShade.Intrinsics
    

    type Action =
        | SetFillMode    of FillMode
        | SetCullMode    of CullMode
        | SetName        of string
        | ToggleVisible 
        | ToggleIsActive
        | SetQuality     of Numeric.Action
        | SetTriangleSize of Numeric.Action
        | SetPriority    of Numeric.Action
        | SetScaling     of Numeric.Action
        | SetScalarMap   of Option<ScalarLayer>
        | SetTexturesMap of Option<TextureLayer>
        //| SetContrast    of Numeric.Action
        //| UseContrast    of bool
        //| SetBrightness  of Numeric.Action
        //| UseBrightness  of bool
        //| SetGamma       of Numeric.Action
        //| UseGamma       of bool
        //| SetColor       of ColorPicker.Action

    let update (model : PRo3D.Surfaces.Surface) (act : Action) =
        match act with
            | SetFillMode mode ->
                { model with fillMode = mode }
            | SetCullMode mode ->
                { model with cullMode = mode }
            | SetName s ->
                { model with name = s }
            | ToggleVisible ->
                { model with isVisible = not model.isVisible }
            | ToggleIsActive ->
                { model with isActive = not model.isActive }
            | SetTriangleSize a ->
                { model with triangleSize = Numeric.update model.triangleSize a}
            | SetQuality a ->
                { model with quality = Numeric.update model.quality a }
            | SetPriority a ->
                { model with priority = Numeric.update model.priority a }
            | SetScaling a ->
                { model with scaling = Numeric.update model.scaling a }
            | SetScalarMap a ->                
                { model with selectedScalar = a } |> Console.print
            | SetTexturesMap a ->                
                { model with selectedTexture = a } |> Console.print
            

    let getTextures (layers : seq<AttributeLayer>) =
        layers 
          |> Seq.choose (fun x -> match x with | TextureLayer l -> Some l | _ -> None) 
          |> Seq.mapi(fun i x -> { x with index = i}) 
          |> PList.ofSeq

    let getScalars (layers : seq<AttributeLayer>) =
        layers 
          |> Seq.choose (fun x -> match x with | ScalarLayer l -> Some l | _ -> None) 
          |> Seq.mapi(fun i x -> { x with index = i}) 
          |> PList.ofSeq

    let getScalarsHmap (layers : seq<AttributeLayer>) =
        layers 
          |> Seq.choose (fun x -> match x with | ScalarLayer l -> Some l | _ -> None) 
          |> Seq.mapi(fun i x -> { x with index = i}) 
          |> Seq.map(fun x -> x.index, x)
          |> HMap.ofSeq

    let mapTolist (input : amap<_,'a>) : alist<'a> = 
        input |> AMap.toASet |> AList.ofASet |> AList.map snd    
    
    let scalarLayerList (m:MSurface) = 
        (m.scalarLayers |> mapTolist)
          
    let view (model : MSurface) =        
      require GuiEx.semui (
        Html.table [                                            
          // Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
          Html.row "Name:"        [Html.SemUi.textBox model.name SetName ]
          Html.row "Visible:"     [GuiEx.iconCheckBox model.isVisible ToggleVisible ]
          Html.row "Active:"      [GuiEx.iconCheckBox model.isActive ToggleIsActive ]
          Html.row "Priority:"    [Numeric.view' [NumericInputType.InputBox] model.priority |> UI.map SetPriority ]       
          Html.row "Quality:"     [Numeric.view' [NumericInputType.Slider]   model.quality  |> UI.map SetQuality ]
          Html.row "TriangleFilter:" [Numeric.view' [NumericInputType.InputBox]   model.triangleSize  |> UI.map SetTriangleSize ]
          Html.row "Scale:"       [Numeric.view' [NumericInputType.InputBox]   model.scaling  |> UI.map SetScaling ]
          Html.row "Fillmode:"    [Html.SemUi.dropDown model.fillMode SetFillMode]                
          Html.row "Scalars:"     [UI.dropDown'' (model |> scalarLayerList)  model.selectedScalar   (fun x -> SetScalarMap (x |> Option.map(fun y -> y.Current |> Mod.force)))   (fun x -> x.label |> Mod.force)]
          Html.row "Textures:"    [UI.dropDown'' model.textureLayers model.selectedTexture  (fun x -> SetTexturesMap x) (fun x -> x.label)]
          Html.row "Cull Faces:"  [Html.SemUi.dropDown model.cullMode SetCullMode]
        ]
      )

module ColorCorrectionProperties =
    open Aardvark.UI.Html.SemUi
    open FShade.Intrinsics

    type Action =
        | SetContrast    of Numeric.Action
        | UseContrast    
        | SetBrightness  of Numeric.Action
        | UseBrightness  
        | SetGamma       of Numeric.Action
        | UseGamma       
        | SetColor       of ColorPicker.Action
        | UseColor
        | UseGrayScale

    let update (model : PRo3D.Surfaces.ColorCorrection) (act : Action) =        
      match act with
        | SetContrast c ->
            { model with contrast = Numeric.update model.contrast c }
        | UseContrast  ->
            { model with useContrast = (not model.useContrast) }
        | SetBrightness b ->
            { model with brightness = Numeric.update model.brightness b }
        | UseBrightness  ->
            { model with useBrightn = (not model.useBrightn) }
        | SetGamma g ->
            { model with gamma = Numeric.update model.gamma g }
        | UseGamma  ->
            { model with useGamma = (not model.useGamma) }
        | SetColor a ->
            { model with color = ColorPicker.update model.color a }
        | UseColor  ->
            { model with useColor = (not model.useColor) }
        | UseGrayScale  ->
            { model with useGrayscale = (not model.useGrayscale) }

    let view (model : MColorCorrection) =        
      require GuiEx.semui (
        Html.table [          
          Html.row "use contrast:"        [GuiEx.iconCheckBox model.useContrast UseContrast ] // group.visible (GroupsMessage(Groups.ToggleGroup path))
          Html.row "set contrast:"        [Numeric.view' [NumericInputType.Slider]   model.contrast  |> UI.map SetContrast ] 
          Html.row "use brightness:"      [GuiEx.iconCheckBox model.useBrightn UseBrightness ]
          Html.row "set brightness:"      [Numeric.view' [NumericInputType.Slider]   model.brightness  |> UI.map SetBrightness ] 
          Html.row "use gamma:"           [GuiEx.iconCheckBox model.useGamma UseGamma ]
          Html.row "set gamma:"           [Numeric.view' [NumericInputType.Slider]   model.gamma  |> UI.map SetGamma ] 
          Html.row "grayscale:"           [GuiEx.iconCheckBox model.useGrayscale UseGrayScale ]
          Html.row "use color:"           [GuiEx.iconCheckBox model.useColor UseColor ]
          Html.row "color:"               [ColorPicker.view model.color |> UI.map SetColor ]
        ]
      )
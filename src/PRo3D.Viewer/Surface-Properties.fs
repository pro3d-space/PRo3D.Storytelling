namespace PRo3D.Surfaces

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.Rendering
open Aardvark.Opc    
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
        | SetQuality     of Numeric.Action
        | SetTriangleSize of Numeric.Action
        | SetPriority    of Numeric.Action
        | SetScalarMap   of Option<ScalarLayer>
        | SetTexturesMap of Option<TextureLayer>

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
            | SetTriangleSize a ->
                { model with triangleSize = Numeric.update model.triangleSize a }
            | SetQuality a ->
                { model with quality = Numeric.update model.quality a }
            | SetPriority a ->
                { model with priority = Numeric.update model.priority a }
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

    let view (model : MSurface) =
        
        require Html.semui (
            Html.table [                                            
               // Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
                Html.row "Name:"        [Html.SemUi.textBox model.name SetName ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                Html.row "Priority:"    [Numeric.view' [NumericInputType.InputBox] model.priority |> UI.map SetPriority ]       
                Html.row "Quality:"     [Numeric.view' [NumericInputType.Slider]   model.quality  |> UI.map SetQuality ]
                Html.row "TriangleSize:"     [Numeric.view' [NumericInputType.InputBox]   model.triangleSize  |> UI.map SetTriangleSize ]
                Html.row "Fillmode:"    [Html.SemUi.dropDown model.fillMode SetFillMode]
                Html.row "Scalars:"     [UI.dropDown'' model.scalarLayers  model.selectedScalar   (fun x -> SetScalarMap x)   (fun x -> x.label)]
                Html.row "Textures:"    [UI.dropDown'' model.textureLayers model.selectedTexture  (fun x -> SetTexturesMap x) (fun x -> x.label)]
                Html.row "Cull Faces:"  [Html.SemUi.dropDown model.cullMode SetCullMode]
            ]
        )
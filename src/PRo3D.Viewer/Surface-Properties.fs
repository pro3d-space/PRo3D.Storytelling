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
    
    let dropDown'' (values : alist<'a>)(selected : IMod<Option<'a>>) (change : Option<'a> -> 'msg) (f : 'a ->string)  =

        let attributes (name : string) =
            AttributeMap.ofListCond [
                always (attribute "value" (name))
                onlyWhen (
                        selected 
                            |> Mod.map (
                                fun x -> 
                                    match x with
                                        | Some s -> name = f s
                                        | None   -> name = "-None-"
                            )) (attribute "selected" "selected")
            ]

        let ortisOnChange  = 
            let cb (i : int) =
                let currentState = values.Content |> Mod.force
                change (PList.tryAt (i-1) currentState)
                    
            onEvent "onchange" ["event.target.selectedIndex"] (fun x -> x |> List.head |> Int32.Parse |> cb)

        Incremental.select (AttributeMap.ofList [ortisOnChange; style "color:black"]) 
            (
                alist {
                    yield Incremental.option (attributes "-None-") (AList.ofList [text "-None-"])
                    yield! values |> AList.mapi(fun i x -> Incremental.option (attributes (f x)) (AList.ofList [text (f x)]))
                }
            )

    type Action =
        | SetFillMode    of FillMode
        | SetCullMode    of CullMode
        | SetName        of string
        | ToggleVisible 
        | SetQuality     of Numeric.Action
        | SetPriority    of Numeric.Action
        | SetScalarMap   of Option<ScalarLayer>
        | SetTexturesMap of Option<TextureLayer>

    let update (model : Surface) (act : Action) =
        match act with
            | SetFillMode mode ->
                { model with fillMode = mode }
            | SetCullMode mode ->
                { model with cullMode = mode }
            | SetName s ->
                { model with name = s }
            | ToggleVisible ->
                { model with isVisible = not model.isVisible }
            | SetQuality a ->
                { model with quality = Numeric.update model.quality a }
            | SetPriority a ->
                { model with priority = Numeric.update model.priority a }
            | SetScalarMap a ->                
                { model with selectedScalar = a }
            | SetTexturesMap a ->                
                { model with selectedTexture = a }

    let getTextures (layers : seq<AttributeLayer>) =
        layers |> Seq.choose (fun x -> match x with | TextureLayer l -> Some l | _ -> None) |> PList.ofSeq

    let getScalars (layers : seq<AttributeLayer>) =
        layers |> Seq.choose (fun x -> match x with | ScalarLayer l -> Some l | _ -> None) |> PList.ofSeq

    let view (model : MSurface) =
        
        require Html.semui (
            Html.table [                                            
               // Html.row "Path:"        [Incremental.text (model.importPath |> Mod.map (fun x -> sprintf "%A" x ))]                
                Html.row "Fillmode:"    [Html.SemUi.dropDown model.fillMode SetFillMode]
                Html.row "Cullmode:"    [Html.SemUi.dropDown model.cullMode SetCullMode]
                Html.row "Name:"        [Html.SemUi.textBox model.name SetName ]
                Html.row "Visible:"     [Html.SemUi.toggleBox model.isVisible ToggleVisible ]
                Html.row "Quality:"     [Numeric.view' [NumericInputType.Slider]   model.quality  |> UI.map SetQuality ]
                Html.row "Priority:"    [Numeric.view' [NumericInputType.InputBox] model.priority |> UI.map SetPriority ]       
                Html.row "Scalars:"     [dropDown'' model.scalarLayers model.selectedScalar (fun x -> SetScalarMap x) (fun x -> x.label)]
                Html.row "Textures:"    [dropDown'' model.textureLayers model.selectedTexture (fun x -> SetTexturesMap x) (fun x -> x.label)]
            ]
        )
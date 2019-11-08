module AnnotationApp

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

open Annotations

[<AutoOpen>]
module private Helpers =

    let baseSize = V2i (800, 600)

    let rnd = new RandomSystem ()

    let tryFindById (id : AnnotationId) (a : Annotations) =
        a.list |> PList.tryFindIndex (fun x -> x.id = id)
               |> Option.map (fun i -> i, PList.get i a.list)

    // Updates the annotations based on the id of an annotation and the given function; if the annotation
    // does not exist, the identity function is applied instead
    let updateById (f : Index -> Annotation -> Annotations -> Annotations) (id : AnnotationId) (a : Annotations) =
        match tryFindById id a with
            | None ->
                Log.error "Annotation with id '%A' does not exist!" id
                a
            | Some (i, x) ->
                a |> f i x

    let mapById (f : Annotation -> Annotation) (id : AnnotationId) (a : Annotations) =
        let g i a =
            Lens.update Annotations.Lens.list (PList.set i (f a))

        a |> updateById g id

    let deleteById (id : AnnotationId) (a : Annotations) =
        let g i _ =
            Lens.update Annotations.Lens.list (PList.remove i)

        a |> updateById g id

let init = {
    list = PList.empty
    focus = None
    targeting = false
}

let update (msg : AnnotationAction) (a : Annotations) =
    match msg with
        | LabelChanged (id, text) ->
            a |> mapById (Annotation.setLabelText text) id
        | LabelMoved (id, pos) ->
            a |> mapById (Annotation.setLabelPosition pos) id
        | LabelResized (id, w) ->
            a |> mapById (Annotation.setLabelWidth w) id
        | LabelClicked id ->
            match a.targeting, a.focus with
                | (true, Some f) when f.id = id ->
                    a |> mapById (Annotation.setTarget None) id
                      |> Lens.set Annotations.Lens.targeting false
                | _ ->
                    a
        | ChangeLabelFontSize ->
            a |> mapById Annotation.changeLabelFontSize a.focus.Value.id
        | Focus id ->
            { a with focus = a |> tryFindById id |> Option.map snd }
        | Blur ->
            { a with focus = None }   
        | Add s ->
            let r = rnd.UniformV2d (Box2d (0.25, 0.25, 0.75, 0.75))
            let pos = V2i (r * V2d (baseSize))
            let x = pos |> Label.create s |> Annotation.create None

            { a with list = a.list |> PList.append x }
        | Remove id ->
            a |> deleteById id
        | Target ->
            { a with targeting = not a.targeting }
        | SaveTarget p ->
            a |> mapById (Annotation.setTarget <| Some p) a.focus.Value.id
              |> Lens.set Annotations.Lens.targeting false

let view (viewport : IMod<V2i>) (viewProjTrafo : IMod<Trafo3d>) (sceneHit : IMod<V3d>) (presentation : bool) (annotations : MAnnotations) =

    let preventBlur (x : DomNode<'a>) =
        onBoot "$('#__ID__').on('mousedown', function (event) { event.preventDefault(); });" x

    let onBlur' (cb : unit -> 'msg list) =
        onEvent' "onblur" [] (ignore >> cb >> Seq.ofList)
    
    let setBaseSize =
        onBoot (sprintf "setBaseSize(%s);" <| Pickler.jsonToString baseSize)

    let setViewport =
        onBootInitial "viewportData" viewport "setViewport(__DATA__)"

    // Returns if the given annotation is currently
    // focused and targeting
    let isTargeting (a : MAnnotation) =
        adaptive {
            let! focus = annotations.focus
            let! targeting = annotations.targeting

            match targeting, focus with
                | true, Some f ->
                    return! Mod.map2 (=) f.id a.id
                | _ -> 
                    return false
        }

    let hasTarget (a : MAnnotation) =
        a.target |> Mod.map Option.isSome
                 |> Mod.map2 (||) (isTargeting a)

    let mkArrow (a : MAnnotation) =
        let clipPos = adaptive {
            let! targeting = isTargeting a

            let! t = 
                if targeting then 
                    sceneHit
                else
                    a.target |> Mod.map (Option.defaultValue V3d.Zero)

            let! vp = viewProjTrafo |> Mod.map (fun t -> t.Forward)
            return vp.TransformPosProjFull t
        }

        let targetPos = clipPos |> Mod.map (fun cp ->
            let u = (cp.XY / cp.W + V2d.II) * 0.5
            V2d (u.X, 1.0 - u.Y) * V2d baseSize
        )

        let arrowHeadSize = clipPos |> Mod.map (fun cp ->
            (64.0 / cp.W) |> max 2.0
                          |> min 64.0
        )

        let setArrowTarget = onBootInitial "targetData" targetPos "setArrowTarget($('#__ID__'), __DATA__)"
        let setArrowHeadSize = onBootInitial "sizeData" arrowHeadSize "setArrowHeadSize($('#__ID__'), __DATA__)"

        let setData =
            setArrowTarget >> setArrowHeadSize

        setData (
            Svg.g [
                clazz "arrow"
                attribute "data-origin" <| Pickler.jsonToString V2i.Zero
                attribute "data-midpoint" <| Pickler.jsonToString V2i.Zero
                attribute "data-target" <| Pickler.jsonToString V2i.Zero
            ] [
                Svg.defs [] [
                    Svg.marker [
                        clazz "arrowHead"
                        attribute "orient" "auto"
                    ] [ Svg.path [] ]
                ]

                Incremental.Svg.polyline (AttributeMap.ofAMap <| amap {
                    yield clazz "arrowLine"

                    let! hasTarget = hasTarget a
                    let! behindCamera = clipPos |> Mod.map (fun p -> p.W < 0.0)

                    if not hasTarget || behindCamera then
                        yield style "visibility: hidden"
                })
            ]
        )

    let mkLabel (a : MAnnotation) =
        let disablePropagation event =
            sprintf "$('#__ID__').on('%s', function(e) { e.stopPropagation(); } ); " event

        let init x = 
            onBoot ("initLabel($('#__ID__')); " + 
                    disablePropagation "keydown" +
                    disablePropagation "keypress" +
                    disablePropagation "keyup") x

        let setText = onBootInitial "textData" a.label.text "setText($('#__ID__'), __DATA__)"
        let setWidth = onBootInitial "widthData" a.label.width "setWidth($('#__ID__'), __DATA__)"
        let setPosition = onBootInitial "positionData" a.label.position "setPosition($('#__ID__'), __DATA__)"
        let setFontSize = onBootInitial "fontSizeData" a.label.fontSize "setFontSize($('#__ID__'), __DATA__)"

        let setData =
            setWidth >> setPosition

        init (
            setData (
                Incremental.div (AttributeMap.ofAMap <| amap {
                    yield clazz <| "label"

                    let! id = a.id
                    yield attribute "data-pos" <| Pickler.jsonToString V2i.Zero
                    yield onLabelMoved (fun p -> (id, p) |> LabelMoved)
                    yield onLabelResized (fun w -> (id, w) |> LabelResized)
                    yield onClick (fun _ -> LabelClicked id)

                }) <| AList.ofList [
                    if not presentation then
                        yield div [clazz "grabber"] []

                        yield preventBlur (
                            div [clazz "ui icon move button"] [
                                i [clazz "move icon"] []
                            ]
                        )

                        yield Incremental.div (AttributeMap.ofAMap <| amap {
                            let! id = a.id

                            yield clazz "ui icon remove button"
                            yield onClick (fun _ -> id |> Remove)
                        }) <| AList.ofList [
                            i [clazz "remove icon"] []
                        ]

                    yield (setText >> setFontSize) (
                        Incremental.textarea (AttributeMap.ofAMap <| amap {
                            let! id = a.id

                            yield attribute "rows" "1"

                            if not presentation then
                                yield attribute "placeholder" "Add text here..."

                            yield onChange (fun s -> LabelChanged (id, s))
                            yield onFocus (fun _ -> Focus id)

                            let! targeting = annotations.targeting

                            if targeting then
                                let! p = sceneHit
                                yield onBlur' (fun _ -> [SaveTarget p; Blur])
                            else
                                yield onBlur (fun _ -> Blur)

                            if presentation then
                                yield attribute "disabled" ""
                        }) a.label.text
                    )
                ]
            )
        )

    let mkAnnotation (a : MAnnotation) =
        Svg.g [
            clazz "annotation"
        ] [
            mkArrow a

            Svg.foreignObject [] [
                mkLabel a
            ]
        ]

    let dependencies = [
        { kind = Stylesheet; name = "annotationsStyle"; url = "Annotations.css" }
        { kind = Script; name = "annotationsScript"; url = "Annotations.js" }
    ]

    require dependencies (
        setViewport (
            setBaseSize (
                Incremental.Svg.svg (AttributeMap.ofList [
                    yield clazz <| "annotations" + if presentation then " presentation" else ""
                ]) <| AList.map mkAnnotation annotations.list
            )
        )
    )
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

    let pendingTarget (a : Annotations) =
        a.selected
        |> Option.bind (fun id -> tryFindById id a)
        |> Option.map (snd >> Annotation.hasPendingTarget)
        |> Option.defaultValue false

    let autoPosition (trafo : Trafo3d) (offset : V2i) (x : V3d) (y : V3d) =
        let center = (x + y) / 2.0
        let cp = trafo.Forward.TransformPosProjFull center
        let u = (cp.XY / cp.W + V2d.II) * 0.5

        V2i (V2d (u.X, 1.0 - u.Y) * V2d baseSize) + offset

let init = {
    list = PList.empty
    selected = None
    targeting = false
}

let update (viewProj : Trafo3d) (msg : AnnotationAction) (a : Annotations) =
    match msg with
        | LabelChanged (id, text) ->
            a |> mapById (Annotation.setLabelText text) id
        | LabelMoved (id, pos) ->
            a |> mapById (Annotation.setLabelPosition (autoPosition viewProj V2i.Zero) pos) id
        | LabelResized (id, w) ->
            a |> mapById (Annotation.setLabelWidth w) id
        | LabelClicked id ->
            if a.selected |> Option.contains id then
                a
            else
                { a with selected = Some id; targeting = false }
        | ChangeLabelFontSize ->
            a |> mapById Annotation.changeLabelFontSize a.selected.Value
        | Add s ->
            let r = rnd.UniformV2d (Box2d (0.25, 0.25, 0.75, 0.75))
            let pos = V2i (r * V2d (baseSize))
            let x = pos |> Label.create s |> Annotation.create (Point None)

            { a with list = a.list |> PList.append x
                     selected = Some x.id
                     targeting = false }
        | Remove id ->
            if a.selected |> Option.contains id then
                { a with selected = None; targeting = false }
            else
                a
            |> deleteById id
        | Target initial ->
            match a.targeting, a.selected with
            | _, Some id ->
                a |> mapById (Annotation.setTarget initial) id
            | _ ->
                a
            |> Lens.update Annotations.Lens.targeting not
        | SaveTarget p ->
            let n =
                match a.targeting, a.selected with
                | true, Some id ->
                    a |> mapById (Annotation.saveTarget p) id
                | _ ->
                    { a with selected = None }

            n |> Lens.set Annotations.Lens.targeting (pendingTarget n)

let view (viewport : IMod<V2i>) (viewProjTrafo : IMod<Trafo3d>) (sceneHit : IMod<V3d>) (presentation : bool) (annotations : MAnnotations) =

    let preventBlurAndPropagation =
        let blur = "$('#__ID__').on('mousedown', function (e) { e.preventDefault(); });"
        let propagation = "$('#__ID__').on('click', function(e) { e.stopPropagation(); } );"

        onBoot (blur + propagation)
    
    let setBaseSize =
        onBoot (sprintf "setBaseSize(%s);" <| Pickler.jsonToString baseSize)

    let setViewport =
        onBootInitial "viewportData" viewport "setViewport(__DATA__)"

    let isSelected (a : MAnnotation) =
        annotations.selected |> Mod.map2 Option.contains a.id

    let getPosition (a : MAnnotation) =
        adaptive {
            let! target = a.target
            match target with
            | MScale (x, y) ->
                let! x = x
                let! y = y

                match x, y with
                | Some x, Some y ->
                    let! vp = viewProjTrafo
                    let! offset = a.label.offset
                    return autoPosition vp offset x y
                | _ ->
                    return! a.label.position
            | _ ->
                return! a.label.position 
        }

    // Returns if the given annotation is currently
    // focused and targeting
    let isTargeting (a : MAnnotation) =
        adaptive {
            let! targeting = annotations.targeting

            if targeting then
                return! annotations.selected |> Mod.map2 (Option.contains) a.id
            else
                return false
        }

    let getAnchors (a : MAnnotation) =
        a.target |> Mod.bind (fun t ->
            match t with
                | MPoint p ->
                    p |> Mod.map List.singleton
                | MScale(x, y) ->
                    Mod.map2 (fun x y -> [x; y]) x y
        )

    let mkArrow (a : MAnnotation) =

        let clipPos = adaptive {
            let! anchors = getAnchors a
            let! targeting = isTargeting a
            let! sceneHit = sceneHit
            let! vp = viewProjTrafo

            let filtered =
                match anchors with
                    | [_]
                    | None::[_] when targeting ->
                        [sceneHit]
                    | (Some x)::[_] when targeting ->
                        [x; sceneHit]
                    | xs ->
                        xs |> List.filter Option.isSome
                           |> List.map Option.get

            return filtered |> List.map vp.Forward.TransformPosProjFull
        }

        let targetPos = clipPos |> Mod.map (fun pts ->
            pts |> List.map (fun cp ->
                let u = (cp.XY / cp.W + V2d.II) * 0.5
                V2d (u.X, 1.0 - u.Y) * V2d baseSize
            )
        )

        // TODO: This should probably depend on the scale of the
        // loaded OPC instead of using some magic numbers
        let arrowHeadSize = clipPos |> Mod.map (fun pts ->
            pts |> List.map (fun cp ->
                (24.0 / cp.W)
                    |> max 2.0
                    |> min 4.0
            )
        )
        
        let setArrowTargets = onBootInitial "targetData" targetPos "setArrowTargets($('#__ID__'), __DATA__)"
        let setArrowHeadSize = onBootInitial "sizeData" arrowHeadSize "setArrowHeadSize($('#__ID__'), __DATA__)"
        let setArrowOutline = onBoot "setArrowOutline($('#__ID__'))"
        let setArrowData = setArrowOutline >> setArrowTargets

        Svg.g [
            clazz "arrow"
        ] [
            let marker =
                Svg.marker [
                    clazz "arrowHead"
                    attribute "orient" "auto-start-reverse"
                ] [ Svg.path [] ]

            setArrowHeadSize (
                Svg.defs [] [marker; marker]
            )

            setArrowData (
                Incremental.Svg.polyline (AttributeMap.ofAMap <| amap {
                    yield clazz "arrowLine"

                    let! behindCamera =
                        clipPos |> Mod.map (List.exists (fun p -> p.W < 0.0))

                    let! emptyTargets =
                        targetPos |> Mod.map List.isEmpty

                    // We could also check this in an incremental div and
                    // decide not to add it to the DOM. But this would have to be done
                    // at the same level as the label (see mkAnnotation) resulting in recomputations of the label
                    // This is problematic since a new element is created and focus is not preserved.
                    // There are certainly more elegant solutions but this one seems to be the simplest one.
                    if behindCamera || emptyTargets then
                        yield style "visibility: hidden"
                })
            )
        ]

    let mkLabel (a : MAnnotation) =
        let disablePropagation event =
            sprintf "$('#__ID__').on('%s', function(e) { e.stopPropagation(); } ); " event

        let init x = 
            onBoot ("initLabel($('#__ID__')); " + 
                    disablePropagation "keydown" +
                    disablePropagation "keypress" +
                    disablePropagation "keyup" +
                    disablePropagation "click") x

        let setText = onBootInitial "textData" a.label.text "setText($('#__ID__'), __DATA__)"
        let setWidth = onBootInitial "widthData" a.label.width "setWidth($('#__ID__'), __DATA__)"
        let setPosition = onBootInitial "positionData" (getPosition a) "setPosition($('#__ID__'), __DATA__)"
        let setFontSize = onBootInitial "fontSizeData" a.label.fontSize "setFontSize($('#__ID__'), __DATA__)"

        let setData =
            setWidth >> setPosition

        init (
            setData (
                Incremental.div (AttributeMap.ofAMap <| amap {
                    let! selected = isSelected a
                    yield clazz <| "label" + if selected && not presentation then " selected" else ""

                    let! id = a.id
                    yield attribute "data-pos" <| Pickler.jsonToString V2i.Zero
                    yield onLabelMoved (fun p -> (id, p) |> LabelMoved)
                    yield onLabelResized (fun w -> (id, w) |> LabelResized)
                    yield onClick (fun _ -> LabelClicked id)

                }) <| AList.ofList [
                    if not presentation then
                        yield preventBlurAndPropagation (
                            div [clazz "grabber"] []
                        )

                        yield preventBlurAndPropagation (
                            div [clazz "ui icon move button"] [
                                i [clazz "move icon"] []
                            ]
                        )

                        yield preventBlurAndPropagation (
                            Incremental.div (AttributeMap.ofAMap <| amap {
                                let! id = a.id

                                yield clazz "ui icon remove button"
                                yield onClick (fun _ -> id |> Remove)
                            }) <| AList.ofList [
                                i [clazz "remove icon"] []
                            ]
                        )

                    yield (setText >> setFontSize) (
                        Incremental.textarea (AttributeMap.ofAMap <| amap {
                            let! id = a.id

                            yield attribute "rows" "1"

                            if presentation then
                                yield attribute "disabled" ""
                            else
                                yield attribute "placeholder" "Add text here..."
                                yield onChange (fun s -> LabelChanged (id, s))                                
                        }) a.label.text
                    )
                ]
            )
        )

    let mkAnnotation (a : MAnnotation) =
        Svg.g [
            clazz "annotation"
        ][
            mkArrow a

            Svg.foreignObject [] [
                mkLabel a
            ]
        ]

    // FIXME: This looks pretty decent on the arrows, unfortunately doesn't work
    // when the line is completely horizontal or vertical
    let outlineFilter =
        Svg.filter [
            clazz "outlineFilter"
            attribute "x" "-50%"
            attribute "y" "-50%"
            attribute "width" "500%"
            attribute "height" "500%"
        ] [
            Svg.feColorMatrix [
                attribute "type" "matrix"
                attribute "result" "blackOut"
                attribute "in" "SourceGraphic"
                attribute "values" "0 0 0 0 0, 0 0 0 0 0, 0 0 0 0 0, 0 0 0 1 0"
            ]

            Svg.feGaussianBlur [
                attribute "result" "blurOut"
                attribute "in" "blackOut"
                attribute "stdDeviation" "2"
            ]

            Svg.feBlend [
                attribute "in" "SourceGraphic"
                attribute "in2" "blurOut"
                attribute "mode" "normal"
            ]
        ]

    let dependencies = [
        { kind = Stylesheet; name = "annotationsStyle"; url = "Annotations.css" }
        { kind = Script; name = "annotationsScript"; url = "Annotations.js" }
    ]

    require dependencies (
        setViewport (
            setBaseSize (
                Svg.svg [] [
                    Svg.defs [] [outlineFilter]

                    Incremental.Svg.g (AttributeMap.ofList [
                        yield clazz <| "annotations" + if presentation then " presentation" else ""
                    ]) <| AList.map mkAnnotation annotations.list
                ]
            )
        )
    )
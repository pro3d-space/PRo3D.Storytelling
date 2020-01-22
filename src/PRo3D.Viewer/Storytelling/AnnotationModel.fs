namespace Annotations

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI

// Each annotation has an id
type AnnotationId = 
    private AnnotationId of Guid with

    static member generate () =
        AnnotationId (Guid.NewGuid ())

    static member ofGuid (v : Guid) =
        AnnotationId v

    static member parse (s : string) =
        s |> Guid.Parse |> AnnotationId

    static member tryParse (s : string) =
        try
            Some (s |> Guid.Parse |> AnnotationId)
        with
            | _ -> None

    override x.ToString () =
        let (AnnotationId v) = x in string v

// A label displays text at a given position
[<DomainType>]
type Label = {
    width : int
    position : V2i  // Absolute position in screen space
    offset : V2i    // Relative offset in screen space when using a scale target
    text : string
    fontSize : int
}

// An annotation can target either separate points
// or a scale represented by two points
type AnnotationTargetType =
    | PointsType
    | ScaleType

[<DomainType>]
type AnnotationTarget =
    | Points of V3d list
    | Scale of V3d option * V3d option

// An annotation is a label and optionally a target
// in world space that is to be annotated
[<DomainType>]
type Annotation = {
    id : AnnotationId
    target : AnnotationTarget
    label : Label
}

// Annotations can be selected
[<DomainType>]
type Selection = {
    // Id of the annotation
    id : AnnotationId

    // Is the annotation being targeted
    targeting : bool

    // Are the font controls showing for the user to
    // adjust the font of the selected annotation
    fontControls : bool
}

[<DomainType; CustomEquality; NoComparison>]
type Annotations = 
    {
        list : Annotation plist
        selection : Selection option
    }

    override x.GetHashCode () =
        hash x.list

    override x.Equals y =
        match y with
            | :? Annotations as y -> x.list = y.list
            | _ -> false

type AnnotationAction =
    | ResetSelection
    | LabelChanged              of AnnotationId * string
    | LabelMoved                of AnnotationId * V2i
    | LabelResized              of AnnotationId * int
    | LabelClicked              of AnnotationId
    | ToggleFontSizeSelection
    | SetFontSize               of int
    | Add                       of string
    | Remove                    of AnnotationId
    | StartTargeting            of (Annotation -> Annotation)
    | StopTargeting
    | SaveTarget                of V3d

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Label =
    let create (text : string) (position : V2i) = {
        width = 300
        position = position
        offset = V2i.Zero
        text = text
        fontSize = 32
    }

[<AutoOpen>]
 module Events =
    // Fired after a label is resized
    let onLabelResized (cb : int -> 'msg) =
        onEvent "onlabelresized" [] (List.head >> Pickler.unpickleOfJson >> cb)

    // Fired after a label is moved
    let onLabelMoved (cb : V2i -> 'msg) =
        onEvent "onlabelmoved" [] (List.head >> Pickler.unpickleOfJson >> cb)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Annotation =
    let create (target : AnnotationTarget) (label : Label) = {
        id = AnnotationId.generate ()
        target = target
        label = label
    }

    let getTargetType (a : Annotation) =
        match a.target with
        | Points _ -> PointsType
        | Scale _ -> ScaleType

    let hasPendingTarget (a : Annotation) =
        match a.target with
        | Scale (None, _) -> true
        | Scale (_, None) -> true
        | _ -> false

    let setLabelText (text : string) (a : Annotation) =
        { a with label = { a.label with text = text }}

    let setLabelPosition (autoPosition : V3d -> V3d -> V2i) (pos : V2i) (a : Annotation) =
        let label =
            match a.target with
            | Scale (Some x, Some y) ->
                let center = autoPosition x y
                { a.label with offset = pos - center }
            | _ ->
                { a.label with position = pos }

        { a with label = label }

    let setLabelWidth (width : int) (a : Annotation) =
        { a with label = { a.label with width = width }}

    let setLabelFontSize (size : int) (a : Annotation) =
        { a with label = { a.label with fontSize = size }}

    let startPointTargeting (a : Annotation) =
        let target =
            match a.target with
            | Scale _ ->
                Points []
            | Points xs ->
                Points xs

        { a with target = target }

    let startScaleTargeting (a : Annotation) =
        { a with target = Scale (None, None) }

    let saveTarget (target : V3d) (a : Annotation) =
        let target =
            match a.target with
            | Points xs ->
                Points (target::xs)
            | Scale (None, y) ->
                Scale (Some target, y)
            | Scale (x, _) ->
                Scale (x, Some target)

        { a with target = target }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Selection =
    let create (id : AnnotationId) =
        {
            id = id
            targeting = false
            fontControls = false
        }

    let targeting (selection : Selection option) =
        selection |> Option.exists (fun sel -> sel.targeting)

    let contains (id : AnnotationId) (selection : Selection option) =
        selection |> Option.exists (fun sel -> sel.id = id)

    let setTargeting (targeting : bool) (selection : Selection option) =
        selection |> Option.map (fun sel ->
            { sel with targeting = targeting }
        )

    let toggleTargeting (selection : Selection option) =
        selection |> Option.map (fun sel ->
            { sel with targeting = not sel.targeting }
        )

    let setFontControls (fontControls : bool) (selection : Selection option) =
        selection |> Option.map (fun sel ->
            { sel with fontControls = fontControls }
        )

    let toggleFontControls (selection : Selection option) =
        selection |> Option.map (fun sel ->
            { sel with fontControls = not sel.fontControls }
        )
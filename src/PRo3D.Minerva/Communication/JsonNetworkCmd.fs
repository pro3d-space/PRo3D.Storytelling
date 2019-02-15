namespace PRo3D.Minerva.Communication

open System
open FSharp.Data

module JsonNetworkCommand =


    type V2d(x, y) =
        let mutable internalX : float = x
        let mutable internalY : float = y
        member this.X = internalX
        member this.Y = internalY

    type V2i(x, y) =
        let mutable internalX : int = x
        let mutable internalY : int = y
        member this.X = internalX
        member this.Y = internalY

    type C4b(r, g, b, a) =
        let mutable internalR : byte = r
        let mutable internalG : byte = g
        let mutable internalB : byte = b
        let mutable internalA : byte = a
        member this.R = internalR
        member this.G = internalG
        member this.B = internalB
        member this.A = internalA

    /////////////////////////////////////////////////////////////////////////////////////////////////
    // Sending the JsonCommands
    /////////////////////////////////////////////////////////////////////////////////////////////////

    type Params =
        {
            names : string[]
            numOfLists : int
            numOfEntries : int

            image : string option
            imageResolution : V2i option

            messageID : string
        }
    let EmptyParams = {names = [||]; numOfLists = 0; numOfEntries = 0; image = None; imageResolution = None; messageID = ""}
    
    type Data =
        {
            ids : string[] option
            positions : V2d[] option
            colors : C4b[] option
        }
    let EmptyData = {ids = None; positions = None; colors = None}

    type CommandData =
        {
            name : string
            para : Params
            data : Data
        }


    let createJsonName (name : string) = 
        let jsonName = "{ \"Name\": \"" + name + "\"}"
        jsonName
    

    let createJsonParam (par : Params) =
        let mutable jsonParam = "{ \"Param\": {\"Names\": ["
        for n in par.names do 
            jsonParam <- jsonParam + "\"" + n + "\","
        jsonParam <- jsonParam.Remove(jsonParam.Length - 1) + "],"
        jsonParam <- jsonParam + "\"NumOfLists\": " + (par.numOfLists |> string) + ", "
        jsonParam <- jsonParam + "\"NumOfEntries\": " + (par.numOfEntries |> string) + ", "

        match par.image with
        | Some(s) ->  jsonParam <- jsonParam + "\"Image\": \"" + s + "\", "
        | None -> ()

        match par.imageResolution with
        | Some(res) -> jsonParam <- jsonParam + "\"ImageResolution\": [" + (res.X |> string) + "," + (res.Y |> string) + "],"
        | None -> ()

        jsonParam <- jsonParam + "\"MessageID\": \"" + par.messageID  + "\"}}"

        jsonParam


    let createJsonData (par : Params) (data : Data) =
        let mutable jsonData = "{\"Data\": {\""

        match data.ids with
            | Some(ids) ->
                jsonData <- jsonData + par.names.[0] + "\": ["
                for id in ids do
                    jsonData <- jsonData + "\"" + (id) + "\","
                jsonData <- jsonData.Remove(jsonData.Length - 1) + "]"
            | None -> ()

        match data.positions with
        | Some(positions) ->
            jsonData <- jsonData + ", \"" + par.names.[1]  + "\": ["
            for pos in positions do
                jsonData <- jsonData + (pos.X |> string) + "," + (pos.Y |> string) + ","
            jsonData <- jsonData.Remove(jsonData.Length - 1) + "]"
        | None -> ()

        // either positions or colors, but not both!
        match data.colors with
        | Some(colors) ->
            jsonData <- jsonData + ", \"" + par.names.[1]  + "\": ["
            for c in colors do
                jsonData <- jsonData + (c.R |> string) + "," + (c.G |> string) + "," + (c.B |> string) + "," + (c.A |> string) + ","
            jsonData <- jsonData.Remove(jsonData.Length - 1) + "]"
        | None -> ()

        jsonData <- jsonData + "}}"
        jsonData
    

    let createJsonNetworkCommand (cmd : CommandData) =
        let cmdName = cmd.name |> createJsonName
        let cmdPara = cmd.para |> createJsonParam
        let cmdData = cmd.data |> createJsonData cmd.para

        let header = "JsonNetworkCommand"
        let command = "{\"JsonCommand\": [" + cmdName + ", " + cmdPara + ", " + cmdData + "]}"
        let numberOfBytes = command.Length
        let message = header + " " + (numberOfBytes |> string) + " " + command
        message
    

    let createJsonNetworkResponse (msg : string) =
        let cmdName = "{ \"Name\": \"" + msg + "\"}"
        let cmdPara = "{ \"Param\": \"{}}"
        let cmdData = "{ \"Data\": \"{}}"

        let header = "JsonNetworkResponse"
        let command = "{\"JsonCommand\": [" + cmdName + ", " + cmdPara + ", " + cmdData + "]}"
        let numberOfBytes = command.Length
        let message = header + " " + (numberOfBytes |> string) + " " + command
        message

    

    /////////////////////////////////////////////////////////////////////////////////////////////////
    // Receiving the JsonCommands
    /////////////////////////////////////////////////////////////////////////////////////////////////
    
    /// this converter can also be used for other json messages... colors e.g. is just empty...
    type MinervaCommand = JsonProvider<"""
            {"JsonCommand" :
                [
                    {"Name" : "MinervaColorMapping"},
                    {"Param":
                        {
                            "Names": ["IdList", "Colors"],
                            "NumOfLists": 2,
                            "NumOfEntries": 1,
                            "Image": "path/to/image.png",
                            "ImageResolution": [1024, 768],
                            "MessageID": "messageID"
                        }
                    },
                    {"Data" :
                        {
                            "IdList": ["APB_494375951RWP10910491876_______P1"],
                            "Positions": [0.01, 0.02],
                            "Colors": [125, 125, 178, 255]
                        }
                    }
                ]
            }
            """, SampleIsList = true>
    
    let convertMinervaCommand(msg : string) : CommandData =
        let mutable messageID = ""
        let command = MinervaCommand.Parse(msg)
        let length = command.JsonCommand.Length
        if length <> 3 then
            printf "Error: JsonCommand should always have length 3"

        let cmdname = command.JsonCommand.[0].Name
        let cmdparam = command.JsonCommand.[1].Param
        let cmddata = command.JsonCommand.[2].Data

        let name =
            match cmdname with
            | Some(s) -> s
            | None -> ""
        
        let para = 
            match cmdparam with
            | Some(p) ->
                        let listNames = p.Names
                        let numOfLists = p.NumOfLists
                        let numOfEntries = p.NumOfEntries
                        let image =
                            match p.Image with
                            | "" -> None
                            | _ -> Some(p.Image)

                        let resolution =
                            match p.ImageResolution with
                            | [||] -> None
                            | _ -> Some(V2i(p.ImageResolution.[0], p.ImageResolution.[1]))
                        
                        let msgID =
                            match p.MessageId with
                            | "" -> name
                            | _ -> p.MessageId

                        {
                            names = listNames; numOfLists = numOfLists; numOfEntries = numOfEntries;
                            image = image; imageResolution = resolution; messageID = msgID
                        }

            | None -> EmptyParams
        
        let data = 
            match cmddata with
            | Some(d) ->
                        let ids =
                            match d.IdList with
                            | [||] -> None
                            | _ -> Some(d.IdList)

                        let colors =
                            match d.Colors with
                            | [||] -> None
                            | _ ->
                                    let c = d.Colors
                                    let l = c.Length
                                    let mutable colorArray = [||]
                                    let mutable index = 0
                                    while index < (l-1) do
                                        let color = C4b(c.[index + 0] |> byte, c.[index + 1] |> byte, c.[index + 2] |> byte, c.[index + 3] |> byte)
                                        colorArray <- [|color|] |> Array.append colorArray
                                        index <- index + 4
                                    Some(colorArray)

                        let positions =
                            match d.Positions with
                            | [||] -> None
                            | _ ->
                                    let pos = d.Positions
                                    let l = pos.Length
                                    let mutable positionArray = [||]
                                    let mutable index = 0
                                    while index < (l-1) do
                                        let p = V2d(pos.[index + 0] |> float, pos.[index + 1] |> float)
                                        positionArray <- [|p|] |> Array.append positionArray
                                        index <- index + 1
                                    Some(positionArray)

                        {ids = ids; positions = positions; colors = colors}

            | None -> EmptyData

        {name = name; para = para; data = data}

    
    let removeHeaderAndGetCommand (converter : string -> CommandData) (uneditedString : string) : CommandData =
        let parts = uneditedString.Split(' ')
        let count = parts.Length
        if count < 3 then failwith "message should contain at least 2 whitspaces!"
        let mutable msg = parts.[2]
        if count > 3 then
            for i in 3..(count-1) do
                msg <- msg + " " + parts.[i]
        msg |> converter 
    
    let convertMessageToCommand (msg : string) : CommandData =
        msg |> removeHeaderAndGetCommand convertMinervaCommand

    (********************************************************************************************************
     * For debugging purposes: use the ones below for printing the message to the console
     ********************************************************************************************************)

    let printCommand(cmd : CommandData) : unit =
        
        printfn "Command name is: %A" cmd.name

        let p = cmd.para
        printfn "mandatory params are: %A, %A and %A" p.names p.numOfLists p.numOfEntries
        printfn "optional params ars: %A(image), %A(image resolution) and %A(messageID)" p.image p.imageResolution p.messageID

        let d = cmd.data
        printfn "data - ids: %A with colors: %A, and positions: %A" d.ids d.colors d.positions
        ()
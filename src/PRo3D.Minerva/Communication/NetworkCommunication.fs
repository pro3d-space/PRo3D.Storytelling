namespace PRo3D.Minerva.Communication

//type Communicator =
//  {
  
//  }

module Communicator =
  
  open System.Threading
  open Server
  open Client    
  open PRo3D.Minerva.Communication.JsonNetworkCommand
  
  let genRandomNumbers count =
      let rnd = System.Random()
      List.init count (fun _ -> ((rnd.Next() % 10000) |> float) / 10000.0 )
  
  let rndCount = 10000
  let rndList = genRandomNumbers rndCount
  
  let mutable next = 0
  let mutable errorCntr = 0
  let errorNumber = 25
  
  let generateRandomNumber() : float =
      let num = rndList.[next]
      if next >= (rndCount-1) then next <- 0
      else next <- next + 1
      if errorCntr >= errorNumber then
          errorCntr <- 0
          num + 1.0 // invalid number --> want to check how visplore handels these
      else
          errorCntr <- errorCntr + 1
          num
  
  type Communicator() =
    
    let address = "127.0.0.1"
    let listeningPort = 50124
    let sendingPort = 50123
  
    let mutable client : Client = Unchecked.defaultof<_>
    let mutable server : Server = Unchecked.defaultof<_>
  
    let mutable cnt = 0
    let mutable evalCommand : CommandData -> string = fun _ -> "no eval set"
  
    // incoming requests
    //let evaluateCommand (cmd : CommandData) : string =
    //  match cmd.name with
    //  | "MinervaReplaceSelection" ->
    //     // Visplore sends Pro3D the selected ids --> do something with them (here: print them)
    //     cmd |> printCommand
    //     cmd.para.messageID
  
    //  | "MinervaRequestProjection" ->
    //    // Visplore requests new projection data --> send them
    //    // get the new data
    //    let positions, idLength =
    //        match cmd.data.ids with
    //        | Some(ids) ->
    //            // since this is a prototype --> generate random values between 0 and 1
    //            let mutable posArray : V2d[] = [||]
    //            for id in ids do
    //                let pX = generateRandomNumber()
    //                let pY = generateRandomNumber()
    //                posArray <- [| V2d(pX, pY) |] |> Array.append posArray
    //            Some(posArray), ids.Length
    //        | None -> None, 0
  
    //    // these are just some dummy data
        
    //    let mutable imagepath = "C:/Users/schimkowitsch/Pictures/donauweg.jpg"
    //    let mutable imageresolution = V2i(1033, 944)
  
    //    if cnt = 0 then
    //        imagepath <- "C:/Users/schimkowitsch/Pictures/lastofus.jpg"
    //        imageresolution <- V2i(1920, 1040)
    //        cnt <- 1
    //    else
    //        cnt <- 0
  
    //    // get the new command
    //    let name = "MinervaReplaceProjection"
    //    let para : Params = {names = [|"IdList";"Positions"|]; numOfLists = 2; numOfEntries = idLength;
    //                         image = Some(imagepath); imageResolution = Some(imageresolution); messageID = ""}
    //    let data : Data = {ids = cmd.data.ids; positions = positions; colors = None}
  
    //    // transform to jsonMessage
    //    let jsonMessage = createJsonNetworkCommand {name = name; para = para; data = data}
  
    //    // send it
    //    client.SendMessage(jsonMessage)
  
    //    // return messageID ... required for response, in case one is required
    //    cmd.para.messageID
    //  | _ -> ""
    
    do
        // construct new client --> to send commands to Visplore
        client <- new Client(address, sendingPort)
  
        // construct new server --> to receive commands from Visplore
        let responder (msg : string) : string = createJsonNetworkResponse msg
        let converter (msg : string) : string =
            let tmp = msg |> convertMessageToCommand
            let a = tmp|> evalCommand
            a
        server <- new Server(converter, responder)
    
    member this.GetClient() = client
  
    member this.Start(cmd) =
        let address = System.Net.IPAddress.Parse(address)
        evalCommand <- cmd
        server.Start(ipAddress = address, port = listeningPort)
    
    member this.Close() =
        server.Close()

  


module Server

open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Text

open Suave
open SuaveConfig
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors

open Suave.Json
open System.Runtime.Serialization
open Suave.Logging


type Socket with
  member socket.AsyncAccept() = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)

  member socket.AsyncReceive(buffer:byte[], ?offset, ?count) =
    let offset = defaultArg offset 0
    let count = defaultArg count buffer.Length
    let beginReceive(b,o,c,cb,s) = socket.BeginReceive(b,o,c,SocketFlags.None,cb,s)
    Async.FromBeginEnd(buffer, offset, count, beginReceive, socket.EndReceive)

  member socket.AsyncSend(buffer:byte[], ?offset, ?count) =
    let offset = defaultArg offset 0
    let count = defaultArg count buffer.Length
    let beginSend(b,o,c,cb,s) = socket.BeginSend(b,o,c,SocketFlags.None,cb,s)
    Async.FromBeginEnd(buffer, offset, count, beginSend, socket.EndSend)


open Aardvark.Base
type Server(converter : string -> string, responder : string -> string) =
    
    let mutable internalServer : IDisposable = Unchecked.defaultof<_>
    let mutable listener : Socket = Unchecked.defaultof<_>
    
    member x.ReceivedData : string = ""

    member x.Start(?ipAddress, ?port) =
        
        let ipAddress = defaultArg ipAddress IPAddress.Any
        let port = defaultArg port 80
        let endpoint = IPEndPoint(ipAddress, port)
        let cts = new CancellationTokenSource()

        listener <- new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)
        Log.line "[Minerva] Started listening on port %d" port
    
        let rec loop() = async {
            Log.line "[Minerva] Waiting for request ..."
            let! socket = listener.AsyncAccept()
            Log.line "[Minerva] Received request"

            let buffer : byte[] = Array.zeroCreate 65536
            let buffer1 : byte[] = Array.zeroCreate 100

            try
              let! head = socket.AsyncReceive(buffer1, 0, 50)
              let headString = Encoding.ASCII.GetString(buffer1, 0, head)
              let commandLength = (headString.Split(' ').[1]) |> Int32.Parse //write functional!
              
              Log.line "[Minerva] Found Command of length: %d" commandLength
              
              let mutable msg = ""
              let mutable bytesRead = 0

              //while bytesRead < commandLength do
              // bytesRead <- bytesRead + buffer.length

              let! res = socket.AsyncReceive(buffer, bytesRead, buffer.Length)
              let page = Encoding.ASCII.GetString(buffer, 0, res)
              
              Log.line "[Minerva] read %d bytes" res

              msg <- msg + ((page |> converter) |> responder)
              let response = Encoding.ASCII.GetBytes(msg)
              
              try
                  let! bytesSent = socket.AsyncSend(response)
                  Log.line "[Minerva] Sent response: %A (%A)" msg bytesSent
                  socket.Shutdown(SocketShutdown.Both)              
              with e -> Log.line "[Minerva] An error occurred responding: %s" e.Message
            with e -> Log.line "[Minerva] An error occurred receiving: %s" e.Message

            socket.Shutdown(SocketShutdown.Both)
            socket.Close()

            return! loop() }

        Async.Start(loop(), cancellationToken = cts.Token)
        internalServer <- { new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }
    
    member x.Close() = 
        internalServer.Dispose()

////////////////////////////////////////////////////////////////////////////////////////////////////
[<DataContract>]
type Calc =
   { 
      [<field: DataMember(Name = "a")>]
      a : int;
      [<field: DataMember(Name = "b")>]
      b : int;
   }
 

 [<DataContract>]
type Result =
   { 
      [<field: DataMember(Name = "result")>]
      result : int;
   }

type WebSocket (address : string, port : int) = 

    let add =
       request (fun r ->
           match r.queryParam "a" with
           | Choice1Of2 a -> 
               match r.queryParam "b" with
               | Choice1Of2 b ->    
                    let a = System.Int32.Parse a
                    let b = System.Int32.Parse b
                    OK (sprintf "a: %d b: %d -> %d" a b (a + b))
               | Choice2Of2 msg -> BAD_REQUEST msg
           | Choice2Of2 msg -> BAD_REQUEST msg)

    let app =
        choose
            [ GET >=> choose
                [ path "/hello" >=> OK "Hello GET"
                  path "/goodbye" >=> OK "Good bye GET" 
                  path "/testAdd" >=> add 

                ]
              POST >=> choose
                [ path "/hello" >=> 
                    OK "Hello POST"
                  path "/goodbye" >=> OK "Good bye POST" 
                  path "/urdar" >=> mapJson (fun (calc:Calc) -> { result = calc.a + calc.b })
                ] 
            ]
     
    let mimeTypes =
        Writers.defaultMimeTypesMap
        @@ (function | ".avi" -> Writers.createMimeType "video/avi" false | _ -> None)
    

    /////////////////////////////////////////////////////////////////////
    let logger = Targets.create Verbose [||]

    /// Or a little more elaborated:

    //let loggingOptions =
    //    { Literate.LiterateOptions.create() with
    //        getLogLevelText = function Verbose->"V" | Debug->"D" | Info->"I" | Warn->"W" | Error->"E" | Fatal->"F" }

    //let logger =
    //    LiterateConsoleTarget(
    //                            name = [|"Suave";"Examples";"Example"|],
    //                            minLevel = Verbose,
    //                            options = loggingOptions,
    //                            outputTemplate = "[{level}] {timestampUtc:o} {message} [{source}]{exceptions}"
    //                            ) :> Logger
    /////////////////////////////////////////////////////////////////////
    member x.Start() =
        let newConfig =
            { bindings              = [ HttpBinding.createSimple HTTP address port]
              serverKey             = Utils.Crypto.generateKey HttpRuntime.ServerKeyLength
              errorHandler          = defaultErrorHandler
              listenTimeout         = TimeSpan.FromMilliseconds 2000.
              cancellationToken     = Async.DefaultCancellationToken
              bufferSize            = 2048
              maxOps                = 100
              autoGrow              = true
              mimeTypesMap          = mimeTypes
              homeFolder            = None
              compressedFilesFolder = None
              logger                = logger
              tcpServerFactory      = new DefaultTcpServerFactory()
              cookieSerialiser      = new BinaryFormatterSerialiser()
              tlsProvider           = new DefaultTlsProvider()
              hideHeader            = false
              maxContentLength = 1000000 }

        startWebServer newConfig app
        

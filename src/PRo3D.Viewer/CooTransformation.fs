namespace PRo3D

open Aardvark.Base
open ViewPlanner


module CooTransformation = 

    let init = double(0.0)

    let initCooTrafo = 
        let directory = @".\InstrumentStuff" 
        let test = @".\bin" 
        let errorCode = CooTrafo.Init(test, directory)
        printfn "%A" errorCode

    let deInitCooTrafo = 
        CooTrafo.DeInit()

    let getLatLonAlt (p:V3d) : SphericalCoo = 
        let lat = ref init
        let lon = ref init
        let alt = ref init
        let pX = double p.X
        let pY = double p.Y
        let pZ = double p.Z
        let bla = CooTrafo.Xyz2LatLonAlt("Mars", p.X, p.Y, p.Z, lat, lon, alt)
        
        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        {
            latitude = !lat
            longitude = !lon
            altitude = !alt
            radian = 0.0;
            }

    let getLatLonRad (p:V3d) : SphericalCoo = 
        let lat = ref init
        let lon = ref init
        let rad = ref init
        let pX = double p.X
        let pY = double p.Y
        let pZ = double p.Z
        let bla = CooTrafo.Xyz2LatLonRad( pX, pY, pZ, lat, lon, rad)
        
        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        {
            latitude = !lat
            longitude = !lon
            altitude = 0.0
            radian = !rad
            }

    let getXYZFromLatLonAlt (sc:SphericalCoo) : V3d = 
        let pX = ref init
        let pY = ref init
        let pZ = ref init
        let bla = CooTrafo.LatLonAlt2Xyz( "Mars", sc.latitude, sc.longitude, sc.altitude, pX, pY, pZ )

        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        V3d(!pX, !pY, !pZ)


    let getHeight (p:V3d) = 
        let sc = getLatLonAlt p
        sc.altitude

    let getUpVector (p:V3d) = 
        let sc = getLatLonAlt p
        let height = sc.altitude + 100.0
        (getXYZFromLatLonAlt ({sc with altitude = height})).Normalized

        
       
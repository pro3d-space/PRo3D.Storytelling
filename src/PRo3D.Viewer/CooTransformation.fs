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

    let getLatLonAlt (p:V3d) (planet:string): SphericalCoo = 
        let lat = ref init
        let lon = ref init
        let alt = ref init
        let bla = CooTrafo.Xyz2LatLonAlt(planet, p.X, p.Y, p.Z, lat, lon, alt)
        
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
        let bla = CooTrafo.Xyz2LatLonRad( p.X, p.Y, p.Z, lat, lon, rad)
        
        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        {
            latitude = !lat
            longitude = !lon
            altitude = 0.0
            radian = !rad
            }

    let getXYZFromLatLonAlt (sc:SphericalCoo) (planet:string) : V3d = 
        let pX = ref init
        let pY = ref init
        let pZ = ref init
        let bla = CooTrafo.LatLonAlt2Xyz( planet, sc.latitude, sc.longitude, sc.altitude, pX, pY, pZ )

        if bla <> 0 then
            Log.line "cootrafo errorcode %A" bla

        V3d(!pX, !pY, !pZ)


    let getHeight (p:V3d) (planet:string) = 
        let sc = getLatLonAlt p planet
        sc.altitude

    let getUpVector (p:V3d) (planet:string) = 
        let sc = getLatLonAlt p planet
        let height = sc.altitude + 100.0
        (getXYZFromLatLonAlt ({sc with altitude = height}) planet).Normalized

        
       
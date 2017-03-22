namespace PRo3DGui

type Path = string

type SurfaceInfo = {
    surfaceName     : string
    surfacePath     : string
    opcPaths        : list<Path>
    maxTriangleSize : float
}

type DiscoveredSurfaces = { 
    currentPath        : string
    selectedSurface    : Option<Path>
    selectedSurface1   : Option<SurfaceInfo>
    //selectedSurface2   : Path
    discoveredSurfaces : list<Path> 
    emptySurfaceName   : string
    opcPath            : string
    surfaceInfos       : list<SurfaceInfo>
    surfaceName        : string
    maxTriangleSize    : string
    selectedOPC        : Option<Path>
    fromOPCPath        : string
}

type SurfaceDetails = {
    surfaceInfo         : SurfaceInfo
    surfaceName         : string
    maxTriangleSize     : string
    selectedOPC         : Option<Path>
    fromOPCPath         : string
}

type RibbonToggles = {
    isNavigationActive    : bool
    isMeasurementsActive  : bool
    isViewPlannerActive   : bool
    }



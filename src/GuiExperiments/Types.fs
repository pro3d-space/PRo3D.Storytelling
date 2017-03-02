namespace PRo3DGui


type Path = string

type SurfaceInfo = {
    surfaceName     : string
    opcPaths        : list<Path>
    maxTriangleSize : float
}
type DiscoveredSurfaces = { 
    currentPath        : string
    searchPath         : list<Path> 
    selectedSurface    : Option<SurfaceInfo>
    selectedSurface2    : Path
    discoveredSurfaces : list<Path> 
    emptySurfaceName   : string
    opcPath            : string
    surfacesInfo       : list<SurfaceInfo>
    surfaceName        : string
    maxTriangleSize    : string
    selectedOPC        : Option<Path>
    fromOPCPath        : string
}

namespace PRo3DGui


type Path = string

type SurfaceToLoad = {
    surfaceName     : string
    opcPaths        : list<Path>
    maxTriangleSize : float
}
type DiscoveredSurfaces = { 
    currentPath        : string
    searchPath         : list<Path> 
    selectedSurface    : Option<Path>
    discoveredSurfaces : list<Path> 
    emptySurfaceName   : string
    opcPath            : string
    surfacesToLoad     : list<SurfaceToLoad>
}

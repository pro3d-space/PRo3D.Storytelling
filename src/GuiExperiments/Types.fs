namespace PRo3DGui


type Path = string
type DiscoveredSurfaces = { 
    currentPath        : string
    selectedSurface    : Option<Path>
    discoveredSurfaces : list<Path> 
}
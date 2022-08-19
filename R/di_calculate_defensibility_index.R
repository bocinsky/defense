#' Calculate the defensibility index for a 30 meter DEM
#'
#' @param dem A 30 m DEM
#'
#' @return A raster of the defensibility index
#' @export
di_calculate_defensibility_index <- function(dem){
  e.m <- di_calc_elevation_index(dem)
  v.m <- di_calc_visibility_index(dem)

  # Convert the matrices back into geographic rasters.
  # Also, drop the "buffer" of the outer three rows.
  elev <- raster::raster(e.m[4:(nrow(e.m)-3),4:(ncol(e.m)-3)],crs=sp::CRS(raster::projection(dem)))
  vis <- raster::raster(v.m[4:(nrow(v.m)-3),4:(ncol(v.m)-3)],crs=sp::CRS(raster::projection(dem)))

  # Set the extents of the new elevation and visibility rasters
  raster::extent(vis) <- raster::extent(elev) <- raster::extent(dem)

  # Defensibility is the mean of the
  # Elevation and Visibility indices
  defense <- (elev+vis)/2

  # Apply a 3x3 mean filter to each of the index rasters
  # This removes noise from the rasters, and helps diminish the
  # impact of randomly placed site stakes
  defense <- raster::focal(defense,w=matrix(1/9,ncol=3,nrow=3))
  vis <- raster::focal(vis,w=matrix(1/9,ncol=3,nrow=3))
  elev <- raster::focal(elev,w=matrix(1/9,ncol=3,nrow=3))

  return(list(defensibility = defense,
              visibility = vis,
              elevation = elev) %>%
           raster::brick())
}

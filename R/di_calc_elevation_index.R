#' Calculate the elevation component of the defensibility index for a 30 m DEM
#'
#' @param dem A 30 m DEM
#'
#' @return A raster of the elevation index
#' @export
di_calc_elevation_index <-
  function(dem){
    dem.c <- di_buffer_dem(dem)

    # Calculate the angular difference in elevation between each cell and
    # all of its Moore neighbors within a radius of three cells.
    # Store these in a list of matrices called `elev.diffs'.
    # `elev.diffs' is needed for the calculation of the visibility
    # index.
    # The elevation indices are calculated and stored in a
    # matrix called `e.m'
    elev.diffs <- elev.difference.angular(0, dem)

    # Calculate the Elevation Index for each cell.
    e.m <- matrix(nrow=nrow(dem.c),ncol=ncol(dem.c))
    e.m[,] <- 0
    for(dx in -3:3){
      for(dy in -3:3){
        elev.diffs[4-dx,4-dy][[1]][is.na(elev.diffs[4-dx,4-dy][[1]])] <- 0
        if(!(abs(dx)==3 | abs(dy)==3)) next
        e.m <- e.m+elev.diffs[4-dx,4-dy][[1]]
      }
    }
    e.m <- ((e.m/24)+90)/180

    return(e.m)
  }

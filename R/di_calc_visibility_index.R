#' Calculate the visibility component of the defensibility index for a 30 m DEM
#'
#' @param dem A 30 m DEM
#'
#' @return A raster of the visibility index
#' @export
di_calc_visibility_index <- function(dem){
  # Calculate the visibility between the focal cell and
  # all of its Moore neighbors at a radius of three cells.
  # The visibility indices are calculated and stored in a
  # matrix called `v.m'
  # This algorithm goes cell by cell and checks the visibility
  # to all r3 cells, via matrix algebra.

  # Recalculate angular elevation given the observer height.
  elev.diffs <- elev.difference.angular(0, dem)
  dem.c <- di_buffer_dem(dem)

  # This section check for visiblity of the r3 cells.
  vis.frames <- array(list(),c(7,7))
  # The corners
  vis.frames[7,7][[1]] <- ifelse(elev.diffs[7,7][[1]]>elev.diffs[6,6][[1]] | elev.diffs[7,7][[1]]>elev.diffs[5,5][[1]],0,1)
  vis.frames[1,1][[1]] <- ifelse(elev.diffs[1,1][[1]]>elev.diffs[2,2][[1]] | elev.diffs[1,1][[1]]>elev.diffs[3,3][[1]],0,1)
  vis.frames[7,1][[1]] <- ifelse(elev.diffs[7,1][[1]]>elev.diffs[6,2][[1]] | elev.diffs[7,1][[1]]>elev.diffs[5,3][[1]],0,1)
  vis.frames[1,7][[1]] <- ifelse(elev.diffs[1,7][[1]]>elev.diffs[2,6][[1]] | elev.diffs[1,7][[1]]>elev.diffs[3,5][[1]],0,1)

  # Edge centers
  vis.frames[7,4][[1]] <- ifelse(elev.diffs[7,4][[1]]>elev.diffs[6,4][[1]] | elev.diffs[7,4][[1]]>elev.diffs[5,4][[1]],0,1)
  vis.frames[4,7][[1]] <- ifelse(elev.diffs[4,7][[1]]>elev.diffs[4,6][[1]] | elev.diffs[4,7][[1]]>elev.diffs[4,5][[1]],0,1)
  vis.frames[1,4][[1]] <- ifelse(elev.diffs[1,4][[1]]>elev.diffs[2,4][[1]] | elev.diffs[1,4][[1]]>elev.diffs[3,4][[1]],0,1)
  vis.frames[4,1][[1]] <- ifelse(elev.diffs[4,1][[1]]>elev.diffs[4,2][[1]] | elev.diffs[4,1][[1]]>elev.diffs[4,3][[1]],0,1)

  # One in from the corners
  vis.frames[7,6][[1]] <- ifelse(elev.diffs[7,6][[1]]>elev.diffs[6,5][[1]] | elev.diffs[7,6][[1]]>elev.diffs[5,5][[1]] | elev.diffs[7,6][[1]]>elev.diffs[5,4][[1]] | elev.diffs[7,6][[1]]>elev.diffs[6,6][[1]],0,1)
  vis.frames[7,2][[1]] <- ifelse(elev.diffs[7,2][[1]]>elev.diffs[4,3][[1]] | elev.diffs[7,2][[1]]>elev.diffs[5,3][[1]] | elev.diffs[7,2][[1]]>elev.diffs[5,2][[1]] | elev.diffs[7,2][[1]]>elev.diffs[6,2][[1]],0,1)
  vis.frames[6,7][[1]] <- ifelse(elev.diffs[6,7][[1]]>elev.diffs[4,5][[1]] | elev.diffs[6,7][[1]]>elev.diffs[5,5][[1]] | elev.diffs[6,7][[1]]>elev.diffs[5,6][[1]] | elev.diffs[6,7][[1]]>elev.diffs[6,6][[1]],0,1)
  vis.frames[6,1][[1]] <- ifelse(elev.diffs[6,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[6,1][[1]]>elev.diffs[5,3][[1]] | elev.diffs[6,1][[1]]>elev.diffs[5,2][[1]] | elev.diffs[6,1][[1]]>elev.diffs[6,2][[1]],0,1)
  vis.frames[2,7][[1]] <- ifelse(elev.diffs[2,7][[1]]>elev.diffs[3,4][[1]] | elev.diffs[2,7][[1]]>elev.diffs[3,5][[1]] | elev.diffs[2,7][[1]]>elev.diffs[2,5][[1]] | elev.diffs[2,7][[1]]>elev.diffs[2,6][[1]],0,1)
  vis.frames[2,1][[1]] <- ifelse(elev.diffs[2,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[2,1][[1]]>elev.diffs[3,3][[1]] | elev.diffs[2,1][[1]]>elev.diffs[3,2][[1]] | elev.diffs[2,1][[1]]>elev.diffs[2,2][[1]],0,1)
  vis.frames[1,6][[1]] <- ifelse(elev.diffs[1,6][[1]]>elev.diffs[3,4][[1]] | elev.diffs[1,6][[1]]>elev.diffs[3,5][[1]] | elev.diffs[1,6][[1]]>elev.diffs[2,5][[1]] | elev.diffs[1,6][[1]]>elev.diffs[2,6][[1]],0,1)
  vis.frames[1,2][[1]] <- ifelse(elev.diffs[1,2][[1]]>elev.diffs[2,3][[1]] | elev.diffs[1,2][[1]]>elev.diffs[3,3][[1]] | elev.diffs[1,2][[1]]>elev.diffs[3,4][[1]] | elev.diffs[1,2][[1]]>elev.diffs[2,2][[1]],0,1)

  # Two in from the corners
  vis.frames[7,5][[1]] <- ifelse(elev.diffs[7,5][[1]]>elev.diffs[6,5][[1]] | elev.diffs[7,5][[1]]>elev.diffs[5,4][[1]],0,1)
  vis.frames[7,3][[1]] <- ifelse(elev.diffs[7,3][[1]]>elev.diffs[6,3][[1]] | elev.diffs[7,3][[1]]>elev.diffs[5,4][[1]],0,1)
  vis.frames[5,7][[1]] <- ifelse(elev.diffs[5,7][[1]]>elev.diffs[4,5][[1]] | elev.diffs[5,7][[1]]>elev.diffs[5,6][[1]],0,1)
  vis.frames[5,1][[1]] <- ifelse(elev.diffs[5,1][[1]]>elev.diffs[4,3][[1]] | elev.diffs[5,1][[1]]>elev.diffs[5,2][[1]],0,1)
  vis.frames[3,7][[1]] <- ifelse(elev.diffs[3,7][[1]]>elev.diffs[3,6][[1]] | elev.diffs[3,7][[1]]>elev.diffs[4,5][[1]],0,1)
  vis.frames[3,1][[1]] <- ifelse(elev.diffs[3,1][[1]]>elev.diffs[3,2][[1]] | elev.diffs[3,1][[1]]>elev.diffs[4,3][[1]],0,1)
  vis.frames[1,5][[1]] <- ifelse(elev.diffs[1,5][[1]]>elev.diffs[2,5][[1]] | elev.diffs[1,5][[1]]>elev.diffs[3,4][[1]],0,1)
  vis.frames[1,3][[1]] <- ifelse(elev.diffs[1,3][[1]]>elev.diffs[2,3][[1]] | elev.diffs[1,3][[1]]>elev.diffs[3,4][[1]],0,1)

  # Calculate the Visibility Index
  v.m <- matrix(nrow=nrow(dem.c),ncol=ncol(dem.c))
  v.m[,] <- 0
  for(dx in -3:3){
    for(dy in -3:3){

      if(!(abs(dx)==3 | abs(dy)==3)) next
      v.m <- v.m+vis.frames[4-dx,4-dy][[1]]

    }
  }
  v.m <- (v.m/24)

  return(v.m)
}

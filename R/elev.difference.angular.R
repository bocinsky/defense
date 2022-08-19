#' Calculate the angular elevation difference
#'
#' NOTE: Build to only work on 30m DEMs!
#'
#' @param obs_elev height of the observer, in meters
#' @param dem A 30 m DEM
#'
#' @return the angular elevation difference raster
#' @export
elev.difference.angular <- function(obs_elev, dem){
  dem.m <- raster::as.matrix(dem)
  dem.c <- di_buffer_dem(dem)

  k <- 0
  elev.diffs <- array(list(),c(7,7))
  for(dx in -3:3){
    for(dy in -3:3){
      temp <- dem.m
      k <- k+1

      if(!(3-dx==0)){
        temp.col.left <- matrix(nrow=nrow(dem.m),ncol=3-dx)
        temp.col.left[,1:(3-dx)] <- rep(0,nrow(dem.m))
        temp <- cbind(temp.col.left,temp)
      }
      if(!(3+dx==0)){
        temp.col.right <- matrix(nrow=nrow(dem.m),ncol=3+dx)
        temp.col.right[,1:(3+dx)] <- rep(0,nrow(dem.m))
        temp <- cbind(temp,temp.col.right)
      }

      if(!(3-dy==0)){
        temp.row.top <- matrix(nrow=3-dy,ncol=ncol(dem.m)+6)
        temp.row.top[1:(3-dy),] <- rep(0,ncol(dem.m)+6)
        temp <- rbind(temp.row.top,temp)
      }
      if(!(3+dy==0)){
        temp.row.bottom <- matrix(nrow=3+dy,ncol=ncol(dem.m)+6)
        temp.row.bottom[1:(3+dy),] <- rep(0,ncol(dem.m)+6)
        temp <- rbind(temp,temp.row.bottom)
      }

      elev.diffs[4-dx,4-dy] <- list(atan(((dem.c+obs_elev)-temp)/(sqrt(((dx*30)^2)+((dy*30)^2))))*(180/pi))
    }
  }
  return(elev.diffs)
}

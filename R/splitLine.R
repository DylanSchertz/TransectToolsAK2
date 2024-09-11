#' @title LineReverse
#' @description Splits a line into multiple segments by a pulling a number of points.
#'
#' @importFrom terra geom vect
#'
#' @param Line An object of class SpatVector that must be a line.
#' @param interval An integer, the number of points to be included in each line segment.
#' @param startpt An integer, the position of the first split. Used to offset the buffers, a line from 1 to this number will be created.
#' 
#' 
#' @examples
#' splitLine(Line, interval = 6, startpt = 1)
#' @export
#' 

splitLine <- function(Line, interval = 6, startpt = 1){
  coords <- geom(Line)
  
  npts <- nrow(coords)
  
  splitpoints <- seq(startpt,npts, interval)
  
  if(max(splitpoints) != npts){
    splitpoints <- c(splitpoints, npts)
  }
  
  if(startpt != 1){
    splitpoints <- c(1, splitpoints)
  }
  
  OutLines <- Line[0,]
  
  for(i in  1:(length(splitpoints)-1)){
    
    firstpt <- splitpoints[i]
    lastpt <- splitpoints[i+1]
    shortcoords <- coords[firstpt:lastpt, ]
    NewLine <- vect(shortcoords, type = "Line", crs = crs(Line), atts = data.frame(Line[firstpt:lastpt, ]))
    
    OutLines <- rbind(OutLines, NewLine)
    
  }
  
  return(OutLines)
}
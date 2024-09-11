#' @title LineReverse
#' @description Switches the direction of a SpatVector line
#'
#' @importFrom terra geom vect
#'
#' @param Line An object of class SpatVector that must be a line.
#' 
#' 
#' @examples
#' LineReverse(Line)
#' @export
#' 
LineReverse <- function(Line){
  coords <- geom(Line)
  flippedcoords <- coords[nrow(coords):1,]
  sortedflippedcoords <- flippedcoords[order(flippedcoords[,1]),]
  NewLines <- vect(sortedflippedcoords, type = "Line", crs = crs(Line), atts = data.frame(Line))
  return (NewLines)
}
#' @title ViewDirCheck
#' @description Checks the specified view direction of tracklogs against a DEM. Flags mismatches.
#'
#' @importFrom terra buffer erase zonal
#'
#' @param Line An object of class SpatVector that must be a line.
#' @param DEM An object of class SpatRaster. Elevation data.
#' @param Outer A numeric value for the distance of the outer buffer of the search area.
#' @param Inner A numeric value for the distance of the inner erase buffer of the search area.
#'
#'
#' @examples
#' ViewDirCheck(Lines, DEM)
#'
#' @export
#'

ViewDirCheck <- function(Lines, DEM, Outer = 200, Inner = 100){
  Lines$DirChk <- ""

  for(i in 1:nrow(Lines)){
    Line <- Lines[i,]
    if(is.na(Line)){
      Lines$DirChk[i] <- "No geometry"
    } else {
      Buff <- buffer(Line, width = Outer, singlesided = TRUE, quadsegs = 1)

      Erase <- buffer(Line, width = Inner, singlesided = TRUE, quadsegs = 1)

      Diff <- erase(Buff, Erase)

      MeanElev <- zonal(DEM, Diff, fun="mean", na.rm = TRUE)

      FlippedLine <- LineReverse(Line)

      BuffFlipped <- buffer(FlippedLine, width = Outer, singlesided = TRUE, quadsegs = 1)

      EraseFlipped <- buffer(FlippedLine, width = Inner, singlesided = TRUE, quadsegs = 1)

      DiffFlipped <- erase(BuffFlipped, EraseFlipped)

      MeanElevFlipped <- zonal(DEM, DiffFlipped, fun="mean", na.rm = TRUE)

      if(is.na(Line$Direction)){
        if(MeanElevFlipped > MeanElev){
          Lines$DirChk[i] <- "NA Reported, DEM suggests right."
        } else {
          Lines$DirChk[i] <- "NA Reported, DEM suggests left."
        }
      } else {
        if(MeanElevFlipped > MeanElev){
          if(Line$Direction == 1){
            Lines$DirChk[i] <- "Match"
          } else {
            Lines$DirChk[i] <- "Flagged"
          }
        } else {
          if(Line$Direction == 0){
            Lines$DirChk[i] <- "Match"
          } else {
            Lines$DirChk[i] <- "Flagged"
          }
        }
      }
    }
    }


  return(Lines$DirChk)
}


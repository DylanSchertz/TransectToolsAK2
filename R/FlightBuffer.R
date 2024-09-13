#' @title FlightBuffer
#' @description Generates a one sided buffer with a blind spot from tracklogs based on buffer distance and erase distance.  Uses attributes in tracklog to pick side.
#'
#' @importFrom terra buffer aggregate
#'
#' @param Lines An object of class SpatVector that must be lines.
#' @param BuffDist The distance to be buffered.
#' @param startpt The distance for the blind spot buffer.
#'
#'
#' @examples
#' FlightBuffer(Lines, 685, 22)
#' @export
#'

FlightBuffer <- function(Lines, BuffDist = 685, EraseDist = 22){
  TempBuffer <- buffer(Lines[1, ], width = BuffDist, singlesided = TRUE, quadsegs = 1)
  OutBuffer <- TempBuffer[0, ]

  for(i in 1:nrow(Lines)){
    Line <- Lines[i,]

    if(Line$Direction == 1){
      Line <- LineReverse(Line)
    }

    Transect <- Line$Transect

    Buffer1 <- BlindBuffer(Line, BuffDist, EraseDist, quadsegs = 50)

    SplitLine1 <- splitLine(Line, interval = 4, startpt = 1)

    SplitBuffer1 <- BlindBuffer(SplitLine1, BuffDist = BuffDist, EraseDist = EraseDist, quadsegs = 1)

    SplitLine2 <- splitLine(Line, interval = 4, startpt = 3)

    SplitBuffer2 <- BlindBuffer(SplitLine2, BuffDist = BuffDist, EraseDist = EraseDist, quadsegs = 1)

    MergeBuffers <- rbind(Buffer1, SplitBuffer1, SplitBuffer2)
    DissolvedBuffer <- aggregate(MergeBuffers)

    DissolvedBuffer$Transect <- Transect

    OutBuffer <- rbind(OutBuffer, DissolvedBuffer)

  }

  DissolvedBuffersbyTransect <- OutBuffer[0, ]


  for(i in unique(OutBuffer$Transect)){
    OutputBuffers <- terra::aggregate(OutBuffer[OutBuffer$Transect == i,])
    OutputBuffers$Transect <- i

    StartTime <- min(Lines$Start_Local[Lines$Transect == i])

    OutputBuffers$Observer <- Lines$Observer[Lines$Start_Local == StartTime & Lines$Transect == i]
    OutputBuffers$Pilot <- Lines$Pilot[Lines$Start_Local == StartTime & Lines$Transect == i]
    OutputBuffers$Day_of_Year <- Lines$Day_of_Year[Lines$Start_Local == StartTime & Lines$Transect == i]
    OutputBuffers$Survey <- Lines$Survey[Lines$Start_Local == StartTime & Lines$Transect == i]

    DissolvedBuffersbyTransect <- rbind(DissolvedBuffersbyTransect, OutputBuffers)
  }

  return(DissolvedBuffersbyTransect)
}

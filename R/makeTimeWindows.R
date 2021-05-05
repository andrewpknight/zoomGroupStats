#' Helper function that creates temporal windows in datasets
#' 
#' This creates a set of temporal windows of specified size so that metrics 
#' can be computed within those windows. 
#'
#' @param inputData data.frame that has data over time, usually within a single meeting 
#' @param timeVar name of a numeric column that contains the time variable you want to use
#' @param windowSize numeric value giving the length of time window 
#'
#' @return list with two data.frames:
#' \itemize{
#'     \item windowedData - inputData with the temporal window identifying information included
#'     \item allWindows - contains the full set of temporal windows and identifying information. This is valuable because inputData may not have records within all of the possible temporal windows
#'  }
#' @export
#'
#' @examples
#' win.out = makeTimeWindows(sample_transcript_processed, 
#' timeVar="utteranceStartSeconds", windowSize=10)
makeTimeWindows = function(inputData, timeVar, windowSize) {
  
  tempTimeVarHelp <- tempTimeVar <- NULL
  
  inputData$tempTimeVar = floor(inputData[,timeVar])
  
  # Create a set of windows
  windowStart = windowSize*(1:ceiling(max(inputData[,timeVar])/windowSize)-1)
  windowEnd = windowSize*(1:ceiling((max(inputData[,timeVar])/windowSize)))-1
  windowId = 1:length(windowEnd)
  windows = data.table::data.table(cbind(windowId, windowStart, windowEnd))
  
  # Join
  inputData.dt = data.table::data.table(inputData)
  inputData.dt[, tempTimeVarHelp := tempTimeVar]
  data.table::setkey(windows, windowStart, windowEnd)
  inputMrg = data.frame(foverlaps(inputData.dt, windows, by.x=c('tempTimeVar', 'tempTimeVarHelp'), 
                                  by.y=c('windowStart', 'windowEnd')))
  inputMrg[,timeVar] = inputMrg$tempTimeVar
  inputMrg[,c("tempTimeVar", "tempTimeVarHelp")] = NULL
  windows = data.frame(windows)
  return(list("windowedData" = inputMrg, "allWindows" = windows))
}

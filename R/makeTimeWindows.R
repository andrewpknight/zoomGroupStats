#' Helper function that creates temporal windows in datasets
#'
#' @param inputData data.frame that is output from processZoomTranscript or processZoomChat 
#' @param timeVar name of a numeric column that contains the time variable you want to use
#' @param windowSize numeric value giving the length of time window 
#'
#' @return list with two data.frames. The first is inputData with the temporal windows.
#' The second is the full set of temporal windows. 
#' @export
#'
#' @examples
#' win.out = makeTimeWindows(sample_transcript_processed, 
#' timeVar="utteranceStartSeconds", windowSize=10)
makeTimeWindows = function(inputData, timeVar, windowSize) {
  
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
  head(inputMrg)
  windows = data.frame(windows)
  
  return(list("windowedData" = inputMrg, "allWindows" = windows))
}

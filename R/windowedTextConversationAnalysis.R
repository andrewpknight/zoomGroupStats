#' Run a windowed analysis on either a Zoom transcript or chat
#' This function conducts a temporal window analysis on the conversation in 
#' either a Zoom transcript or chat. It replicates the textConversationAnalysis
#' function across a set of windows at a window size specified by the user. 
#' @param inputData data.frame output of either processZoomTranscript or processZoomChat
#' @param inputType string of either 'chat' or 'transcript'
#' @param speakerId string giving the variable name of the user identity
#' @param doSentiment boolean indicating whether you want the sentiment analysis
#' @param sentiDone boolean indicating whether inputData already has the sentiment done
#' @param timeVar name of variable giving the time marker to be used.
#'  For transcript, either use 'utteranceStartSeconds' or 'utteranceEndSeconds'; 
#'  for chat use 'messageTime' 
#' @param windowSize integer value of the duration of the window in number of seconds 
#'
#' @return list with two data.frames. In the first, each row is a temporal window. 
#' In the second, each row is a user at a given temporal window.
#' @export 
#'
#' @examples
#' win.text.out = windowedTextConversationAnalysis(inputData=sample_transcript_processed, 
#' inputType="transcript", speakerId="userName", doSentiment=FALSE, 
#' sentiDone=FALSE, timeVar="utteranceStartSeconds", windowSize=300)
windowedTextConversationAnalysis = function(inputData, inputType, speakerId, doSentiment=FALSE, sentiDone=FALSE, timeVar, windowSize) {
  
  # We need to add a variable to the chat out that is number of seconds
  if(inputType=="chat" && timeVar=="messageTime") {
    inputData[, "messageTimeSeconds"] = as.numeric(difftime(inputData[,"messageTime"],min(inputData[,"messageTime"]),units="secs")) 
    timeVar = "messageTimeSeconds"		
  }
  
  
  # Add the windowing indicators to the inputData
  t.out.windowed = makeTimeWindows(inputData=inputData, timeVar=timeVar, windowSize=windowSize)
  inputData = t.out.windowed[[1]]
  
  # Create a blank set that gives each user an opportunity to have an aggregate
  # metric during each of the time windows
  
  fullSet = cbind(t.out.windowed[[2]], sort(rep(unique(inputData[, speakerId]), max(t.out.windowed[[2]]$windowId))))
  fullSet = fullSet[order(fullSet$windowId), ]
  names(fullSet)[4] = speakerId
  
  # Now, go through each of the time windows and run the
  # conversation analysis
  grp.res.out = NULL
  ind.res.out = NULL
  for(win in 1:max(fullSet$windowId)) {
    
    windowed.input = inputData[inputData$windowId == win, ]
    
    # run the analysis if there are any pieces of text in this window
    if(nrow(windowed.input) > 0) {
      
      res.line = textConversationAnalysis(inputData=windowed.input, inputType=inputType, speakerId=speakerId, doSentiment=doSentiment, sentiDone=sentiDone)
      grp.res.line = res.line[[1]]
      grp.res.line$windowId = win
      ind.res.line = res.line[[2]]
      ind.res.line$windowId = win
      
      if(!exists("grp.res.out")) {
        grp.res.out = grp.res.line
        ind.res.out = ind.res.line
      } else {
        grp.res.out = rbind(grp.res.out, grp.res.line)
        ind.res.out = rbind(ind.res.out, ind.res.line)
      }
    }
  }
  
  # Add these to the full time window datasets
  indFull = merge(fullSet, ind.res.out, by=c(speakerId, "windowId"), all.x=T)
  
  if(inputType=="transcript") {
    ind.fixVars = c("utteranceTimeWindow_sum", "numUtterances") 
    grp.fixVars = c(ind.fixVars, "totalRecordedTime", "silentTime_sum", "numUniqueSpeakers")
  } else if(inputType=="chat") {
    ind.fixVars = c("numMessages", "messageNumChars_sum")
    grp.fixVars = c(ind.fixVars, "numUniqueMessagers", "totalRecordedTime")
  }
  
  indFull[,ind.fixVars] = lapply(indFull[,ind.fixVars], function(x) ifelse(is.na(x), 0, x))
  
  grp1 = merge(t.out.windowed[[2]], grp.res.out, by=c("windowId"), all.x=T)
  grp1[, grp.fixVars] = lapply(grp1[,grp.fixVars], function(x) ifelse(is.na(x), 0, x))	
  return(list("WINDOW-LEVEL" = grp1, "SPEAKER-LEVEL" = indFull))
}

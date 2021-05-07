#' Run a windowed analysis on either a Zoom transcript or chat
#' This function conducts a temporal window analysis on the conversation in 
#' either a Zoom transcript or chat. It replicates the textConversationAnalysis
#' function across a set of windows at a window size specified by the user. 
#' @param inputData data.frame output of either processZoomTranscript or processZoomChat
#' @param inputType string of either 'chat' or 'transcript'
#' @param meetingId string giving the column with the meeting identifier
#' @param speakerId string giving the name of the identifier for the individual who made this contribution
#' @param sentMethod string giving the type of sentiment analysis to include, either 'aws' or 'syuzhet'
#' @param timeVar name of variable giving the time marker to be used.
#'  For transcript, either use 'utteranceStartSeconds' or 'utteranceEndSeconds'; 
#'  for chat use 'messageTime' 
#' @param windowSize integer value of the duration of the window in number of seconds 
#'
#' @return list with two data.frames. In the first (windowlevel), each row is a temporal window. 
#' In the second (speakerlevel), each row is a user's metrics within a given temporal window.
#' @export 
#'
#' @examples
#' win.text.out = windowedTextConversationAnalysis(inputData=sample_transcript_sentiment_aws, 
#' inputType="transcript", meetingId="batchMeetingId", speakerId="userName", sentMethod="aws", 
#' timeVar="utteranceStartSeconds", windowSize=600)
windowedTextConversationAnalysis = function(inputData, inputType, meetingId, speakerId, sentMethod="none", timeVar="automatic", windowSize) {
  
  if(timeVar=="automatic") {
   
    if(inputType=="transcript") timeVar="utteranceStartSeconds"
    else if(inputType=="chat") timeVar="messageSeconds"
  }
  
  inputData = dplyr::arrange(inputData, get(meetingId), get(timeVar))  
  ##### WINDOWING NEEDS TO HAPPEN IN A MEETING-BY-MEETING SEQUENCE #####
  
  uniqueMeetings = unique(inputData[,meetingId])
  if(length(uniqueMeetings) == 1) pbMin=0 else pbMin=1  
  pb = utils::txtProgressBar(min=pbMin, max=length(uniqueMeetings), style=3)
  for(m in 1:length(uniqueMeetings)) {
    utils::setTxtProgressBar(pb, m)    
    thisMeetingId = uniqueMeetings[m]
    meetingData = inputData[inputData[,meetingId] == thisMeetingId, ]
    
    # Add the windowing indicators to the inputData
    t.out.windowed = makeTimeWindows(inputData=meetingData, timeVar=timeVar, windowSize=windowSize)
    meetingData = t.out.windowed[[1]] 
    
    fullSet = cbind(t.out.windowed[[2]], sort(rep(unique(meetingData[, speakerId]), max(t.out.windowed[[2]]$windowId))))
    fullSet = fullSet[order(fullSet$windowId), ]
    names(fullSet)[4] = speakerId
    fullSet[,meetingId] = thisMeetingId 
    
    # Now, go through each of the time windows and run the
    # conversation analysis
    grp.res.out = NULL
    ind.res.out = NULL
    
    for(win in 1:max(fullSet$windowId)) {
      
      windowed.input = meetingData[meetingData$windowId == win, ]
      
      # run the analysis if there are any pieces of text in this window
      if(nrow(windowed.input) > 0) {
        
        res.line = textConversationAnalysis(inputData=windowed.input, inputType=inputType, meetingId=meetingId, speakerId=speakerId, sentMethod=sentMethod)
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
    
    if(inputType=="transcript") {
      ind.fixVars = c("utteranceTimeWindow_sum", "numUtterances") 
      grp.fixVars = c(ind.fixVars, "totalTranscriptTime", "silentTime_sum", "numUniqueSpeakers")
    } else if(inputType=="chat") {
      ind.fixVars = c("numMessages", "messageNumChars_sum")
      grp.fixVars = c(ind.fixVars, "numUniqueMessagers", "totalChatTime")
    }
    
    # Add these to the full time window datasets
    indFull = merge(fullSet, ind.res.out, by=c(meetingId, speakerId, "windowId"), all.x=T)
    indFull[,ind.fixVars] = lapply(indFull[,ind.fixVars], function(x) ifelse(is.na(x), 0, x))
    
    grp1 = merge(t.out.windowed[[2]], grp.res.out, by=c("windowId"), all.x=T)
    grp1[,meetingId] = thisMeetingId
    grp1[, grp.fixVars] = lapply(grp1[,grp.fixVars], function(x) ifelse(is.na(x), 0, x))	
    
    ### that concludes the single meeting ###
    if(!exists("full.grp")) {
      full.grp = grp1
      full.ind = indFull
    } else {
      full.grp = rbind(full.grp, grp1)
      full.ind = rbind(full.ind, indFull)
    }

  }
  close(pb)
  return(list("windowlevel" = full.grp, "speakerlevel" = full.ind))
}

#' Analyze conversation attributes
#' This function takes in the output of one of the other functions (either processZoomChat or processZoomTranscript) 
#' and produces a set of conversation measures.
#' @param inputData data.frame
#' @param inputType character
#' @param speakerId character
#' @param doSentiment boolean
#' @param sentiDone boolean
#'
#' @return 
#' A list of two data.frames, with names conditional on your choice to analyze
#' a parsed transcript file or a parsed chat file. The first list item contains
#' statistics at the corpus level. The second list item contains statistics
#' at the speaker/messager level of analysis.
#' @export
#'
#' @examples
#' convo.out = textConversationAnalysis(inputData=sample_transcript_processed, 
#' inputType='transcript', speakerId='userName', doSentiment=FALSE, sentiDone=FALSE)
textConversationAnalysis = function(inputData, inputType, speakerId, doSentiment=FALSE, sentiDone=FALSE) {

    ########################################
  # IF THE USER REQUESTED AN ANALYSIS OF A TRANSCRIPT FILE, DO THE FOLLOWING
  ########################################			
  
  if(inputType=="transcript") {
    
    ########################################
    # Create a base transcript-level output for this transcript
    ########################################
    # Calculate the gap between one utterance and the prior one
    inputData$utteranceGap = as.numeric(inputData$utteranceStartTime-dplyr::lag(inputData$utteranceEndTime))
    
    raw.dt = data.table::data.table(inputData)
    agg.tr = data.frame(raw.dt[, list(transcriptStartTime = min(utteranceStartTime), transcriptEndTime = max(utteranceEndTime),
                                   utteranceTimeWindow_sum = sum(utteranceTimeWindow), utteranceTimeWindow_x = mean(utteranceTimeWindow, na.rm=T), utteranceTimeWindow_sd = mean(utteranceTimeWindow, na.rm=T), utteranceGap_x = mean(utteranceGap, na.rm=T), utteranceGap_sd = sd(utteranceGap, na.rm=T), numUtterances = .N, numUniqueSpeakers = uniqueN(get(speakerId)))])
    agg.tr$totalRecordedTime = as.numeric(difftime(agg.tr$transcriptEndTime, agg.tr$transcriptStartTime, units="secs"))
    agg.tr$silentTime_sum = agg.tr$totalRecordedTime-agg.tr$utteranceTimeWindow_sum
    agg.tr$burstinessRaw = (agg.tr$utteranceGap_sd - agg.tr$utteranceGap_x) / (agg.tr$utteranceGap_sd + agg.tr$utteranceGap_x)
    ########################################
    # Create a base individual-level output for this transcript
    ########################################
    agg.ind = data.frame(raw.dt[, list(firstUtteranceTime = min(utteranceStartTime), lastUtteranceTime = max(utteranceStartTime),
                                    utteranceTimeWindow_sum = sum(utteranceTimeWindow), utteranceTimeWindow_x = mean(utteranceTimeWindow, na.rm=T), utteranceTimeWindow_sd = mean(utteranceTimeWindow, na.rm=T), utteranceGap_x = mean(utteranceGap, na.rm=T), utteranceGap_sd = sd(utteranceGap, na.rm=T), numUtterances = .N), by=list(get(speakerId))])
    names(agg.ind)[1] = speakerId
    
    ########################################
    # Address the sentiment analysis
    ########################################		
    
    if(doSentiment) {
      
      # If sentiment is not already done, we need to do it
      if(!sentiDone) {
        inputData = textSentiment(inputData=inputData, idVar="utteranceId", textVar="utteranceMessage", languageCodeVar="utteranceLanguage")
      }
      
      aggSentimentOut = aggSentiment(inputData, speakerId)
      text.out.tr = cbind(agg.tr, aggSentimentOut[[1]])
      text.out.ind = merge(agg.ind, aggSentimentOut[[2]], by=speakerId)
    } else {
      text.out.tr = agg.tr
      text.out.ind = agg.ind
    }
    res.out = list("TRANSCRIPT-LEVEL" = text.out.tr, "SPEAKER-LEVEL" = text.out.ind)
    
    ########################################
    # IF THE USER REQUESTED AN ANALYSIS OF A CHAT FILE, DO THE FOLLOWING
    ########################################		
    
  } else if(inputType=="chat") {
    
    ########################################
    # Create a base chat-level output
    ########################################
    inputData$messageNumChars = nchar(inputData$message)
    # Calculate the gap between one message and the prior one
    inputData$messageGap = as.numeric(inputData$messageTime-dplyr::lag(inputData$messageTime))		
    raw.dt = data.table::data.table(inputData)
    agg.ch = data.frame(raw.dt[, .(chatStartTime = min(messageTime), chatEndTime = max(messageTime), messageNumChars_sum = sum(messageNumChars), messageNumChars_x = mean(messageNumChars), messageNumChars_sd = sd(messageNumChars), messageGap_x = mean(messageGap, na.rm=T), messageGap_sd = sd(messageGap, na.rm=T), numUniqueMessagers = uniqueN(get(speakerId)), numMessages = .N)])
    agg.ch$totalRecordedTime = as.numeric(difftime(agg.ch$chatEndTime, agg.ch$chatStartTime, units="secs"))
    agg.ch$burstinessRaw = (agg.ch$messageGap_sd - agg.ch$messageGap_x) / (agg.ch$messageGap_sd + agg.ch$messageGap_x)
    
    ########################################
    # Create a base individual-level output for this transcript
    ########################################
    agg.ind = data.frame(raw.dt[, .(numMessages = .N, firstMessageTime = min(messageTime), lastMessageTime = max(messageTime), messageNumChars_sum = sum(messageNumChars, na.rm=T), messageNumChars_x = mean(messageNumChars, na.rm=T), messageNumChars_sd = sd(messageNumChars, na.rm=T), messageGap_x = mean(messageGap, na.rm=T), messageGap_sd = sd(messageGap, na.rm=T)), by=list(get(speakerId))])
    names(agg.ind)[1] = speakerId
    
    ########################################
    # Address the sentiment analysis
    ########################################				
    if(doSentiment) {
      
      if(!sentiDone) {
        inputData = textSentiment(inputData = inputData, idVar="messageId", textVar = "message", languageCodeVar="messageLanguage")
      }
      
      aggSentimentOut = aggSentiment(inputData, speakerId)
      text.out.ch = cbind(agg.ch, aggSentimentOut[[1]])
      text.out.ind = merge(agg.ind, aggSentimentOut[[2]], by=speakerId)			
    } else {
      text.out.ch = agg.ch
      text.out.ind = agg.ind
    }
    res.out = list("CHAT-LEVEL" = text.out.ch, "USER-LEVEL" = text.out.ind)			
  }
}
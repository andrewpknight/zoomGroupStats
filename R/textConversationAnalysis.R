#' Analyze conversation attributes
#' 
#' This function takes in the output of one of the other functions (either processZoomChat or processZoomTranscript) 
#' and produces a set of conversation measures.
#' @param inputData data.frame that is the output of either processZoomChat or processZoomTranscript
#' @param inputType string of either 'transcript' or 'chat'
#' @param meetingId string giving the name of the variable with the meetingId
#' @param speakerId string giving the name of the identifier for the individual who made this contribution
#' @param sentMethod string giving the type of sentiment analysis to include, either 'aws' or 'syuzhet'
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
#' inputType='transcript', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="none")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_transcript_sentiment_syu, 
#' inputType='transcript', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="syuzhet")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_chat_sentiment_aws, 
#' inputType='chat', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="aws")
#' 
#' \dontrun{
#' convo.out = textConversationAnalysis(inputData=sample_transcript_sentiment_aws, 
#' inputType='transcript', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="aws")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_transcript_sentiment_syu, 
#' inputType='transcript', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="syuzhet")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_chat_processed, 
#' inputType='chat', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="none")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_chat_sentiment_aws, 
#' inputType='chat', meetingId='batchMeetingId', 
#' speakerId='userName', sentMethod="aws")
#' 
#' convo.out = textConversationAnalysis(inputData=sample_chat_sentiment_syu, 
#' inputType='chat',meetingId='batchMeetingId',  
#' speakerId='userName', sentMethod="syuzhet")
#' }
textConversationAnalysis = function(inputData, inputType, meetingId, speakerId, sentMethod="none") {
  
  utteranceStartTime<-utteranceStartSeconds<-utteranceEndTime<-utteranceTimeWindow<-utteranceGap<-sd<-messageTime<-messageNumChars<-messageGap<-NULL
  
  ########################################
  # IF THE USER REQUESTED AN ANALYSIS OF A TRANSCRIPT FILE, DO THE FOLLOWING
  ########################################			
  
  if(inputType=="transcript") {
    inputData = dplyr::arrange(inputData, get(meetingId), utteranceStartSeconds)    

    ########################################
    # Create a base transcript-level output for this transcript
    ########################################
    # Calculate the gap between one utterance and the prior one
    inputData$utteranceGap = as.numeric(inputData$utteranceStartTime-dplyr::lag(inputData$utteranceEndTime))
    # Make sure this is missing if the proximal utterances are from different meetings
    inputData$utteranceGap = ifelse(dplyr::lag(inputData[,meetingId]) != inputData[,meetingId], NA,inputData$utteranceGap)
    
    raw.dt = data.table::data.table(inputData)
    agg.tr = data.frame(raw.dt[, list(transcriptStartTime = min(utteranceStartTime), transcriptEndTime = max(utteranceEndTime),
                                      utteranceTimeWindow_sum = sum(utteranceTimeWindow), utteranceTimeWindow_x = mean(utteranceTimeWindow, na.rm=T), utteranceTimeWindow_sd = mean(utteranceTimeWindow, na.rm=T), utteranceGap_x = mean(utteranceGap, na.rm=T), utteranceGap_sd = sd(utteranceGap, na.rm=T), numUtterances = .N, numUniqueSpeakers = uniqueN(get(speakerId))), by=list(get(meetingId))])
    names(agg.tr)[1] = meetingId
    agg.tr$totalTranscriptTime = as.numeric(difftime(agg.tr$transcriptEndTime, agg.tr$transcriptStartTime, units="secs"))
    agg.tr$silentTime_sum = agg.tr$totalTranscriptTime-agg.tr$utteranceTimeWindow_sum
    agg.tr$burstinessRaw = (agg.tr$utteranceGap_sd - agg.tr$utteranceGap_x) / (agg.tr$utteranceGap_sd + agg.tr$utteranceGap_x)
    ########################################
    # Create a base individual-level output for this transcript
    ########################################
    agg.ind = data.frame(raw.dt[, list(firstUtteranceTime = min(utteranceStartTime), lastUtteranceTime = max(utteranceStartTime),
                                       utteranceTimeWindow_sum = sum(utteranceTimeWindow), utteranceTimeWindow_x = mean(utteranceTimeWindow, na.rm=T), utteranceTimeWindow_sd = mean(utteranceTimeWindow, na.rm=T), utteranceGap_x = mean(utteranceGap, na.rm=T), utteranceGap_sd = sd(utteranceGap, na.rm=T), numUtterances = .N), by=list(get(meetingId), get(speakerId))])
    names(agg.ind)[1:2] = c(meetingId, speakerId)
    
    ########################################
    # Address the sentiment analysis
    ########################################		
    
    if(sentMethod %in% c("aws", "syuzhet")) {
      aggSentiment.tr = aggSentiment(inputData, meetingId=meetingId, speakerId=NULL, sentMethod)
      aggSentiment.ind = aggSentiment(inputData, meetingId=meetingId, speakerId=speakerId, sentMethod)      
      text.out.tr = merge(agg.tr, aggSentiment.tr, by=meetingId)
      text.out.ind = merge(agg.ind, aggSentiment.ind, by=c(meetingId, speakerId))
    } else {
      text.out.tr = agg.tr
      text.out.ind = agg.ind
    }
    text.out.tr = dplyr::arrange(text.out.tr, get(meetingId))            
    text.out.ind = dplyr::arrange(text.out.ind, get(meetingId), get(speakerId))
    
    res.out = list("transcriptlevel" = text.out.tr, "speakerlevel" = text.out.ind)
    
    ########################################
    # IF THE USER REQUESTED AN ANALYSIS OF A CHAT FILE, DO THE FOLLOWING
    ########################################		
    
  } else if(inputType=="chat") {
    inputData = dplyr::arrange(inputData, get(meetingId), messageTime)        

    ########################################
    # Create a base chat-level output
    ########################################
    inputData$messageNumChars = nchar(inputData$message)
    # Calculate the gap between one message and the prior one
    inputData$messageGap = as.numeric(inputData$messageTime-dplyr::lag(inputData$messageTime))	
    # Make sure this is missing if the proximal utterances are from different meetings
    inputData$messageGap = ifelse(dplyr::lag(inputData[,meetingId]) != inputData[,meetingId], NA,inputData$messageGap)    
    
    raw.dt = data.table::data.table(inputData)
    agg.ch = data.frame(raw.dt[, list(chatStartTime = min(messageTime), chatEndTime = max(messageTime), messageNumChars_sum = sum(messageNumChars), messageNumChars_x = mean(messageNumChars), messageNumChars_sd = sd(messageNumChars), messageGap_x = mean(messageGap, na.rm=T), messageGap_sd = sd(messageGap, na.rm=T), numUniqueMessagers = uniqueN(get(speakerId)), numMessages = .N), by=list(get(meetingId))])
    names(agg.ch)[1] = meetingId
    agg.ch$totalChatTime = as.numeric(difftime(agg.ch$chatEndTime, agg.ch$chatStartTime, units="secs"))
    agg.ch$burstinessRaw = (agg.ch$messageGap_sd - agg.ch$messageGap_x) / (agg.ch$messageGap_sd + agg.ch$messageGap_x)
    
    ########################################
    # Create a base individual-level output for this transcript
    ########################################
    agg.ind = data.frame(raw.dt[, list(numMessages = .N, firstMessageTime = min(messageTime), lastMessageTime = max(messageTime), messageNumChars_sum = sum(messageNumChars, na.rm=T), messageNumChars_x = mean(messageNumChars, na.rm=T), messageNumChars_sd = sd(messageNumChars, na.rm=T), messageGap_x = mean(messageGap, na.rm=T), messageGap_sd = sd(messageGap, na.rm=T)), by=list(get(meetingId), get(speakerId))])
    names(agg.ind)[1:2] = c(meetingId, speakerId)
    
    ########################################
    # Address the sentiment analysis
    ########################################				
    if(sentMethod %in% c("aws", "syuzhet")) {
      aggSentiment.ch = aggSentiment(inputData, meetingId = meetingId, speakerId = NULL, sentMethod)
      aggSentiment.ind = aggSentiment(inputData, meetingId = meetingId, speakerId = speakerId, sentMethod)      
      text.out.ch = merge(agg.ch, aggSentiment.ch, by=meetingId)
      text.out.ind = merge(agg.ind, aggSentiment.ind, by=c(meetingId, speakerId))
    } else {
      text.out.ch = agg.ch
      text.out.ind = agg.ind
    }
    text.out.ch = dplyr::arrange(text.out.ch, get(meetingId))            
    text.out.ind = dplyr::arrange(text.out.ind, get(meetingId), get(speakerId))                
    res.out = list("chatlevel" = text.out.ch, "userlevel" = text.out.ind)			
  }
}
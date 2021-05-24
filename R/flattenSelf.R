#' Combine transcript or chat files into conversational turns
#'
#' Different transcription algorithms mark utterances in different ways. This function 
#' will combine consecutive utterances (or messages) by the same person into conversational 
#' turns. The user can also specify a gap in between messages to use to determine
#' whether they should be combined or not.  
#' @param inputData data.frame output from either processZoomTranscript or processZoomChat
#' @param inputType either 'transcript' or 'chat'
#' @param meetingId character giving the name of the meeting identifier variable
#' @param speakerId character giving the name of the speaker identifier variable
#' @param gapLength integer giving the number of seconds for marking distinct turns by the same speaker. Consecutive utterances by the same speaker of greater than or equal to this value will be treated as different conversational turns. 
#'
#' @return a data.frame that is the same format as inputData, but where the observations are the new, compressed conversational turns. 
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' newChat = flattenSelf(inputData = 
#' sample_chat_processed[sample_chat_processed$batchMeetingId=="00000000001", ], 
#' inputType="chat", meetingId = "batchMeetingId", 
#' speakerId="userName", gapLength=120)
#' 
#' newTranscript = flattenSelf(inputData = 
#' sample_transcript_processed[sample_transcript_processed$batchMeetingId=="00000000001", ], 
#' inputType="transcript", meetingId = "batchMeetingId", 
#' speakerId="userName", gapLength=120)
#' 
flattenSelf = function(inputData, inputType, meetingId, speakerId, gapLength) {
  #Create lagged variables
  inputData$speakerCurrent = inputData[,speakerId]
  if(inputType == "transcript") {
    inputData = inputData[order(inputData$utteranceEndSeconds), ]
    inputData[, c("speakerBefore", "priorUtteranceEndSeconds")] = dplyr::lag(inputData[, c("speakerCurrent", "utteranceEndSeconds")])
    inputData$turnGap = inputData$utteranceStartSeconds - inputData$priorUtteranceEndSeconds
    
    inputData$speakerChange = ifelse(inputData$speakerCurrent != inputData$speakerBefore, TRUE, FALSE)
    inputData$longGap = ifelse(inputData$turnGap >= gapLength, TRUE, FALSE)
    inputData$newTurn = ifelse(inputData$speakerChange | inputData$longGap | is.na(inputData$speakerBefore), TRUE, FALSE)
    turnId = 0
    for(r in 1:nrow(inputData)) {
      if(inputData[r, "newTurn"]) {
        turnId = turnId + 1
      }
      inputData[r, "turnId"] = turnId
    }
    
    turnData = inputData %>%
      dplyr::group_by(turnId) %>%
      dplyr::summarise(utteranceId = unique(.data$turnId), utteranceStartSeconds = min(.data$utteranceStartSeconds), utteranceStartTime = min(.data$utteranceStartTime), utteranceEndSeconds = max(.data$utteranceEndSeconds),utteranceEndTime = max(.data$utteranceEndTime), utteranceTimeWindow = (max(.data$utteranceEndSeconds)-min(.data$utteranceStartSeconds)), {{speakerId}} := unique(.data[[speakerId]]), utteranceMessage = paste(.data$utteranceMessage, collapse = " "), utteranceLanguage=unique(.data$utteranceLanguage), {{meetingId}} := unique(.data[[meetingId]]))		
    
    
  } else if(inputType == "chat") {
    inputData = inputData[order(inputData$messageTime), ]  
    inputData[, c("speakerBefore", "priorMessageTime")] = dplyr::lag(inputData[, c("speakerCurrent", "messageTime")])
    inputData$turnGap = as.numeric(difftime(inputData$messageTime, inputData$priorMessageTime, units="secs"))
    
    inputData$speakerChange = ifelse(inputData$speakerCurrent != inputData$speakerBefore, TRUE, FALSE)
    inputData$longGap = ifelse(inputData$turnGap >= gapLength, TRUE, FALSE)
    inputData$newTurn = ifelse(inputData$speakerChange | inputData$longGap | is.na(inputData$speakerBefore), TRUE, FALSE)
    turnId = 0
    for(r in 1:nrow(inputData)) {
      if(inputData[r, "newTurn"]) {
        turnId = turnId + 1
      }
      inputData[r, "turnId"] = turnId
    }
    
    turnData = inputData %>%
      dplyr::group_by(turnId) %>%
      dplyr::summarise(messageId = unique(.data$turnId), messageSeconds = min(.data$messageSeconds), messageTime = min(.data$messageTime), {{speakerId}} := unique(.data[[speakerId]]), message = paste(.data$message, collapse = " "), messageLanguage=unique(.data$messageLanguage), {{meetingId}} := unique(.data[[meetingId]]))		
  }
  colnames(turnData)[which(names(turnData) == "speakerId")] = speakerId 
  turnData = data.frame(turnData)
  return(turnData)
}
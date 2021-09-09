#' Wrapper to run the flattenSelf function on a batch of meetings
#'
#' @param inputData data.frame with multiple meetings from batchProcessZoomOutput (either transcript or chat)
#' @param inputType character indicating 'transcript' or 'chat'
#' @param meetingId character name of the variable containing the meeting identifier
#' @param speakerId character name of the variable containing the speaker identifier
#' @param gapLength integer giving the number of seconds for marking distinct turns by the same speaker. Consecutive utterances by the same speaker of greater than or equal to this value will be treated as different conversational turns.
#'
#' @return a data.frame that is the same format as inputData, but where the observations are the new, compressed conversational turns. 
#' 
#' @export
#' 
#' @importFrom rlang .data
#'
#' @examples
#' newChat = batchFlattenSelf(inputData = sample_chat_processed, 
#' inputType="chat", meetingId = "batchMeetingId", 
#' speakerId="userName", gapLength=120)
#' 
#' newTranscript = batchFlattenSelf(inputData = sample_transcript_processed, 
#' inputType="transcript", meetingId = "batchMeetingId", 
#' speakerId="userName", gapLength=120)

batchFlattenSelf = function(inputData, inputType, meetingId, speakerId, gapLength) {
  
  meets = unique(inputData[,meetingId])
  for(m in 1:length(meets)) {
    
    meetData = inputData[inputData[,meetingId] == meets[m], ]
    meetTurn = flattenSelf(inputData=meetData, inputType=inputType, meetingId = meetingId, speakerId = speakerId, gapLength=gapLength)
    
    if(m == 1) {
      allOut = meetTurn
    } else {
      allOut = rbind(allOut, meetTurn)
    }
  }
  return(allOut)
}
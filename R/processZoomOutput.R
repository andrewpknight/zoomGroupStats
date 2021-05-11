#' Wrapper function to process the raw files from Zoom in a single call
#' 
#' The user provides a fileRoot that is used for a given meeting. Output 
#' files should be named as fileRoot_chat.txt; fileRoot_transcript.vtt; 
#' and fileRoot_participants.csv. Any relevant files will be processed.
#' @param fileRoot string giving the path to the files and the root
#' @param rosetta boolean to produce the rosetta file or not
#' @param sessionStartDateTime  string giving the start of the session in YYYY-MM-DD HH:MM:SS
#' @param recordingStartDateTime  string giving the start of the recording in YYYY-MM-DD HH:MM:SS 
#' @param languageCode string giving the language code
#'
#' @return a named list containing data.frames for each of the available files:
#'  \itemize{
#'     \item meetInfo - A single row with info for the meeting that is in the 
#'     participants file. Columns provide information about the meeting from the Zoom
#'     Cloud recording site.
#'     \item partInfo - Each row is a Zoom display name (with display name changes 
#'     in parentheses). Columns provide information about participants from the Zoom Cloud
#'     recording site. 
#'     \item transcript - Each row is an utterance in the audio transcript. This is the 
#'     output from processZoomTranscript. 
#'     \item chat - Each row is a message posted to the chat. This is the output 
#'     from processZoomChat.
#'     \item rosetta - Each row is a unique display name (within meeting) encountered 
#'     in the batchInput. This is used to reconcile user identities. 
#'     }
#' @export
#'
#' @examples
#' zoomOut = processZoomOutput(fileRoot=file.path(
#' system.file('extdata', package = 'zoomGroupStats'),"meeting001"
#' ), rosetta=TRUE)
#' \dontrun{
#' zoomOut = processZoomOutput(fileRoot="~/zoomMeetings/myMeeting", rosetta=TRUE)
#' }
processZoomOutput = function(fileRoot, rosetta=TRUE, sessionStartDateTime="1970-01-01 00:00:00", recordingStartDateTime="1970-01-01 00:00:00", languageCode="en") {

  out.list = list()
  
  participantsFile = paste(fileRoot, "_participants.csv", sep="")
  chatFile = paste(fileRoot, "_chat.txt", sep="")
  transcriptFile = paste(fileRoot, "_transcript.vtt", sep="")
  
  if(file.exists(participantsFile)) {
    outInfo = processZoomParticipantsInfo(participantsFile)
    out.list[["meetInfo"]] = outInfo[[1]]
    out.list[["partInfo"]] = outInfo[[2]]	
    
    
    # If there is a participants file, but there is no specified timestamps, use the datetime information included in it
    if(sessionStartDateTime == "1970-01-01 00:00:00") {
      sessionStartDateTime = outInfo[[1]]$meetingStartTime
    }
    if(recordingStartDateTime == "1970-01-01 00:00:00") {    
      recordingStartDateTime = outInfo[[1]]$meetingStartTime		
    }
  }
  
  if(file.exists(chatFile)) {
    out.list[["chat"]] = processZoomChat(fname=chatFile, sessionStartDateTime=sessionStartDateTime, languageCode=languageCode)
  }
  
  if(file.exists(transcriptFile)) {
    out.list[["transcript"]] = processZoomTranscript(fname=transcriptFile, recordingStartDateTime=recordingStartDateTime, languageCode=languageCode)
  }	
  
  if(rosetta) {
    out.list[["rosetta"]] = createZoomRosetta(out.list)
  }
  
  return(out.list)
}
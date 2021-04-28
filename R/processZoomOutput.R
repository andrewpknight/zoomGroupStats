#' Wrapper function to process the raw files from Zoom in a single call
#' The user provides a fileRoot that is used for a given meeting. Output 
#' files should be named as fileRoot_chat.txt; fileRoot_transcript.vtt; 
#' and fileRoot_participants.csv. Any relevant files will be processed.
#' @param fileRoot string giving the path to the files and the root
#' @param sessionStartDateTime  string giving the start of the session in YYYY-MM-DD HH:MM:SS
#' @param recordingStartDateTime  string giving the start of the recording in YYYY-MM-DD HH:MM:SS 
#' @param languageCode string giving the language code
#'
#' @return a named list containing data.frames for each of the available files
#'  (particiapnts, transcript, chat)
#' @export
#'
#' @examples
#' \dontrun{
#' zoomOut = processZoomOutput(fileRoot="~/zoomMeetings/myMeeting")
#' }
processZoomOutput = function(fileRoot, sessionStartDateTime="1970-01-01 00:00:00", recordingStartDateTime="1970-01-01 00:00:00", languageCode="en") {
  
  out.list = list()
  participantsFile = paste(fileRoot, "_participants.csv", sep="")
  chatFile = paste(fileRoot, "_chat.txt", sep="")
  transcriptFile = paste(fileRoot, "_transcript.vtt", sep="")
  
  if(file.exists(participantsFile)) {
    outInfo = processZoomParticipantsInfo(participantsFile)
    out.list[["meetInfo"]] = outInfo[[1]]
    out.list[["partInfo"]] = outInfo[[2]]	
    
    # If the user includes a participants file, use the datetime information included in it	
    sessionStartDateTime = outInfo[[1]]$meetingStartTime
    recordingStartDateTime = outInfo[[1]]$meetingStartTime		
  }
  
  if(file.exists(chatFile)) {
    ch.out = processZoomChat(fname=chatFile, sessionStartDateTime=sessionStartDateTime, languageCode=languageCode)
    out.list[["chat"]] = ch.out
  }
  
  if(file.exists(transcriptFile)) {
    tr.out = processZoomTranscript(fname=transcriptFile, recordingStartDateTime=recordingStartDateTime, languageCode=languageCode)
    out.list[["transcript"]] = tr.out
  }	
  return(out.list)
}
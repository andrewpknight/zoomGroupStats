#' Batch process files that have been downloaded from Zoom
#' 
#' Provide the location of a structured batchInput file and this
#' function will process a set of meetings at once. 
#'
#' @param batchInput String giving the location of the xlsx file
#' that contains the information for the zoom meetings. All corresponding
#' Zoom downloads for the meetings in the batch must be saved in the same
#' directory as the batchInput file. 
#' @param exportZoomRosetta optional string giving the path for exporting the
#' zoomRosetta file to link up unique individual IDs manually. Providing this
#' path will write the zoomRosetta file to that location.
#'
#' @return a list that has a data.frame for each of the elements
#' of a Zoom output that are available in the input directory: 
#' \itemize{
#'     \item batchInfo - Each row is a meeting included in batchInput. Columns 
#'     provide information about each meeting.
#'     \item meetInfo - Each row is a meeting for which there was a downloaded
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
#' }
#'   
#' @export
#'
#' @examples
#' batchOut = batchProcessZoomOutput(batchInput=system.file('extdata', 
#' 'myMeetingsBatch.xlsx', package = 'zoomGroupStats'), 
#' exportZoomRosetta=file.path(tempdir(),"_rosetta.xlsx"))
#' 
batchProcessZoomOutput = function(batchInput, exportZoomRosetta=NULL) {
  
  
  if(!file.exists(batchInput)) {
    stop("Cannot find the specified batchInput file: ",batchInput)
    
  }
  batchInfo = openxlsx::read.xlsx(batchInput)
  batchInfo[,c("participants_processed", "transcript_processed", "chat_processed", "video_processed")] = 0
  batchInfo$dirRoot = dirname(batchInput)
  
  ### Initialize the output frames that will be filled ###
  batchMeetInfo = data.frame(meetingId=character(), meetingTopic=character(), meetingStartTime=character(), meetingEndTime=character(), userEmail=character(), meetingDuration=integer(), numParticipants=integer(), batchMeetingId=character())
  
  batchPartInfo = data.frame(userName=character(), userEmail=character(), userDuration=integer(), userGuest=logical(), batchMeetingId=character())
  
  batchTranscript = data.frame(utteranceId=integer(), utteranceStartSeconds=numeric(), utteranceStartTime=character(), utteranceEndSeconds=numeric(), utteranceEndTime=character(), utteranceTimeWindow=numeric(), userName=character(), utteranceMessage=character(), utteranceLanguage=character(), batchMeetingId=character())
  batchTranscript$utteranceStartTime = as.POSIXct(batchTranscript$utteranceStartTime)
  batchTranscript$utteranceEndTime = as.POSIXct(batchTranscript$utteranceEndTime)	
  
  batchChat = data.frame(messageId=integer(), messageSeconds=numeric(), messageTime=character(), userName=character(), message=character(), messageLanguage=character(), batchMeetingId=character())
  batchChat$messageTime = as.POSIXct(batchChat$messageTime)	
  
  batchRosetta = data.frame(userName=character(), userEmail=character(), batchMeetingId=character())
  message("Processing any correctly named Zoom downloads in ",dirname(batchInput))	
  if(nrow(batchInfo) == 1) pbMin=0 else pbMin=1  
  pb = utils::txtProgressBar(min=pbMin, max=nrow(batchInfo), style=3)  
  for(r in 1:nrow(batchInfo)) {
    utils::setTxtProgressBar(pb, r)    
    zoomOut = processZoomOutput(file.path(dirname(batchInput),batchInfo[r, "fileRoot"]), sessionStartDateTime=batchInfo[r, "sessionStartDateTime"], recordingStartDateTime=batchInfo[r, "recordingStartDateTime"])
    
    if(!is.null(zoomOut$meetInfo)) {
      zoomOut$meetInfo$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchMeetInfo = rbind(batchMeetInfo, zoomOut$meetInfo)
    }
    
    if(!is.null(zoomOut$partInfo)) {
      zoomOut$partInfo$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchPartInfo = rbind(batchPartInfo, zoomOut$partInfo)
      batchInfo$participants_processed = 1
    }
    
    if(!is.null(zoomOut$transcript)) {
      zoomOut$transcript$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchTranscript = rbind(batchTranscript, zoomOut$transcript)
      batchInfo$transcript_processed = 1      
    }		
    
    if(!is.null(zoomOut$chat)) {
      zoomOut$chat$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchChat = rbind(batchChat, zoomOut$chat)
      batchInfo$chat_processed = 1            
    }
    
    if(!is.null(zoomOut$rosetta) && nrow(zoomOut$rosetta) > 0) {
      zoomOut$rosetta$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchRosetta = rbind(batchRosetta, zoomOut$rosetta)
    }	
    
    
  }
  close(pb)
  batchOut = list("batchInfo" = batchInfo, "meetInfo" = batchMeetInfo, "partInfo" = batchPartInfo, "transcript" = batchTranscript, "chat" = batchChat, "rosetta" = batchRosetta)
  if(!is.null(exportZoomRosetta)) {
    openxlsx::write.xlsx(batchOut$rosetta, exportZoomRosetta)      	
  }  
  
  return(batchOut)
}

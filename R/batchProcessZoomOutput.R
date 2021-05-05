#' Batch process files that have been downloaded from Zoom
#' 
#' Provide the location of a structured batchInput file and this
#' function will process a set of meetings at once. 
#'
#' @param batchInput String giving the location of the xlsx file
#' that contains the information for the zoom meetings
#' @param exportZoomRosetta string giving the path for exporting the
#' zoomRosetta file to link up unique individual IDs manually
#'
#' @return a list that has a list item for each of the elements
#' of a Zoom output that are available--batchInfo, meetInfo, partInfo, 
#' transcript, chat, and rosetta
#' @export
#'
#' @examples
#' batchOut = batchProcessZoomOutput(batchInput=system.file('extdata', 
#' 'myMeetingsBatch.xlsx', package = 'zoomGroupStats'))
batchProcessZoomOutput = function(batchInput, exportZoomRosetta=NULL) {
  batchInfo = openxlsx::read.xlsx(batchInput)
  
  ### Initialize the output frames that will be filled ###
  batchMeetInfo = data.frame(meetingId=character(), meetingTopic=character(), meetingStartTime=character(), meetingEndTime=character(), userEmail=character(), meetingDuration=integer(), numParticipants=integer(), batchMeetingId=character())
  
  batchPartInfo = data.frame(userName=character(), userEmail=character(), userDuration=integer(), userGuest=logical(), batchMeetingId=character())
  
  batchTranscript = data.frame(utteranceId=integer(), utteranceStartSeconds=numeric(), utteranceStartTime=character(), utteranceEndSeconds=numeric(), utteranceEndTime=character(), utteranceTimeWindow=numeric(), userName=character(), utteranceMessage=character(), utteranceLanguage=character(), batchMeetingId=character())
  batchTranscript$utteranceStartTime = as.POSIXct(batchTranscript$utteranceStartTime)
  batchTranscript$utteranceEndTime = as.POSIXct(batchTranscript$utteranceEndTime)	
  
  batchChat = data.frame(messageId=integer(), messageSeconds=numeric(), messageTime=character(), userName=character(), message=character(), messageLanguage=character(), batchMeetingId=character())
  batchChat$messageTime = as.POSIXct(batchChat$messageTime)	
  
  batchRosetta = data.frame(userName=character(), userEmail=character(), batchMeetingId=character())	
  for(r in 1:nrow(batchInfo)) {
    
    zoomOut = processZoomOutput(file.path(dirname(batchInput),batchInfo[r, "fileRoot"]))
    
    if(!is.null(zoomOut$meetInfo)) {
      zoomOut$meetInfo$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchMeetInfo = rbind(batchMeetInfo, zoomOut$meetInfo)
    }
    
    if(!is.null(zoomOut$partInfo)) {
      zoomOut$partInfo$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchPartInfo = rbind(batchPartInfo, zoomOut$partInfo)
    }
    
    if(!is.null(zoomOut$transcript)) {
      zoomOut$transcript$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchTranscript = rbind(batchTranscript, zoomOut$transcript)
    }		
    
    if(!is.null(zoomOut$chat)) {
      zoomOut$chat$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchChat = rbind(batchChat, zoomOut$chat)
    }
    
    if(!is.null(zoomOut$rosetta)) {
      zoomOut$rosetta$batchMeetingId = batchInfo[r, "batchMeetingId"]
      batchRosetta = rbind(batchRosetta, zoomOut$rosetta)
    
      if(!is.null(exportZoomRosetta)) {
        openxlsx::write.xlsx(zoomOut$rosetta, exportZoomRosetta)      	
      }
    }	
    
  }
  
  batchOut = list("batchInfo" = batchInfo, "meetInfo" = batchMeetInfo, "partInfo" = batchPartInfo, "transcript" = batchTranscript, "chat" = batchChat, "rosetta" = batchRosetta)
  return(batchOut)
}
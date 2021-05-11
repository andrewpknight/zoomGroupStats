#' Helper function to add unique identifiers to processed Zoom downloads
#' 
#' Import an edited zoomRosetta file that tells how to 
#' link up Zoom display names to some unique individual
#' identifier
#'
#' @param zoomOutput the output of batchProcessZoomOutput
#' @param zoomRosetta the path to an edited zoomRosetta xlsx
#' @param meetingId the name of the meetingId you want to use
#'
#' @return returns zoomOutput with identifiers in zoomRosetta
#' merged to any available data.frames in the zoomOutput file 
#' @export
#'
#' @examples
#' batchOutIds = importZoomRosetta(zoomOutput=
#' batchProcessZoomOutput(batchInput=system.file('extdata', 
#' 'myMeetingsBatch.xlsx', package = 'zoomGroupStats')), 
#' zoomRosetta=system.file('extdata', 
#' 'myMeetingsBatch_rosetta_edited.xlsx', package = 'zoomGroupStats'), 
#' meetingId="batchMeetingId")
#' 
#' \dontrun{
#' batchOutIds = importZoomRosetta(zoomOutput=batchOut, zoomRosetta="myEditedRosetta.xlsx", 
#' meetingId="batchMeetingId")
#' }
importZoomRosetta = function(zoomOutput, zoomRosetta, meetingId) {
  zoomRosettaUpdate = openxlsx::read.xlsx(zoomRosetta)
  zoomOutput$rosetta=zoomRosettaUpdate
  if(!is.null(zoomOutput$transcript)) {
    zoomOutput$transcript = merge(zoomOutput$transcript, zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T) 
    zoomOutput$transcript = zoomOutput$transcript[
      with(zoomOutput$transcript, 
           order(zoomOutput$transcript[,meetingId], zoomOutput$transcript$utteranceId)), ]    
  }
  if(!is.null(zoomOutput$chat)) {
    zoomOutput$chat = merge(zoomOutput$chat, zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T)
    zoomOutput$chat = zoomOutput$chat[
      with(zoomOutput$chat, 
           order(zoomOutput$chat[,meetingId], zoomOutput$chat$messageId)), ]    
    
  }
  if(!is.null(zoomOutput$partInfo)) {    
    zoomOutput$partInfo = merge(zoomOutput$partInfo[,c(meetingId, "userName", "userGuest")], zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T)
  }
  return(zoomOutput)
}

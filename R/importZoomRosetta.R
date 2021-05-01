#' Import an edited zoomRosetta file that tells how to 
#' link up zoom display names to some unique individual
#' identifier
#'
#' @param zoomOutput the output of batchProcessZoomOutput
#' @param zoomRosetta the path to an edited zoomRosetta
#' @param meetingId the name of the meetingId you want to use
#'
#' @return returns zoomOutput with the identifiers in zoomRosetta
#' merged to each of the list items if they are available
#' @export
#'
#' @examples
#' \dontrun{
#' importZoomRosetta(zoomOutput=batchOut, zoomRosetta="myEditedRosetta.xlsx", 
#' meetingId="batchMeetingId")
#' }
importZoomRosetta = function(zoomOutput, zoomRosetta, meetingId) {
  zoomRosettaUpdate = openxlsx::read.xlsx(zoomRosetta)
  zoomOutput$rosetta=zoomRosettaUpdate
  if(!is.null(zoomOutput$transcript)) {
    zoomOutput$transcript = merge(zoomOutput$transcript, zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T)    
  }
  if(!is.null(zoomOutput$chat)) {
    zoomOutput$chat = merge(zoomOutput$chat, zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T)
  }
  if(!is.null(zoomOutput$partInfo)) {    
    zoomOutput$partInfo = merge(zoomOutput$partInfo[,c(meetingId, "userName", "userGuest")], zoomRosettaUpdate, by=c(meetingId, "userName"), all.x=T)
  }
  return(zoomOutput)
}

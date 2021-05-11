#' Process participant information from a Zoom meeting export
#'
#' This function parses the information from the downloadable meeting information file in Zooms reports section.
#' The function presumes that you have checked the box to
#' include the meeting information in the file.
#' That means that there is a header (2 rows) containing the zoom meeting information.
#' Following that header are four columns:
#' Name of user, user email, total duration, and guest.
#'
#' @param inputPath character
#'
#' @return list of two data.frames with parsed information from the downloadable 
#' Zoom participants file
#' \itemize{
#'     \item meetInfo - provides the meeting level information that Zoom Cloud gives
#'     \item partInfo - provides the participant level information that Zoom Cloud gives
#'}
#' @export
#'
#' @examples
#' partInfo = processZoomParticipantsInfo(
#' system.file('extdata', "meeting001_participants.csv", package = 'zoomGroupStats')
#' )
processZoomParticipantsInfo = function(inputPath) {
  
  meetInfo = utils::read.table(inputPath, header=F, nrows=1, skip=1, sep=",", stringsAsFactors=F, colClasses=c(rep("character", 5), "numeric", "numeric", "character"))
  meetInfo = meetInfo[,1:7]
  names(meetInfo) = c("meetingId", "meetingTopic", "meetingStartTime", "meetingEndTime", "userEmail", "meetingDuration", "numParticipants")
  
  # Change the date column to something more useable in the other functions
  meetInfo$meetingStartTime =  as.character(lubridate::parse_date_time(meetInfo$meetingStartTime, "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))
  meetInfo$meetingEndTime = as.character(lubridate::parse_date_time(meetInfo$meetingEndTime, "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone()))
  
  partInfo = data.frame(utils::read.delim(inputPath, header=T, skip=3, sep=",", stringsAsFactors=F))
  partInfo = partInfo[,1:4]
  names(partInfo) = c("userName", "userEmail", "userDuration", "userGuest")
  
  outInfo = list(meetInfo = meetInfo, partInfo = partInfo)
  
  return(outInfo)
}
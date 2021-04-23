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
#' @return list
#' @export
#'
#' @examples
#' partInfo = processZoomParticipantsInfo(
#' system.file('extdata', "sample_participants.csv", package = 'zoomGroupStats')
#' )
processZoomParticipantsInfo = function(inputPath) {

  # convDate transforms the Zoom output for a timestamp into
  # ISO 8601, but without the "T" delimiter: YYYY-MM-DD HH:MM:SS
  convDate = function(oldDate) {
    month = substr(oldDate,1,2)
    day = substr(oldDate,4,5)
    year = substr(oldDate,7,10)
    hour = substr(oldDate,12,13)
    min = substr(oldDate,15,16)
    sec = substr(oldDate,18,19)
    tod = substr(oldDate,21,22)
    if(tod == "PM" && as.numeric(hour) < 12) {
      hour = as.character((as.numeric(hour) + 12))
    }
    newDate = paste(year,"-", month, "-", day, " ", hour,":",min,":",sec, sep="")
    return(newDate)
  }

  meetInfo = utils::read.table(inputPath, header=F, nrows=1, skip=1, sep=",", stringsAsFactors=F)
  meetInfo = meetInfo[,1:7]
  names(meetInfo) = c("meetingId", "meetingTopic", "meetingStartTime", "meetingEndTime", "userEmail", "meetingDuration", "numParticipants")


  # Change the date column to something more useable in the other functions
  meetInfo$meetingStartTime = convDate(meetInfo$meetingStartTime)
  meetInfo$meetingEndTime = convDate(meetInfo$meetingEndTime)

  partInfo = data.frame(utils::read.delim(inputPath, header=T, skip=3, sep=",", stringsAsFactors=F))
  partInfo = partInfo[,1:4]
  names(partInfo) = c("userName", "userEmail", "userDuration", "userGuest")

  outInfo = list(meetInfo = meetInfo, partInfo = partInfo)

  return(outInfo)
}

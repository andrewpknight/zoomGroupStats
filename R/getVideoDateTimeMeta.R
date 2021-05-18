#' Extracts the date/time that a video was created
#'
#'Using ffmpeg, this function extracts the creation timestamp for a given video. It is important to note that this is in UTC - so, you must have some knowledge of the timestamp to sync up with other data streams
#'
#' @param inputVideo path to a video file
#' @param tz the timezone code for the video file
#'
#' @return a character object in YYYY-MM-DD HH:MM:SS that is the date/time that the video was created
#' @export
#'
#' @examples
#' vidCreate = getVideoDateTimeMeta(inputVideo=
#' system.file('extdata', "meeting001_video.mp4", 
#' package = 'zoomGroupStats'), tz="America/Chicago")
#' \dontrun{
#' vidCreate = getVideoDateTimeMeta(inputVideo=
#' "myVideo.mp4", tz="America/Chicago")
#' }
getVideoDateTimeMeta = function(inputVideo, tz) {
  
  haveffmpeg = tryCatch(system("ffmpeg -hide_banner -loglevel quiet -version", intern=T), error=function(err) NA)[1]  
  
  if(!is.na(haveffmpeg[1])) {
    
    ffCmd = paste0("ffprobe -v quiet -select_streams v:0  -show_entries stream_tags=creation_time -of default=noprint_wrappers=1:nokey=1 ", inputVideo)
    o = system(ffCmd, intern=T)      	
    dateTime = as.character(format(as.POSIXct(gsub("T", " ", o), tz="UTC"), tz=tz))
    
  }	else {
    message("You do not have ffmpeg")
    dateTime = NA
  }
  return(dateTime)
  
}

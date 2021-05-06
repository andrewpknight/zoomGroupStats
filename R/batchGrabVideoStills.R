#' Batch process video files, breaking them into stills 
#' 
#' #' This helper calls grabVideoStills, which function currently 
#' relies on either magick or ffmpeg to split a video file into images. 
#' This function will save the images locally in the same 
#' directory where the video is located.
#'
#' @param batchInfo the batchInfo data.frame that is output from batchProcessZoomOutput
#' @param sampleWindow an integer indicating how frequently you want to sample
#' images in number of seconds. 
#' 
#' @return a data.frame that gives information about the batch. Each record 
#' corresponds to one video, with: 
#' \itemize{
#'     \item batchMeetingId - the meeting identifier
#'     \item videoExists - boolean indicating whether the video file was there
#'     \item sampleWindow - integer with the sampleWindow requested
#'     \item numFramesExtracted - the number of image files that were saved
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' vidBatchInfo = batchGrabVideoStills(batchInfo=zoomOut$batchInfo, 
#' sampleWindow=600)
#' }
batchGrabVideoStills = function(batchInfo, sampleWindow){
  vidBatch = data.frame(batchMeetingId=integer(), videoExists=logical(), sampleWindow=integer(), numFramesExtracted=integer())
  if(nrow(batchInfo) == 1) pbMin=0 else pbMin=1
  pb = utils::txtProgressBar(min=pbMin, max=nrow(batchInfo), style=3)  
  for(m in 1:nrow(batchInfo)) {
    utils::setTxtProgressBar(pb, m)
    mInfo = batchInfo[m, ]
    videoExists = file.exists(file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")))
    
    if(videoExists) {
      
      gVS = grabVideoStills(inputVideo=file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")), sampleWindow=sampleWindow)
      vidBatch=rbind(vidBatch, cbind(batchMeetingId=mInfo$batchMeetingId, videoExists=videoExists, sampleWindow=sampleWindow, numFramesExtracted=length(gVS)))
    } else {
      vidBatch=rbind(vidBatch, cbind(batchMeetingId=mInfo$batchMeetingId, videoExists=videoExists, sampleWindow=sampleWindow, numFramesExtracted=0))  
    }
  }
  close(pb)
  return(vidBatch)
}
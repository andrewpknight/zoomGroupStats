#' Batch process video files, breaking them into stills 
#' 
#' #' This helper calls grabVideoStills, which function currently 
#' relies on the av package and 'ffmpeg' to split a video file into images. 
#' This function will save the images to the director specified by the user. 
#'
#' @param batchInfo the batchInfo data.frame that is output from batchProcessZoomOutput
#' @param imageDir the directory where you want the function to write the extracted image files
#' @param overWriteDir logical indicating whether you want to overwrite imageDir if it exists
#' @param sampleWindow an integer indicating how frequently you want to sample
#' images in number of seconds. 
#' 
#' @return a data.frame that gives information about the batch. Each record 
#' corresponds to one video, with: 
#' \itemize{
#'     \item batchMeetingId - the meeting identifier
#'     \item videoExists - boolean indicating whether the video file was there
#'     \item imageDir - path to the directory where video images are saved
#'     \item sampleWindow - integer with the sampleWindow requested
#'     \item numFramesExtracted - the number of image files that were saved
#' }
#' @export
#'
#' @examples
#' vidBatchInfo = batchGrabVideoStills(batchInfo=sample_batch_info,
#' imageDir=tempdir(), overWriteDir=TRUE, sampleWindow=2)
#' \dontrun{
#' vidBatchInfo = batchGrabVideoStills(batchInfo=zoomOut$batchInfo,
#' imageDir="~/Documents/myMeetings/videoImages", overWriteDir=TRUE,  sampleWindow=600)
#' }
batchGrabVideoStills = function(batchInfo,imageDir=NULL, overWriteDir=FALSE, sampleWindow){
  
  if(is.null(imageDir)) {
    stop("You must provide a value for imageDir so that the function knows where to write the images extracted from the video.")
  }
  

  vidBatch = data.frame(batchMeetingId=integer(), videoExists=logical(), sampleWindow=integer(), imageDir=character(), numFramesExtracted=integer())
  
  haveffmpeg = tryCatch(system("ffmpeg -hide_banner -loglevel quiet -version", intern=T), error=function(err) NA)
  
  if(!is.na(haveffmpeg[1])) {  
  
  if(nrow(batchInfo) == 1) pbMin=0 else pbMin=1
  
  pb = utils::txtProgressBar(min=pbMin, max=nrow(batchInfo), style=3)  
  
  for(m in 1:nrow(batchInfo)) {
    
    utils::setTxtProgressBar(pb, m)
    
    mInfo = batchInfo[m, ]
    
    videoExists = file.exists(file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")))
    
    if(videoExists) {
      inname = basename(tools::file_path_sans_ext(file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4"))))
      outDir = file.path(imageDir, inname)
    
      gVS = grabVideoStills(inputVideo=file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")), imageDir=imageDir,overWriteDir=overWriteDir, sampleWindow=sampleWindow)
      vidBatch=rbind(vidBatch, cbind(batchMeetingId=mInfo$batchMeetingId, videoExists=videoExists, imageDir=outDir, sampleWindow=sampleWindow, numFramesExtracted=length(gVS)))
    } else {
      vidBatch=rbind(vidBatch, cbind(batchMeetingId=mInfo$batchMeetingId, videoExists=videoExists, imageDir=NA, sampleWindow=NA, numFramesExtracted=0))  
    }
  }
  close(pb)
  } else {
    message("Error: No videos can be processed because you do not have a working version of ffmpeg. Please check your installation of ffmpeg.")       
  }
  return(vidBatch)
}
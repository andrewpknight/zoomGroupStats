#' Batch analyze faces in videos
#' 
#' Using this function you can analyze attributes of facial expressions within 
#' a batch of video files. This batch approach requires breaking the videos into 
#' still frames in advance by using the batchGrabVideoStills() function. 
#'
#' @param batchInfo the batchInfo data.frame that is output from batchProcessZoomOutput
#' @param imageDir the path to the top-level directory of where all the images are stored
#' @param sampleWindow an integer indicating how frequently you have sampled images
#'  in number of seconds. 
#' @param facesCollectionID name of an 'AWS' collection with identified faces
#'
#' @return data.frame with one record for every face detected in each frame across all meetings. For each face, there is an abundance of information from 'AWS Rekognition'. This output is quite detailed. Note that there will be a varying number of faces per sampled frame in the video. Imagine that you have sampled the meeting and had someone rate each person's face within that sampled moment.
#' @export
#'
#' @examples
#' \dontrun{
#'   vidOut = batchVideoFaceAnalysis(batchInfo=zoomOut$batchInfo, 
#'   imageDir="~/Documents/meetingImages",
#'   sampleWindow = 300)
#' }
batchVideoFaceAnalysis = function(batchInfo, imageDir, sampleWindow, facesCollectionID=NA) {
  for(m in 1:nrow(batchInfo)) {
    mInfo = batchInfo[m, ]
    message("Processing Meeting ",mInfo$batchMeetingId," ", "(",m," of ",nrow(batchInfo),")")
    videoExists = file.exists(file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")))
    dirExists = dir.exists(file.path(imageDir, paste0(mInfo$fileRoot,"_video")))  
    if(dirExists) {
      vidOut = videoFaceAnalysis(inputVideo=file.path(mInfo$dirRoot, paste0(mInfo$fileRoot,"_video.mp4")), recordingStartDateTime=mInfo$recordingStartDateTime, sampleWindow=sampleWindow, facesCollectionID=facesCollectionID, videoImageDirectory=imageDir)
      vidOut$batchMeetingId = mInfo$batchMeetingId
      
      if(!exists('resOut')) {
        resOut = vidOut
      } else {
        resOut = rbind(resOut, vidOut)
      }
    } else {
      message("There is no directory of pre-processed images for the video for this meeting. No analysis was conducted")
    }
  }
  return(resOut)
}


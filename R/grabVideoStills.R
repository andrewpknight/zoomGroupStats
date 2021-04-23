#' Helper function to split a video into still frames
#' This function currently relies on a local install of ffmpeg to 
#' split a video file into images. The images are saved locally
#' in a directory that can be specified by the user or the function.
#' @param inputVideo filepath to a video file
#' @param sampleWindow framerate for the pull of images
#' @param stillPath path to where the images should be saved
#'
#' @return does not return anything. just saves the images to a local directory
#' @export
#'
#' @examples
#' \dontrun{
#' grabVideoStills(
#' inputVideo=system.file('extdata', 'sample_gallery_video.mp4', package = 'zoomGroupStats'), 
#' sampleWindow=45, stillPath="")
#' }
grabVideoStills = function(inputVideo, sampleWindow, stillPath) {
  
  # Check to see if the user has ffmpeg installed on their machine (should probably suppress output of this)
  if(system("ffmpeg -version", intern=F) != 0) {
    stop("You must have ffmpeg installed on your machine to use this function.")
  } else {
    ffCmd = paste("ffmpeg -i ", inputVideo, " -r 1/",sampleWindow, " -f image2 ", paste(stillPath,"/",sep=""), "%05d.png", sep="")
    system(ffCmd, intern=T)
  }
}

#' Helper function to split a video into still frames
#' 
#' This function currently relies on the av package and 
#' 'ffmpeg' to split a video file into images. This function will save 
#' the images to the directory specified by the user.
#' 
#' @param inputVideo full filepath to a video file
#' @param imageDir the directory where you want the function to write the extracted image files
#' @param overWriteDir logical indicating whether you want to overwrite imageDir if it exists
#' @param sampleWindow an integer indicating how frequently you want to sample
#' images in number of seconds. 
#'
#' @return a data.frame that gives information about the still frames. Each record is 
#' a stillframe, with the following info: 
#' \itemize{
#'    \item imageSeconds - number of seconds from the start of the video when this image was captured
#'    \item imageName - full path to where the image has been saved as a .png 
#' }
#' @export
#'
#' @examples
#' vidOut = grabVideoStills(inputVideo=system.file('extdata', "meeting001_video.mp4", 
#' package = 'zoomGroupStats'), imageDir=tempdir(), overWriteDir=TRUE, sampleWindow=2)
#' \dontrun{
#' grabVideoStills(inputVideo='myMeeting.mp4', 
#' imageDir="~/Documents/myMeetings/videoImages", overWriteDir=TRUE,  sampleWindow=45)
#' }
grabVideoStills = function(inputVideo, imageDir=NULL, overWriteDir=FALSE, sampleWindow) {
  
  if(is.null(imageDir) || !dir.exists(imageDir)) {
    stop("You must provide a value for imageDir so that the function knows where to write the images extracted from the video.")
  }
  
  # Get the full path of inputVideo
  inname = basename(tools::file_path_sans_ext(inputVideo))
  outDir = file.path(imageDir, inname)
  outpath = file.path(outDir, "image_%6d.png")
  
  if(dir.exists(file.path(outDir)) && overWriteDir) {
    unlink(outDir, recursive = TRUE) 
  }
  
  if(!dir.exists(file.path(imageDir))) {
    stop("You did not provide a valid path for the image directory.")
  } 
  
  haveffmpeg = tryCatch(system("ffmpeg -hide_banner -loglevel quiet -version", intern=T), error=function(err) NA)
  
  if(!is.na(haveffmpeg[1])) {
    dir.create(outDir)    
    ffCmd = paste("ffmpeg -i ", inputVideo, " -vf fps=1.0/",sampleWindow, " ", outpath, " -hide_banner -nostdin -loglevel error", sep="")    
    message("Processing ", basename(inputVideo), " using ffmpeg. Note that processing videos can be time intensive for long duration videos.")
    o = system(ffCmd, intern=T)    
    
    # How many images are in the directory:
    avOut = list.files(path=outDir, pattern="*.png", full.names=T)    
  
    if(length(avOut) > 1) {
      imageSeconds = c(sampleWindow/2, sampleWindow/2+(1:(length(avOut)-1))*sampleWindow)  	
    } else {
      imageSeconds = sampleWindow/2
    }
    
    imageInfo = data.frame(imageSeconds=imageSeconds, imageName=avOut)
    
  } else {
    message("Error: No videos can be processed because you do not have a working version of ffmpeg. Please check your installation of ffmpeg.")   
    imageInfo = data.frame(imageSeconds=NA, imageName=NA)
  } 
  
  return(imageInfo)
}
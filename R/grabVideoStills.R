#' Helper function to split a video into still frames
#' 
#' This function currently relies on either magick or 
#' ffmpeg to split a video file into images. This function will save 
#' the images locally in the same directory where the video is located.
#' @param inputVideo full filepath to a video file
#' @param sampleWindow an integer indicating how frequently you want to sample
#' images in number of seconds. 
#' @param tryffmpeg boolean indicating whether you want it to try to use ffmpeg, 
#' which tends to be much faster than magick for splitting frames
#'
#' @return a dataframe that gives information about the still frames. Each record is 
#' a stillframe, with the following info: 
#' \itemize{
#'    \item imageSeconds - number of seconds from the start of the video when this image was captured
#'    \item imageName - full path to where the image has been saved as a .png 
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' grabVideoStills(inputVideo='myMeeting.mp4', 
#' sampleWindow=45)
#' }
grabVideoStills = function(inputVideo, sampleWindow, tryffmpeg=TRUE) {

  # Get the full path of inputVideo
  inname = tools::file_path_sans_ext(inputVideo)
  outpath = file.path(inname, "%05d.png")

  # Create the new directory or press forward and overwrite? 
  if(dir.exists(file.path(inname))) {
    choice = utils::select.list(c("Yes", "No"), title=paste("The ",outpath, " directory already exists. Do you want to delete the old directory and files and create a new one?", sep=""))

  if(choice %in% c("Yes", 1)) {
      message("OK - This will delete the existing directory and files for ",basename(inputVideo), " in ",dirname(inputVideo), ".")
      unlink(file.path(inname), recursive=TRUE)
  } else {

    stop("Did not process the video because doing so could overwrite the contents of this directory.")
  }     
  } 

  dir.create(inname)  

  haveffmpeg = tryCatch(system("ffmpeg -hide_banner -loglevel quiet -version", intern=T), error=function(err) NA)

  if(!is.na(haveffmpeg[1]) && tryffmpeg) {

# ffCmd = paste("ffmpeg -i ", inputVideo, " -r 1/",sampleWindow, " ", outpath, " -hide_banner -nostdin -loglevel error", sep="")
  ffCmd = paste("ffmpeg -i ", inputVideo, " -vf fps=1.0/",sampleWindow, " ", outpath, " -hide_banner -nostdin -loglevel error", sep="")   
    message("Processing ", basename(inputVideo), " using ffmpeg. Note that processing videos can be time intensive for long duration videos.")
    o = system(ffCmd, intern=T)       
  } else {
    message("Processing ", basename(inputVideo), " using magick. Note that processing videos can be time intensive for long duration videos. In particular, magick is far slower than ffmpeg. If you install ffmpeg, these functions will run more quickly.")   
  imagesFromVideo = magick::image_read_video(inputVideo, fps=(1/sampleWindow), format="png")
  for(i in 1:length(imagesFromVideo)) {
    outpath = file.path(inname, sprintf("%05d.png", i))
    magick::image_write(imagesFromVideo[i], path=outpath)
  } 
  } 

  # How many images were actually created?
  imageNames = list.files(path=inname, pattern="*.png", full.names=T)  
    return(imageNames)
}


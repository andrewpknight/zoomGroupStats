#' Detects the timestamp within a recorded Zoom session
#'
#'This function uses the 'AWS Rekognition' service to detect the timestamp within a Zoom recording. This presumes that the user included the timestamp option in their Zoom Cloud Recording setup. 
#'
#' @param imageInput the full path to the image file
#'
#' @return returns the timestamp as character YYYY-MM-DD HH:MM:SS
#' @export
#'
#' @examples
#' \dontrun{
#' timeStamp = imageDetectTimeStamp(imageInput="./imageFile.png")
#' }
imageDetectTimeStamp = function(imageInput) {
  svc = paws::rekognition()
  o = svc$detect_text(
    Image=list(Bytes=imageInput), 
    Filters=list(WordFilter=list(MinConfidence=50),RegionsOfInterest=list(list(BoundingBox = list(Width=0.20, Height=0.10, Left=0.80, Top=0.90)))))
  
  if(length(o$TextDetections) > 0) {
    tr = as.data.frame(jsonlite::fromJSON(jsonlite::toJSON(o)))
    timeDate = tr[tr$TextDetections.Type=="LINE", "TextDetections.DetectedText"][[1]]
  } else {
    timeDate = NA
  }
  return(timeDate)	
}
#' Analyze the facial features within an exported Zoom video file
#' 
#' Using this function you can analyze attributes of facial expressions within 
#' a video file. There are two ways to supply the video information. First, you 
#' can provide the actual video file. The function will then break it down 
#' into still frames using the grabVideoStills() function. Second, you can use 
#' the videoImageDirectory argument to give the location of a directory where 
#' images have been pre-saved. 
#'
#' @param inputVideo string path to the video file (ideal is gallery)
#' @param recordingStartDateTime YYYY-MM-DD HH:MM:SS of the start of the recording
#' @param sampleWindow Frame rate for the analysis
#' @param facesCollectionID name of an 'AWS' collection with identified faces
#' @param videoImageDirectory path to a directory that either contains image files or where you want to save image files
#' @param grabVideoStills logical indicating whether you want the function to split the video file or not
#' @param overWriteDir logical indicating whether to overwrite videoImageDirectory if it exists
#'
#' @return data.frame with one record for every face detected in each frame. For each face, there is an abundance of information from 'AWS Rekognition'. This output is quite detailed. Note that there will be a varying number of faces per sampled frame in the video. Imagine that you have sampled the meeting and had someone rate each person's face within that sampled moment. 
#' @export
#'
#' @examples
#' \dontrun{
#' vid.out = videoFaceAnalysis(inputVideo="meeting001_video.mp4", 
#' recordingStartDateTime="2020-04-20 13:30:00", 
#' sampleWindow=1, facesCollectionID="group-r",
#' videoImageDirectory="~/Documents/meetingImages", 
#' grabVideoStills=FALSE, overWriteDir=FALSE)
#' }
videoFaceAnalysis = function(inputVideo, recordingStartDateTime, sampleWindow, facesCollectionID=NA, videoImageDirectory=NULL, grabVideoStills=FALSE, overWriteDir=FALSE) {
  
  svc = paws::rekognition()
  
  recordingStartDateTime = as.POSIXct(recordingStartDateTime)
  
  if(grabVideoStills) {
    grabVidOut = grabVideoStills(inputVideo=inputVideo, imageDir=videoImageDirectory,overWriteDir=overWriteDir, sampleWindow=sampleWindow)
  } 
  videoImageDirectory = file.path(videoImageDirectory, basename(tools::file_path_sans_ext(inputVideo)))
  
  # Get any images associated with this video
  imgFiles = list.files(path=videoImageDirectory, full.names=T)

  # These are empty lists to use to save the information
  df.o = list()
  inf = list()
  
  # Now loop through the images that are part of this video (which were already extracted)
  if(length(imgFiles) == 0) {
    message("There are no images extracted for this video file")
    res.out =data.frame(matrix(nrow=0, ncol=length(col.names)))
  } else {
    
    message("Analyzing faces in sampled frames. For lots of images and lots of faces, this can be time intensive.")
    
    if(length(imgFiles) == 1) pbMin=0 else pbMin=1    
    pb = utils::txtProgressBar(min=0, max=length(imgFiles), style=3)
    for(i in 1:length(imgFiles)) {
      utils::setTxtProgressBar(pb, i)
      # Pull the image and its information
      img = magick::image_read(imgFiles[i])
      inf[[i]] = magick::image_info(img)
      
      # This is stupid, but we have to adjust the timestamping of the images. The first one is 0 + sampleWindow/2, then the rest are sampleWindow incremented
      if(i == 1) {
        imgTimestamp = sampleWindow/2
      } else {
        imgTimestamp = sampleWindow/2 + (i-1)*sampleWindow
        
      }
      
      # Detect faces in this frame
      df.o[[i]] = svc$detect_faces(Image=list(Bytes=imgFiles[i]), Attributes="ALL")
      
      # Get the details of any faces detected in this frame
      faces = df.o[[i]]$FaceDetails
      
      # If there are no faces in the image, then create a blank results record, with just the image id
      if(length(faces) == 0) {
        res.line = matrix(nrow=1,ncol=23)
        res.line[1,1] = imgFiles[i]
        res.line[1, 21] = imgTimestamp			
      } else {
        # Otherwise, if there are faces in the image, go through each face to get its info	
        # create a matrix to hold the info
        res.line = matrix(nrow=length(faces), ncol=23)
        
        # Loop through each face and analyze it
        
        for(face.num in 1:length(faces)) {
          fd = faces[[face.num]]
          res.line[face.num,1] = imgFiles[i]
          res.line[face.num,2] = face.num
          res.line[face.num,3] = fd$AgeRange$Low
          res.line[face.num,4] = fd$AgeRange$High
          res.line[face.num,5] = fd$Smile$Value
          res.line[face.num,6] = fd$Eyeglasses$Value
          res.line[face.num,7] = fd$Sunglasses$Value
          res.line[face.num,8] = fd$Gender$Value
          res.line[face.num,9] = fd$Beard$Value
          res.line[face.num,10] = fd$Mustache$Value
          res.line[face.num,11] = fd$EyesOpen$Value		
          res.line[face.num,12] = fd$MouthOpen$Value		
          
          # Make an emotions table for this image
          for(e in fd$Emotions) {
            
            if(e$Type == "CONFUSED") res.line[face.num,13] = e$Confidence
            else if(e$Type == "CALM") res.line[face.num,14] = e$Confidence
            else if(e$Type == "HAPPY") res.line[face.num,15] = e$Confidence
            else if(e$Type == "DISGUSTED") res.line[face.num,16] = e$Confidence
            else if(e$Type == "ANGRY") res.line[face.num,17] = e$Confidence
            else if(e$Type == "FEAR") res.line[face.num,18] = e$Confidence
            else if(e$Type == "SAD") res.line[face.num,19] = e$Confidence
            else if(e$Type == "SURPRISED") res.line[face.num,20] = e$Confidence		
          }
          res.line[face.num, 21] = imgTimestamp
          
          # if the user specified a face collection, go into it to see if the face has an identity
          # Including the confidence value because it sometimes couldn't tell it was a face
          # at low levels of confidence
          if(!is.na(facesCollectionID) && fd$Confidence > 90) {
            
            # Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
            # why the image info object above is needed
            box = fd$BoundingBox
            imageWidth=inf[[i]]$width
            imageHeight=inf[[i]]$height
            x1 = box$Left*imageWidth
            y1 = box$Top*imageHeight
            x2 = x1 + box$Width*imageWidth
            y2 = y1 + box$Height*imageHeight	
            
            # Crop out just this particular face out of the video
            img.crop = magick::image_crop(img, paste(box$Width*imageWidth,"x",box$Height*imageHeight,"+",x1,"+",y1, sep=""))
            img.crop = magick::image_write(img.crop, path = NULL, format = "png")
            
            # Search in a specified collection to see if we can label the identity of the face is in this crop
            faceRec = try(svc$search_faces_by_image(CollectionId=facesCollectionID,Image=list(Bytes=img.crop), FaceMatchThreshold=70), silent=T)
            
            
            if(is.character(faceRec)) {
              res.line[face.num, 22] = "IDENTITY NOT RECOGNIZED"							
            } else {
              if(length(faceRec$FaceMatches) > 0) {
                res.line[face.num, 22] = faceRec$FaceMatches[[1]]$Face$ExternalImageId
                res.line[face.num, 23] = faceRec$FaceMatches[[1]]$Face$Confidence
              } else {
                res.line[face.num, 22] = "IDENTITY NOT RECOGNIZED"							
              }							
            }
          } else {
            res.line[face.num, 22] = "IDENTITY NOT RECOGNIZED"
          }
          # Close the face loop
        }
        # Close the else	
      }		
      if(i == 1) {
        raw.out = res.line
      } else {
        raw.out = rbind(raw.out, res.line)
      }			
      # Close the image loop
    }		
    close(pb)
    res.out = data.frame(raw.out, stringsAsFactors=F)
  }

  col.character=c(1,8,22)
  col.numeric = c(2:4, 13:21, 23)
  col.boolean = c(5:7,9:12)
  col.names = c("frameId", "faceId", "ageLow", "ageHigh", "smile", "eyeglasses", "sunglasses", "gender", "beard", "mustache", "eyesopen", "mouthopen", "confused", "calm", "happy", "disgusted", "angry", "fear", "sad", "surprised", "imgSeconds", "identifiedPerson", "identifiedConfidence")
  res.out[,col.character] = lapply(res.out[,col.character], as.character)  
  res.out[,col.numeric] = lapply(res.out[,col.numeric], as.numeric)
  res.out[,col.boolean] = lapply(res.out[,col.boolean], as.logical)
  names(res.out) = col.names	
  res.out$imgTimestamp = recordingStartDateTime + res.out$imgSeconds
  res.out = res.out[, c(1,21,22,23, 24, 2:20)]	
  return(res.out)
}
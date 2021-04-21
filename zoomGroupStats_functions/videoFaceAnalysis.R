############################################################
# videoFaceAnalysis Function
# 
############################################################
# Video-based sentiment analysis function
# This function takes in a video file and produces measures based on the faces in the 
# video. Note that this is a very crude way of doing this. It uses the image detection
# capabilities in AWS rekognition to cut the video up in to a sample of frames, then 
# analyzes those frames. rekognition has a video analysis feature that I'll incorporate
# later. 

# For best results with recorded Zoom sessions: 
# I would recommend going into your settings for recordings and choosing to 
# record active speaker, gallery view, and shared screen separately. 
# Make sure to choose to record the gallery view so that you get
# all of the faces in a single video feed. You also might want to choose "Optimize the recording..."

# To use this function, you must have an aws account that with privileges for the rekognition service
# However you authenticate for AWS, you should do so before running the function.

# example call:					vid.out = videoFacesAnalysis(inputVideo="~/Desktop/sample_video.mp4", recordingStartDateTime="2020-04-01 17:56:34", sampleWindow=20, facesCollectionID="class15-r")

# INPUT ARGUMENTS:
# inputVideo: 					the input video file
# recordingStartDateTime: 		the name of the id variable for the text 
# sampleWindow:					the number of seconds in between each sample of the recording
# facesCollectionID:			Not necessary: Name of an S3 collection if you want to ID specific people

# OUTPUT: 
# frameId						an identifier for the frame of the video used for this record
# imgTimestamp					the timestamp of the image from the video (see note below re: recording)
# identifiedPerson				the name of the person identified in the frame, if a collection is given
# identitiedConfidence			the confidence level for the identity (first one)
# faceId						an identifier for the face in the frame
# ageLow						low boundary for estimated age
# ageHigh						high boundary for estimated age
# smile 						boolean - does the face have smile
# eyeglasses 					boolean - does the face have eyeglasses
# sunglasses 					boolean - does the face have sunglasses
# gender 						gender of face
# beard 						boolean - does the face have beard
# mustache 						boolean - does the face have mustache
# eyesopen 						boolean - does the face have eyes open
# mouthopen 					boolean - does the face have mouth open 
# confused 						confidence level for the face showing confused
# calm 							confidence level for the face showing calm
# happy 						confidence level for the face showing happy
# disgusted 					confidence level for the face showing disgusted
# angry 						confidence level for the face showing angry
# fear 							confidence level for the face showing fear
# sad 							confidence level for the face showing sad
# surprised 					confidence level for the face showing surprised

# Note: This function currently does things in a brute force way. I'll refine this so that 
# things are processed in batch, rather than in so many gross loops. 

# Note: Same as with transcripts: I plan to fix at a later point in time the timing issue. Specifically, it is not clear where in Zoom's system I can get the actual time that the recording was started. This 
# is a problem for linking the transcript file up with the chat file.
# One workaround for now (for research) would be to set recordings to auto-start. This is not ideal, though.
# we should be able to know when the recording was started. It is embedded in the video, so could pull from there.



###############
# This function pulls still frames out of a video. It depends on the user having ffmpeg on their machine. 
# It also likely would not work on a windows machine. need to add a kill message and error if someone doesn't have
# ffmpeg
###############

grabVideoStills = function(inputVideo, sampleWindow, stillPath) {

		# Check to see if the user has ffmpeg installed on their machine (should probably suppress output of this)
		if(system("ffmpeg -version", intern=F) != 0) {
			kill("You must have ffmpeg installed on your machine to use this function.")
		} else {

			ffCmd = paste("ffmpeg -i ", inputVideo, " -r 1/",sampleWindow, " -f image2 ", paste(stillPath,"/",sep=""), "%05d.png", sep="")
			system(ffCmd, intern=T)
		}
}


videoFaceAnalysis = function(inputVideo, recordingStartDateTime, sampleWindow, facesCollectionID=NA) {
	require(paws)
	require(magick)

	svc = rekognition()

	recordingStartDateTime = as.POSIXct(recordingStartDateTime)

	# Should add an option to the function to tell it to create the stills or that
	# The stills are already created (and the directory). This would give an option to use
	# the function for non-ffmpeg users. For now, leaving as is. 

	## Create stills from the video => Save in a temp directory
	baseName = strsplit(basename(inputVideo), ".", fixed=T)[[1]][[1]]
	imgTempDir = 	paste(dirname(inputVideo),"/videoFaceAnalysis_temp_", baseName, sep="")
	dir.create(imgTempDir)
	grabVideoStills(inputVideo, sampleWindow, imgTempDir)

	# Get any images associated with this video
	imgFiles = list.files(path=imgTempDir, full.names=T)

	# These are empty lists to use to safe the information
	df.o = list()
	inf = list()

	# Now loop through the images that are part of this video (which were already extracted)

	for(i in 1:length(imgFiles)) {

		# Pull the image and its information
		img = image_read(imgFiles[i])
		inf[[i]] = image_info(img)

		# This is stupid, but I've found it is necessary to adjust the timestamping, which isn't right on target
		# from the splitting of videos
		if(i >= 3) {
			imgTimestamp = 17 + (i-3)*20
		} else {
			imgTimestamp = 0
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
					img.crop = image_crop(img, paste(box$Width*imageWidth,"x",box$Height*imageHeight,"+",x1,"+",y1, sep=""))
					img.crop = image_write(img.crop, path = NULL, format = "png")
					
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

	res.out = data.frame(raw.out, stringsAsFactors=F)
	col.numeric = c(2:4, 13:21, 23)
	col.boolean = c(5:7,9:12)
	col.names = c("frameId", "faceId", "ageLow", "ageHigh", "smile", "eyeglasses", "sunglasses", "gender", "beard", "mustache", "eyesopen", "mouthopen", "confused", "calm", "happy", "disgusted", "angry", "fear", "sad", "surprised", "imgTimestamp", "identifiedPerson", "identifiedConfidence")
	res.out[,col.numeric] = lapply(res.out[,col.numeric], as.numeric)
	res.out[,col.boolean] = lapply(res.out[,col.boolean], as.logical)
	names(res.out) = col.names	
	res.out = res.out[, c(1,21,22,23, 2:20)]	
}
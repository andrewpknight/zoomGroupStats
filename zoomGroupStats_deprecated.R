############################################################
# Author: 			Andrew Knight (http://apknight.org)
	
# Last Update:		2021-02-12 09:30 US CDT
# Update Notes:		
#					- Some bug fixes 
#					- Added a function to process zoomMeetingInfo
#					- Added an alpha function to do turn-taking analysis. 
# 					- Added a few alpha functions to process audio files
#					- Added a few alpha functions to do windowed analyses
#					- Changed the video processing from magick to ffmpeg

# I created this as a way to help people do social science research through web-based meetings (i.e., Zoom). 
# It's still a work in progress, but this is a start. If you would like to use it or help build it, 
# please reach out! 


############################################################
# OVERVIEW OF FUNCTIONS 
############################################################
# This script contains functions to use for analyzing recorded Zoom sessions
# This is a work in progress and more are coming. 

# FUNCTIONS THAT ARE HEAVILY COMMENTED
# processZoomMeetingInfo	Parses the downloaded meeting particiapnts file from Zoom
# processZoomChat			Parses the downloaded chat file from a recorded Zoom session
# processZoomTranscript		Parses the downloaded transcript from a recorded Zoom session
# sentiOut					Conducts a sentiment analysis on either the Chat or Transcript
# videoFaceAnalysis 		Analyzes the video from a Zoom session and outputs face/emotion measures
# textConversationAnalysis	Analyzes either chat or transcript and outputs conversation metrics

# FUNCTIONS THAT ARE IN ALPHA STAGE AND THAT I'M TESTING
# transcribeZoomAudio		uses AWS transcription service to process an audio file
# processZoomAudio			parse the output of the AWS transcription
# makeTimeWindows			Creates time windows in a transcript to do windowed analyses
# windowedTextConversationAnalysis	Conducts a windowed conversation analysis
# turn-taking				Does an analysis of conversation turn-taking

# Note you will require the following packages to run these: 
# reshape2
# stringr
# paws
# magick
# data.table

# You will also require an aws account with privileges for rekognition and comprehend to use the 
# text analysis and video analysis. If you don't know how to do this, please: 
# Search online for (a) setting up AWS account; (b) setting up paws. I found the following useful: 
# https://github.com/paws-r/paws/blob/master/docs/credentials.md

# For doing face analysis, you'll also need to have ffmpeg installed on your machine (this change
# was motivated by the speed of video processing using ffmpeg)

# If, after you try you are still struggling, I can give guidance on this if useful--just contact me. 

############################################################
# processZoomMeetingInfo Function
############################################################

# Zoom Meeting / Participants info parsing function
# This function parses the information from the downloadable meeting information 
# file in Zooms "reports" section. This file presumes that you have 
# checked the box to include the meeting information in the file. 

processZoomMeetingInfo = function(inputPath) {


	convDate = function(oldDate) {
		month = substr(oldDate,1,2)
		day = substr(oldDate,4,5)
		year = substr(oldDate,7,10)
		hour = substr(oldDate,12,13)
		min = substr(oldDate,15,16)
		sec = substr(oldDate,18,19)
		tod = substr(oldDate,21,22)
		if(tod == "PM" && as.numeric(hour) < 12) {
			hour = as.character((as.numeric(hour) + 12))
		}
		newDate = paste(year,"-", month, "-", day, " ", hour,":",min,":",sec, sep="")
		return(newDate)
	}	

	meetInfo = data.frame(read.delim(inputPath, header=T, nrows=1, skip=0, sep=","), stringsAsFactors=F)
	names(meetInfo) = c("meetingId", "meetingTopic", "meetingStartTime", "meetingEndTime", "userEmail", "meetingDuration", "numParticipants", "blankCol")
	meetInfo$blankCol = NULL
	meetInfo$meetingStartTime = convDate(meetInfo$meetingStartTime)
	meetInfo$meetingEndTime = convDate(meetInfo$meetingEndTime)

	# Change the date column to something more useable in the other functions

	partInfo = data.frame(read.delim(inputPath, header=T, skip=3, sep=","), stringsAsFactors=F)
	names(partInfo) = c("userName", "userEmail", "userDuration", "userGuest")	

	outInfo = list(meetInfo, partInfo)	

	return(outInfo)
}

############################################################
# processZoomChat Function
############################################################

# Zoom Chat File Processing
# This function parses the data from the chatfile that is downloaded from the Zoom website. 
# NOTE: This is the file that accompanies a recording. This is not the file 
# that you download directly within the window. It is also not the one that is 
# saved locally on your computer. This is the file that you can access after a session 
# if you record in the cloud. 

# example call:				ch.out = processZoomChat(fname="~/Desktop/chat.txt", sessionStartDateTime="2020-04-01 17:56:34", languageCode="en")

# INPUT ARGUMENTS: 
# fname: 					the path to the local file where the chat file (txt) is saved.
# sessionStartDateTime: 	the time that the actual session was launched. Format is YYYY-MM-DD HH:MM:SS
# languageCode:				the code for the language (e.g., en)

# OUTPUT: 
# message_id:				an incremented numeric identifier for a message in the chat
# message_time:				a timestamp for the message, based on the start of the Zoom session
# user_name:				the name attached to the message
# message:					the text of the message
# message_language:			the language code for the chat

processZoomChat = function(fname, sessionStartDateTime, languageCode) {

	require(reshape2)
	require(stringr)

	# NOTE: Need to fix this to remove any stray tabs from this file before bringing it in....yikes.

	ch = read.delim(fname, sep="\t", stringsAsFactors=F, header=F, col.names=c("message_increment", "user_name", "message"))

	####################################
	# First thing do to is to create a message_time variable

	# This is user-supplied and could come from the usermeeting report that can be downloaded
	sessionStartDateTime = as.POSIXct(sessionStartDateTime)

	# This is the value embedded in the chat record. It is an HH:MM:SS delta from the start of the session
	# I'm doing a crude parse to just get the total number of seconds that the delta is. This is then
	# used as the increment from the sessionStartDateTime
	ch$inc_hours = as.numeric(substr(ch$message_increment,1,2))
	ch$inc_mins = as.numeric(substr(ch$message_increment,4,5))	
	ch$inc_secs = as.numeric(substr(ch$message_increment,7,8))		
	ch$inc_total_secs = ch$inc_hours*60*60 + ch$inc_mins*60 + ch$inc_secs
	ch$message_time = sessionStartDateTime + ch$inc_total_secs

	####################################
	# Chat transcripts do not handle soft returns well (i.e., if the same person uses a soft line break 
	# for multiple lines in a single message that is submitted to the system). 
	# This is a crude way to identify them based on someone having an invalid message time. 
	# For now, will assign that text to the last marked user name in the dataset, 
	# pasting the messages together into a single line (separated by a space. )

	# Create a flag to mark erroneous records based on the message time variable. This should be made stronger
	# and cleaner eventually
	ch$flag = is.na(as.integer(substr(ch$message_increment,1,1))) + (nchar(ch$message_increment) != 8)

	# Assign the value in the message_increment variable to the message variable. This is because
	# the parsing of the file is fucked up when there are soft returns in someone's chat message
	ch$message = ifelse(ch$flag > 0, ch$message_increment, ch$message)

	# Go through the records from the bottom up to paste the message on the one it 
	# should be part of
	for(i in nrow(ch):1) {
		if(ch[i,"flag"] > 0) {
			ch[(i-1), "message"] = paste(ch[(i-1), "message"], ch[i, "message"], sep=" ")
		}
	}

	# now drop the unnecessary records
	ch = ch[ch$flag == 0, ]

	# get rid of whitespace at the beginning and end
	ch$message = str_trim(ch$message, "both")

	# Add a language variable, which is user-supplied for now
	ch$message_language = languageCode

	# Add a simple numeric incrementing identifier for the messages that people submitted
	ch$message_id = 1:nrow(ch)

	# Get rid of the superfluous colon at the end of the usernames
	ch$user_name = substr(ch$user_name, 1, nchar(ch$user_name)-1)

	# Mark as unidentified any user with a blank username
	ch$user_name = ifelse(ch$user_name == "" | is.na(ch$user_name), "UNIDENTIFIED", ch$user_name)	


	# Clean up the ordering of variables that are returned
	ch = ch[,c("message_id", "message_time", "user_name", "message", "message_language")]

	return(ch)
}

############################################################
# processZoomTranscript Function
############################################################

# Zoom Recording Transcript File Processing
# This function parses the data from the transcript file (.vtt) that is downloaded from the Zoom website. 
# NOTE: This is the file that accompanies a recording to the cloud. 

# example call:				tr.out = processZoomTranscript(fname="~/Desktop/transcript.vtt", recordingStartDateTime="2020-04-01 17:56:34", languageCode="en")

# INPUT:
# fname: 					the path to the local file where the transcript file (vtt) is saved.
# recordingStartDateTime: 	the time that the recording was launched. Format is YYYY-MM-DD HH:MM:SS
# languageCode:				the code for the language (e.g., en)

# Note: I plan to fix at a later point in time the timing issue. Specifically, it is not clear
# where in Zoom's system I can get the actual time that the recording was started. This 
# is a problem for linking the transcript file up with the chat file.
# One workaround for now (for research) would be to set recordings to auto-start. This is not ideal, though.
# we should be able to know when the recording was started. It is embedded in the video, so could pull from there.

# OUTPUT: 
# utterance_id:				an incremented numeric identifier for a marked speech utterance
# utterance_start_seconds	the number of seconds from the start of the recording (when it starts)
# utterance_start_time:		the timestamp for the start of the utterance
# utterance_end_seconds		the number of seconds from the start of the recording (when it ends)
# utterance_end_time:		the timestamp for the end of the utterance
# utterance_time_window:	the number of seconds that the utterance took
# user_name: 				the name attached to the utterance
# utterance_message:		the text of the utterance
# utterance_language:		the language code for the transcript

processZoomTranscript = function(fname, recordingStartDateTime, languageCode) {
	library(reshape2)
	require(stringr)

	# Parse the transcript file -- vtt is a structured format. 
	f = readLines(fname)
	
	# there are three main pieces of data for each marked "utterance" - an id, a window of time, and the text
	utterance_id = as.integer(f[seq(3,length(f), 4)])
	utterance_window = f[seq(4,length(f), 4)]
	utterance_text = f[seq(5,length(f), 4)]

	# Parse the time window into two separate elements
	utterance_start_time = unlist(strsplit(utterance_window, " --> "))[seq(1, length(utterance_window)*2, 2)]
	utterance_end_time = unlist(strsplit(utterance_window, " --> "))[seq(2, length(utterance_window)*2, 2)]	

	# Parse the time to create a total number of seconds
	timeCalc = function(incTime) {
		inc_hours = as.numeric(substr(incTime,1,2))
		inc_mins = as.numeric(substr(incTime,4,5))	
		inc_secs = as.numeric(substr(incTime,7,12))		
		inc_total_secs = inc_hours*60*60 + inc_mins*60 + inc_secs
	}

	# Preserve this to use in a dynamic conversation analysis
	utterance_start_seconds = timeCalc(utterance_start_time)
	utterance_end_seconds = timeCalc(utterance_end_time)

	# Now turn these into actual datetime values
	recordingStartDateTime = as.POSIXct(recordingStartDateTime)
	utterance_start_time = recordingStartDateTime + utterance_start_seconds
	utterance_end_time = recordingStartDateTime + utterance_end_seconds

	# Create a time window (in seconds) for the utterances -- how long is each in seconds
	utterance_time_window = as.numeric(difftime(utterance_end_time, utterance_start_time, units="secs"))

	# Parse the utterance message itself
	utterance_message = substring(utterance_text, regexpr("[:]", utterance_text)+2)

	# Get the user name that spoke the text
	user_name = substr(utterance_text, 1, regexpr("[:]", utterance_text)-1)	


	# Prepare the output file
	res.out = data.frame(utterance_id, utterance_start_seconds, utterance_start_time, utterance_end_seconds, utterance_end_time, utterance_time_window, user_name, utterance_message, stringsAsFactors=F)

	# Mark as unidentified any user with a blank username
	res.out$user_name = ifelse(res.out$user_name == "" | is.na(res.out$user_name), "UNIDENTIFIED", res.out$user_name)		

	# Add the language code
	res.out$utterance_language = languageCode

	return(res.out)
}

############################################################
# sentiOut Function
############################################################
# Text-based sentiment analysis function
# This function takes in the output of the chat and transcript functions. It then
# conducts a sentiment analysis on an identified chunk of text
# and returns the values. 
# To use this function, you must have an aws account that with privileges for the comprehend service
# However you authenticate for AWS, you should do so before running the function.

# example call:			sent.out = sentiOut(inputData=tr.out, idVar = "utterance_id", textVar = "utterance_message", languageCodeVar = "utterance_language")

# INPUT:
# inputData: 			the input data frame
# idVar: 				the name of the id variable for the text 
# textVar:				the name of the variable with the text in it
# languageCodeVar:		the variable containing the language code for each text chunk

# OUTPUT: This function returns the inputData plus the following variables
# sent_class: 			the text-based sentiment classification of the message
# Mixed:				the confidence level for the text being mixed sentiment
# Negative: 			the confidence level for the text being negative sentiment
# Neutral: 				the confidence level for the text being neutral sentiment
# Positive: 			the confidence level for the text being positive sentiment

# Note: This function currently does this in a brute force way. In the future, I will
# build this so that it batches chunks of text to run, rather than looping through.

sentiOut = function(inputData, idVar, textVar, languageCodeVar){
	require(paws)
	require(reshape2)
	# Identify the AWS service comprehend: 
	# AS STATED ABOVE--YOU MUST HAVE AN AUTHENTICATED ACCOUNT WITH THE RIGHT PRIVILIGES
	svc = comprehend()

	# Loop through each record of the inputData
	for(i in 1:nrow(inputData)) {

		# Run the sentiment detection function from AWS Comprehend on this chunk of text
		sent = svc$detect_sentiment(Text = inputData[i,textVar], LanguageCode=inputData[i,languageCodeVar])

		# Create a simple 
		res.line = cbind(inputData[i,idVar],unlist(sent$SentimentScore), sent$Sentiment)
		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}		
	}

	# Now, clean up the output so that it comes as a dataframe
	d.res = data.frame(res.out, stringsAsFactors=F)
	names(d.res) = c(idVar, "sent_value", "sent_class")
	d.res$sent_type = unlist(lapply(strsplit(row.names(d.res), '[.]'), '[[',1))

	d.res.melt = reshape2::melt(d.res, idVars=c(idVar, sent_class, "sent_type"), variable.name="sent_variable", value.name="sent_value")

	d.res.wide = reshape2::dcast(d.res.melt, get(idVar) + sent_class ~ sent_type, value.var="sent_value")
	names(d.res.wide)[1] = idVar

	d.res.wide[,c("Mixed", "Negative", "Neutral", "Positive")] = lapply(d.res.wide[,c("Mixed", "Negative", "Neutral", "Positive")], as.numeric)

	d.mrg = merge(inputData, d.res.wide, by=idVar, all.x=T)
	return(d.mrg)
}


############################################################
# videoFaceAnalysis Function
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
# frame_id						an identifier for the frame of the video used for this record
# img_timestamp					the timestamp of the image from the video (see note below re: recording)
# identified_person				the name of the person identified in the frame, if a collection is given
# identification_confidence		the confidence level for the identity (first one)
# face_id						an identifier for the face in the frame
# age_low						low boundary for estimated age
# age_high						high boundary for estimated age
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

# Note: This function currently es things in a brute force way. I'll refine this so that 
# things are processed in batch, rather than in so many gross loops. 

# Note: Same as with transcripts: I plan to fix at a later point in time the timing issue. Specifically, it is not clear where in Zoom's system I can get the actual time that the recording was started. This 
# is a problem for linking the transcript file up with the chat file.
# One workaround for now (for research) would be to set recordings to auto-start. This is not ideal, though.
# we should be able to know when the recording was started. It is embedded in the video, so could pull from there.


grabVideoStills = function(inputVideo, sampleWindow, stillPath) {
		ffCmd = paste("ffmpeg -i ", inputVideo, " -r 1/",sampleWindow, " -f image2 ", paste(stillPath,"/",sep=""), "%05d.png", sep="")
		system(ffCmd)
}


videoFaceAnalysis = function(inputVideo, recordingStartDateTime, sampleWindow, facesCollectionID=NA) {
	require(paws)
	require(magick)

	svc = rekognition()

	recordingStartDateTime = as.POSIXct(recordingStartDateTime)

	## Create stills from the video => Save in a temp directory
	base_name = strsplit(basename(inputVideo), ".", fixed=T)[[1]][[1]]
	img_temp_dir = 	paste(dirname(inputVideo),"/videoFaceAnalysis_temp_", base_name, sep="")
	dir.create(img_temp_dir)

	grabVideoStills(inputVideo, sampleWindow, img_temp_dir)

	# Get any images associated with this video
	img_files = list.files(path=img_temp_dir, full.names=T)


	# These are empty lists to use to safe the information
	df.o = list()
	inf = list()

	# Now loop through the images that are part of this video (which were already extracted)

	for(i in 1:length(img_files)) {

		# Pull the image and its information
		img = image_read(img_files[i])
		inf[[i]] = image_info(img)

		# This is stupid, but it is necessary to adjust the timestamping
		if(i >= 3) {
			img_timestamp = 17 + (i-3)*20
		} else {
			img_timestamp = 0
		}		

		# Detect faces in this frame
		df.o[[i]] = svc$detect_faces(Image=list(Bytes=img_files[i]), Attributes="ALL")

		# Get the details of any faces detected in this frame
		faces = df.o[[i]]$FaceDetails

		# If there are no faces in the image, then create a blank results record, with just the image id
		if(length(faces) == 0) {
			res.line = matrix(nrow=1,ncol=23)
			res.line[1,1] = img_files[i]
			res.line[1, 21] = img_timestamp			
		} else {
		# Otherwise, if there are faces in the image, go through each face to get its info	
			# create a matrix to hold the info
			res.line = matrix(nrow=length(faces), ncol=23)

			# Loop through each face and analyze it

			for(face.num in 1:length(faces)) {
				fd = faces[[face.num]]
				res.line[face.num,1] = img_files[i]
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
				res.line[face.num, 21] = img_timestamp

				# if the user specified a face collection, go into it to see if the face has an identity
				# Including the confidence value because it sometimes couldn't tell it was a face
				# at low levels of confidence
				if(!is.na(facesCollectionID) && fd$Confidence > 90) {

					# Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
					# why the image info object above is needed
					box = fd$BoundingBox
					image_width=inf[[i]]$width
					image_height=inf[[i]]$height
					x1 = box$Left*image_width
					y1 = box$Top*image_height
					x2 = x1 + box$Width*image_width
					y2 = y1 + box$Height*image_height	

					# Crop out just this particular face out of the video
					img.crop = image_crop(img, paste(box$Width*image_width,"x",box$Height*image_height,"+",x1,"+",y1, sep=""))
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
	col.names = c("frame_id", "face_id", "age_low", "age_high", "smile", "eyeglasses", "sunglasses", "gender", "beard", "mustache", "eyesopen", "mouthopen", "confused", "calm", "happy", "disgusted", "angry", "fear", "sad", "surprised", "img_timestamp", "identified_person", "identification_confidence")
	res.out[,col.numeric] = lapply(res.out[,col.numeric], as.numeric)
	res.out[,col.boolean] = lapply(res.out[,col.boolean], as.logical)
	names(res.out) = col.names	
	res.out = res.out[, c(1,21,22,23, 2:20)]	
}







videoFaceAnalysisOld = function(inputVideo, recordingStartDateTime, sampleWindow, facesCollectionID=NA) {
	require(paws)
	require(magick)
	svc = rekognition()

	recordingStartDateTime = as.POSIXct(recordingStartDateTime)

	# This reads in the stills from the video. This would output one image every 60 seconds. 
	imagesFromVideo = image_read_video(inputVideo, fps=(1/sampleWindow))

	# Create a directory structure to hold some temporary image files. 
	va_temp_dir = paste(dirname(inputVideo),"videoFaceAnalysis_temp", sep="/")
	dir.create(va_temp_dir)

	base_name = strsplit(basename(inputVideo), ".", fixed=T)[[1]][[1]]
	img_temp_dir = 	paste(dirname(inputVideo),"videoFaceAnalysis_temp", base_name, sep="/")
	dir.create(img_temp_dir)

	# This now goes through each of the images that was extracted from the video. For each, it writes it to disk and gets the face details that are in the image (if there are any)
	# Note: This is clunky and it would be better to just put them in the Amazon collection straight away. Will do that when have more time.
	df.o = list()
	inf = list()
	for(videoCounter in 1:length(imagesFromVideo)) {

		# This puts the timestamp on this clip
		img_timestamp = recordingStartDateTime + (videoCounter-1)*sampleWindow

		# Write the image to the temporary directory that was created
		image_write(imagesFromVideo[videoCounter], paste(img_temp_dir,"/","img_",videoCounter,".png", sep=""), format="png")

		# Get the information about the file (this is used later for face analysis)
		inf[[videoCounter]] = image_info(imagesFromVideo[videoCounter])		

		# Detect faces in this frame
		df.o[[videoCounter]] = svc$detect_faces(Image=list(Bytes=paste(img_temp_dir, "/", "img_",videoCounter,".png", sep="")), Attributes="ALL")

		# Get the details of any faces detected in this frame
		faces = df.o[[videoCounter]]$FaceDetails

		# If there are no faces in the image, then create a blank results record, with just the image id
		if(length(faces) == 0) {
			res.line = matrix(nrow=1,ncol=23)
			res.line[1,1] = paste(base_name, "-","img_", videoCounter, sep="")	
			res.line[1, 21] = img_timestamp			
		} else {
		# Otherwise, if there are faces in the image, go through each face to get its info	
			# create a matrix to hold the info
			res.line = matrix(nrow=length(faces), ncol=23)

			# Loop through each face and analyze it
			for(face.num in 1:length(faces)) {
				fd = faces[[face.num]]
				res.line[face.num,1] = paste(base_name, "-","img_", videoCounter, sep="")					
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
				res.line[face.num, 21] = img_timestamp

				# if the user specified a face collection, go into it to see if the face has an identity
				# Including the confidence value because it sometimes couldn't tell it was a face
				# at low levels of confidence
				if(!is.na(facesCollectionID) && fd$Confidence > 90) {

					# Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
					# why the image info object above is needed
					box = fd$BoundingBox
					image_width=inf[[videoCounter]]$width
					image_height=inf[[videoCounter]]$height
					x1 = box$Left*image_width
					y1 = box$Top*image_height
					x2 = x1 + box$Width*image_width
					y2 = y1 + box$Height*image_height	

					# Crop out just this particular face out of the video
					img.crop = image_crop(imagesFromVideo[videoCounter], paste(box$Width*image_width,"x",box$Height*image_height,"+",x1,"+",y1, sep=""))
					img.crop = image_write(img.crop, path = NULL, format = "png")
					
					# Search in a specified collection to see if we can label the identity of the face is in this crop
					faceRec = svc$search_faces_by_image(CollectionId=facesCollectionID,Image=list(Bytes=img.crop), FaceMatchThreshold=70)		

					if(length(faceRec$FaceMatches) > 0) {
						res.line[face.num, 22] = faceRec$FaceMatches[[1]]$Face$ExternalImageId
						res.line[face.num, 23] = faceRec$FaceMatches[[1]]$Face$Confidence
					} else {
						res.line[face.num, 22] = "IDENTITY NOT RECOGNIZED"
					}							
				}
			}
		}
		if(videoCounter == 1) {
			raw.res.out = res.line
		} else {
			raw.res.out = rbind(raw.res.out, res.line)
		}		
	}

	# Do some final formatting on the returned object
	res.out = data.frame(raw.res.out, stringsAsFactors=F)
	col.numeric = c(2:4, 13:20, 23)
	col.boolean = c(5:7,9:12)
	col.names = c("frame_id", "face_id", "age_low", "age_high", "smile", "eyeglasses", "sunglasses", "gender", "beard", "mustache", "eyesopen", "mouthopen", "confused", "calm", "happy", "disgusted", "angry", "fear", "sad", "surprised", "img_timestamp", "identified_person", "identification_confidence")

	res.out[,col.numeric] = lapply(res.out[,col.numeric], as.numeric)
	res.out[,col.boolean] = lapply(res.out[,col.boolean], as.logical)
	res.out[,21] = as.POSIXct(as.numeric(res.out[,21]), origin="1970-01-01")
	names(res.out) = col.names	
	res.out = res.out[, c(1,21,22,23, 2:20)]
	return(res.out)
}


############################################################
# textConversationAnalysis Function
############################################################
# Conversation Analysis Function
# This function takes in the output of one of the other functions (either processZoomChat or processZoomTranscript) and produces # a set of conversation measures. I don't know conversation analysis, so this is just a rough cut of things I was
# curious about. If you are a conversation expert and want to contribute, let me know!
#
# Example Call: 	o = textConversationAnalysis(inputData=outputOfOtherFunctions, inputType="chat", sentiment=TRUE, speakerId = "user_name")

# INPUT ARGUMENTS:

# inputData			The output from either the processZoomChat or processZoomTranscript functions
# inputType			either "chat" or "transcript"
# speakerId			The name of the variable in inputData that contains the unique identfier (e.g., "user_name")
# sentiment 		Boolean to indicate whether you want to output sentiment analysis metrics, as well.

# OUTPUT: 
# this function outputs a list with two items. One is a set of measures aggregated for the overall file (either chat or transcript). The second is a set of measures aggregated to the level of the individual speaker. The function presumes that speakerId is a unique identifier and treats each value as the ID for a speaker.

# Note that any time measures in the output are represented as seconds.  

# Most variable names are self-explanatory, but I'll highlight a few: 

#### For the overall transcript output ####

# utterance_gap_x: 		This is the average number of seconds between one person's utterance and the next person's utterance
# utterance_gap_sd: 	The SD of the utterance gaps
# burstiness_raw:		This is a measure of how concentrated (in time) utterances are. It is not adjusted for # of utterances

#### For the speaker-level transcript output ####

# utterance_gap_x: 		This is the average number of seconds (from the last utterance) that pass before this person makes an utterance

#### For the overall chat output ####

# message_gap_x: 		This is the average number of seconds between one person's message and the next person's message
# message_gap_sd: 		The SD of the message gaps
# burstiness_raw:		This is a measure of how concentrated (in time) chat messages are. It is not adjusted for # of messages

#### For the speaker-level chat output ####

# message_gap_x: 		This is the average number of seconds (from the last message) that pass before this person sends a message


############################################################


textConversationAnalysis = function(inputData, inputType, speakerId, sentiment=FALSE, sentiDone=FALSE) {
	require(data.table)
	########################################
	# IF THE USER REQUESTED AN ANALYSIS OF A TRANSCRIPT FILE, DO THE FOLLOWING
	########################################			

	if(inputType=="transcript") {

		########################################
		# Do the sentiment analysis if it was requested
		########################################
		if(sentiment==TRUE) {
			if(sentiDone==FALSE) {
				inputData = sentiOut(inputData=inputData, idVar="utterance_id", textVar="utterance_message", languageCodeVar="utterance_language")
			}
			tab_denom = nrow(inputData[!is.na(inputData$sent_class), ])
			utterance_positive_pct = nrow(inputData[inputData$sent_class=="POSITIVE", ])/tab_denom
			utterance_neutral_pct = nrow(inputData[inputData$sent_class=="NEUTRAL", ])/tab_denom
			utterance_negative_pct = nrow(inputData[inputData$sent_class=="NEGATIVE", ])/tab_denom
			utterance_mixed_pct = nrow(inputData[inputData$sent_class=="MIXED", ])/tab_denom	

			utterance_mixed_x = mean(inputData$Mixed, na.rm=T)					
			utterance_neutral_x = mean(inputData$Neutral, na.rm=T)					
			utterance_negative_x = mean(inputData$Negative, na.rm=T)					
			utterance_positive_x = mean(inputData$Positive, na.rm=T)										

			utterance_mixed_sd = sd(inputData$Mixed, na.rm=T)					
			utterance_neutral_sd = sd(inputData$Neutral, na.rm=T)					
			utterance_negative_sd = sd(inputData$Negative, na.rm=T)					
			utterance_positive_sd = sd(inputData$Positive, na.rm=T)		
			sent.cols = cbind(utterance_positive_pct, utterance_positive_x, utterance_positive_sd, utterance_neutral_pct, utterance_neutral_x, utterance_neutral_sd, utterance_negative_pct, utterance_negative_x, utterance_negative_sd, utterance_mixed_pct, utterance_mixed_x, utterance_mixed_sd)		
		}

		########################################
		# Create a transcript-level output
		########################################

		# First, get some overall statistics - all time units are in seconds
		total_recorded_time = as.numeric(difftime(max(inputData$utterance_end_time), min(inputData$utterance_start_time), units="secs"))
		utterance_time_window_sum = sum(inputData$utterance_time_window)
		silent_time_sum = total_recorded_time-utterance_time_window_sum
		utterance_time_window_x = mean(inputData$utterance_time_window, na.rm=T)
		utterance_time_window_sd = sd(inputData$utterance_time_window, na.rm=T)

		num_unique_speakers = length(unique(inputData[,speakerId]))
		num_utterances = nrow(inputData)		

		# Second, if there is more than one utterance, get the information for burstiness, which is calculated as the CV of 
		# the gap between utterances (so concentration of speech)
		inputData$utterance_gap = NA
		if(nrow(inputData) >= 2) {

			# Figure out the gap from one utterance to the next
			for(i in 2:nrow(inputData)) {
				# start time of current utterance - end time of prior utterance (in seconds)
				inputData[i, "utterance_gap"] = as.numeric(difftime(inputData[i, "utterance_start_time"], inputData[(i-1), "utterance_end_time"], units="secs"))
			}

			utterance_gap_x = mean(inputData$utterance_gap, na.rm=T)
			utterance_gap_sd = sd(inputData$utterance_gap, na.rm=T)		

			burstiness_raw = (sd(inputData$utterance_gap, na.rm=T)-mean(inputData$utterance_gap, na.rm=T))/(sd(inputData$utterance_gap, na.rm=T)+mean(inputData$utterance_gap, na.rm=T))
		} else {

			utterance_gap_x = NA
			utterance_gap_sd = NA
			burstiness_raw = NA
		}


		transcript_out = cbind(total_recorded_time, num_utterances, num_unique_speakers, utterance_time_window_sum, silent_time_sum, utterance_time_window_x, utterance_time_window_sd, utterance_gap_x, utterance_gap_sd, burstiness_raw)
		if(sentiment==TRUE) transcript_out = cbind(transcript_out, sent.cols)		

		########################################
		# Create an individual-level output
		# Note, the presumption is that user_name is something unique -- hopefully it is!
		########################################
		dt = data.table(inputData)

		if(sentiment == TRUE) {
			agg.dt = dt[,list(utterance_time_window_sum = sum(utterance_time_window, na.rm=T), num_utterances = .N, utterance_time_x = mean(utterance_time_window, na.rm=T), utterance_time_sd = sd(utterance_time_window, na.rm=T), utterance_gap_x = mean(utterance_gap, na.rm=T), utterance_gap_sd = sd(utterance_gap, na.rm=T),
				utterance_positive_pct = sum(sent_class=="POSITIVE")/.N, utterance_positive_x = mean(Positive, na.rm=T), utterance_positive_sd = sd(Positive, na.rm=T), 
				utterance_negative_pct = sum(sent_class=="NEGATIVE")/.N, utterance_negative_x = mean(Negative, na.rm=T), utterance_negative_sd = sd(Negative, na.rm=T), 				 		
				utterance_neutral_pct = sum(sent_class=="NEUTRAL")/.N, utterance_neutral_x = mean(Neutral, na.rm=T), utterance_neutral_sd = sd(Neutral, na.rm=T), 				 						
				utterance_mixed_pct = sum(sent_class=="MIXED")/.N, utterance_mixed_x = mean(Mixed, na.rm=T), utterance_mixed_sd = sd(Mixed, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId

		} else {
			agg.dt = dt[,list(utterance_time_window_sum = sum(utterance_time_window, na.rm=T), num_utterances = .N, utterance_time_x = mean(utterance_time_window, na.rm=T), utterance_time_sd = sd(utterance_time_window, na.rm=T), utterance_gap_x = mean(utterance_gap, na.rm=T), utterance_gap_sd = sd(utterance_gap, na.rm=T)), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId
		}

		agg.out = data.frame(agg.dt)

		res.out = list("TRANSCRIPT-LEVEL" = data.frame(transcript_out, stringsAsFactors=F), "SPEAKER-LEVEL" = agg.out)

	########################################
	# IF THE USER REQUESTED AN ANALYSIS OF A CHAT FILE, DO THE FOLLOWING
	########################################		

	} else if(inputType=="chat") {

		########################################
		# Do the sentiment analysis if it was requested
		########################################
		if(sentiment==TRUE) {
			if(sentiDone==FALSE) {
				inputData = sentiOut(inputData=inputData, idVar="message_id", textVar="message", languageCodeVar="message_language")
			}
			tab_denom = nrow(inputData[!is.na(inputData$sent_class), ])
			message_positive_pct = nrow(inputData[inputData$sent_class=="POSITIVE", ])/tab_denom
			message_neutral_pct = nrow(inputData[inputData$sent_class=="NEUTRAL", ])/tab_denom
			message_negative_pct = nrow(inputData[inputData$sent_class=="NEGATIVE", ])/tab_denom
			message_mixed_pct = nrow(inputData[inputData$sent_class=="MIXED", ])/tab_denom	

			message_mixed_x = mean(inputData$Mixed, na.rm=T)					
			message_neutral_x = mean(inputData$Neutral, na.rm=T)					
			message_negative_x = mean(inputData$Negative, na.rm=T)					
			message_positive_x = mean(inputData$Positive, na.rm=T)										

			message_mixed_sd = sd(inputData$Mixed, na.rm=T)					
			message_neutral_sd = sd(inputData$Neutral, na.rm=T)					
			message_negative_sd = sd(inputData$Negative, na.rm=T)					
			message_positive_sd = sd(inputData$Positive, na.rm=T)		
			sent.cols = cbind(message_positive_pct, message_positive_x, message_positive_sd, message_neutral_pct, message_neutral_x, message_neutral_sd, message_negative_pct, message_negative_x, message_negative_sd, message_mixed_pct, message_mixed_x, message_mixed_sd)	
		}

		########################################
		# Create a chat-level output
		########################################
		inputData$message_numchars = nchar(inputData$message)

		# First, get some overall statistics - all time units are in seconds
		total_recorded_time = as.numeric(difftime(max(inputData$message_time), min(inputData$message_time), units="secs"))
		message_numchars_sum = sum(inputData$message_numchars)
		message_numchars_x = mean(inputData$message_numchars)
		message_numchars_sd = sd(inputData$message_numchars)				

		num_unique_messagers = length(unique(inputData[,speakerId]))
		num_messages = nrow(inputData)		

		# Second get the information for burstiness, which is calculated as the CV of 
		# the gap between messages (so concentration of speech)

		# Figure out the gap from one message to the next
		inputData$message_gap = NA
		if(num_messages > 1) {
			for(i in 2:nrow(inputData)) {
				# start time of current utterance - end time of prior utterance (in seconds)
				inputData[i, "message_gap"] = as.numeric(difftime(inputData[i, "message_time"], inputData[(i-1), "message_time"], units="secs"))
			}			
		}

		message_gap_x = mean(inputData$message_gap, na.rm=T)
		message_gap_sd = sd(inputData$message_gap, na.rm=T)		

		burstiness_raw = (sd(inputData$message_gap, na.rm=T)-mean(inputData$message_gap, na.rm=T))/(sd(inputData$message_gap, na.rm=T)+mean(inputData$message_gap, na.rm=T))

		chat_out = cbind(total_recorded_time, num_messages, message_numchars_sum, num_unique_messagers, message_gap_x, message_gap_sd, burstiness_raw)
		if(sentiment==TRUE) chat_out = cbind(chat_out, sent.cols)		

		########################################
		# Create an individual-level output
		# Note, the presumption is that user_name is something unique -- hopefully it is!
		########################################		

		dt = data.table(inputData)

		if(sentiment == TRUE) {
			agg.dt = dt[,list(message_numchars_sum = sum(message_numchars, na.rm=T), num_messages = .N, message_numchars_x = mean(message_numchars), message_numchars_sd = sd(message_numchars), message_gap_x = mean(message_gap, na.rm=T), message_gap_sd = sd(message_gap, na.rm=T),
				message_positive_pct = sum(sent_class=="POSITIVE")/.N, message_positive_x = mean(Positive, na.rm=T), message_positive_sd = sd(Positive, na.rm=T), 
				message_negative_pct = sum(sent_class=="NEGATIVE")/.N, message_negative_x = mean(Negative, na.rm=T), message_negative_sd = sd(Negative, na.rm=T), 				 		
				message_neutral_pct = sum(sent_class=="NEUTRAL")/.N, message_neutral_x = mean(Neutral, na.rm=T), message_neutral_sd = sd(Neutral, na.rm=T), 				 						
				message_mixed_pct = sum(sent_class=="MIXED")/.N, message_mixed_x = mean(Mixed, na.rm=T), message_mixed_sd = sd(Mixed, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId

		} else {
			agg.dt = dt[,list(message_numchars_sum = sum(message_numchars, na.rm=T), num_messages = .N, message_numchars_x = mean(message_numchars), message_numchars_sd = sd(message_numchars), message_gap_x = mean(message_gap, na.rm=T), message_gap_sd = sd(message_gap, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId
		}

		agg.out = data.frame(agg.dt)
		res.out = list("CHAT-LEVEL" = chat_out, "USER-LEVEL" = agg.out)		
	}
}





############################################################
############################################################
#  Functions that are in alpha stage. I have used these, but haven't had time to test them or, for some, provide commentary and instructions. 
############################################################
############################################################


############################################################
# transcribeZoomAudio Function
############################################################

# Zoom Audio File Processing, Function to launch transcription jobs
# This function starts an audio transcription job only == it does not output anything of use. However, 
# it is useful for batch uploading audio files and starting transcription jobs for them. 

# This can be done with a local file (uploads to a specified s3 bucket) or with a file that already
# exists in an s3 bucket

# example call:				transcribeZoomAudio(fileLocation="local", bucketName="my-transcription-bucket", filePath="mylocalfile.m4a", jobName="mylocalfile.m4a", languageCode="en-US")

# INPUT ARGUMENTS: 
# fileLocation: 			either "local" or "s3" - if local, then this function will upload the file to the specified bucket
# bucketName: 				name of an existing s3 bucket that you are using for storing audio files to transcribe and finished transcriptions
# filePath: 				the path to the local file or to the s3 file (depending on whether it is "local" or "s3")
# jobName:					the name of the transcription job for aws -- I set this to the same as the filename (without path) for convenience
# numSpeakers:				this helps AWS identify the speakers in the clip - specify how many speakers you expect
# languageCode:				the code for the language (e.g., en-US)

# OUTPUT: 
# None

transcribeZoomAudio = function(fileLocation, bucketName, filePath, jobName, numSpeakers, languageCode) {
	require(paws)

	# First, if the file location is local, then upload it into the 
	# designated s3 bucket
	if(fileLocation == "local") {
		localFilePath = filePath
		svc = s3()
		upload_file = file(localFilePath, "rb")
		upload_file_in = readBin(upload_file, "raw", n = file.size(localFilePath))
		svc$put_object(Body = upload_file_in, Bucket = bucketName, Key = jobName)
		filePath = paste("s3://", bucketName, "/",jobName, sep="")
		close(upload_file)
	}

	svc = transcribeservice()	
	svc$start_transcription_job(TranscriptionJobName = jobName, LanguageCode = languageCode, Media = list(MediaFileUri = filePath), OutputBucketName = bucketName, Settings = list(ShowSpeakerLabels=TRUE, MaxSpeakerLabels=numSpeakers))
}


############################################################
# processZoomAudio Function
############################################################

# Zoom Audio File Processing, process finished transcriptions
# This function parses the JSON transcription completed by AWS transcribe. 
# The output is the same as the processZoomTranscript function.

# example call:				audio.out = processZoomAudio(bucketName = "my-transcription-bucket", jobName = "mylocalfile.m4a", localDir = "path-to-local-directory-for-output", speakerNames = c("Tom Smith", "Jamal Jones", "Jamika Jensen"), recordingStartDateTime = "2020-06-20 17:00:00", writeTranscript=TRUE)

# INPUT ARGUMENTS: 
# bucketName: 				name of the s3 bucket where the finished transcript is stored
# jobName:					name of the transcription job (see above - i usually set this to the filename of the audio)
# localDir:					a local directory where you can save the aws json file and also a plain text file of the transcribed text
# speakerNames: 			a vector with the Zoom user names of the speakers, in the order in which they appear in the audio clip. 
# recordingStartDateTime:	the date/time that the meeting recording started
# writeTranscript:			a boolean to indicate whether you want to output a plain text file of the transcript			

# OUTPUT: 
# utterance_id:				an incremented numeric identifier for a marked speech utterance
# utterance_start_seconds	the number of seconds from the start of the recording (when it starts)
# utterance_start_time:		the timestamp for the start of the utterance
# utterance_end_seconds		the number of seconds from the start of the recording (when it ends)
# utterance_end_time:		the timestamp for the end of the utterance
# utterance_time_window:	the number of seconds that the utterance took
# user_name: 				the name attached to the utterance
# utterance_message:		the text of the utterance
# utterance_language:		the language code for the transcript



processZoomAudio = function(bucketName, jobName, localDir, speakerNames=c(), recordingStartDateTime, writeTranscript) {
	require(paws)
	require(jsonlite)

	transcriptName = paste(jobName, "json", sep=".")
	svc = s3()
	transcript = svc$get_object(Bucket = bucketName, Key = transcriptName)
	# Write the binary component of the downloaded object to the local path 
	writeBin(transcript$Body, con = paste(localDir, transcriptName, sep="/"))
	tr.json = fromJSON(paste(localDir, transcriptName, sep="/"))

	if(writeTranscript) {
		outTranscript = paste(localDir, "/", jobName, ".txt", sep="")
		write(tr.json$results$transcripts$transcript, outTranscript)
	}

	# This IDs the words as AWS broke out the different segments of speech
	for(i in 1:length(tr.json$results$speaker$segments$items)){

		res.line = tr.json$results$speaker$segments$items[[i]]
		res.line$segment_id = i
		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}

	}

	segments = res.out	
	segment_cuts = tr.json$results$speaker$segments[,c("start_time", "speaker_label", "end_time")]	

	# Pull this apart to just get the word/punctuation with the most confidence 
	# Not currently dealing with any of the alternatives that AWS could give
	for(i in 1:length(tr.json$results$items$alternatives)) {

		res.line = tr.json$results$items$alternatives[[i]]

		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}

	}

	words = cbind(res.out, tr.json$results$items[,c("start_time", "end_time", "type")])
	words = words[words$type == "pronunciation", ]
	words_segments = merge(words, segments, by=c("start_time", "end_time"), all.x=T)

	words_segments$start_time = as.numeric(words_segments$start_time)
	words_segments$end_time = as.numeric(words_segments$end_time)

	words_segments = words_segments[order(words_segments$start_time), ]
	segment_ids = unique(words_segments$segment_id)
	i = 1


	segment_cuts$utterance_id = NA
	segment_cuts$utterance_message = NA
	for(i in 1:length(segment_ids)) {
		utterance_id = segment_ids[i]
		segment_cuts[i, "utterance_id"] = utterance_id		
		segment_cuts[i, "utterance_message"] = paste0(words_segments[words_segments$segment_id == utterance_id, "content"], collapse=" ")
	}	

	if(length(speakerNames) > 0) {
		user_names = data.frame(0:(length(speakerNames)-1), speakerNames, stringsAsFactors=F)
		names(user_names) = c("speaker_label", "user_name")
		user_names$speaker_label = paste("spk",user_names$speaker_label, sep="_")
		segment_cuts = merge(segment_cuts, user_names, by="speaker_label", all.x=T)
	}

	names(segment_cuts)[2:3] = c("utterance_start_seconds", "utterance_end_seconds")
	segment_cuts[, 2:3] = lapply(segment_cuts[, 2:3], function(x) as.numeric(x))
	segment_cuts = segment_cuts[order(segment_cuts$utterance_start_seconds), ]

	# Now turn these into actual datetime values
	recordingStartDateTime = as.POSIXct(recordingStartDateTime)
	segment_cuts$utterance_start_time = recordingStartDateTime + segment_cuts$utterance_start_seconds
	segment_cuts$utterance_end_time = recordingStartDateTime + segment_cuts$utterance_end_seconds

	# Create a time window (in seconds) for the utterances -- how long is each in seconds
	segment_cuts$utterance_time_window = as.numeric(difftime(segment_cuts$utterance_end_time, segment_cuts$utterance_start_time, units="secs"))

	# Prepare the output file
	res.out = segment_cuts[, c("utterance_id", "utterance_start_seconds", "utterance_start_time", "utterance_end_seconds", "utterance_end_time", "utterance_time_window", "user_name", "utterance_message")]

	# Mark as unidentified any user with a blank username
	res.out$user_name = ifelse(res.out$user_name == "" | is.na(res.out$user_name), "UNIDENTIFIED", res.out$user_name)		

	# Add the language code
	res.out$utterance_language = languageCode

	return(res.out)		

}

############################################################
# The following functions  are used  to do a windowed analysis of the transcript. 
############################################################

makeTimeWindows = function(inputData, inputType, windowSize) {
	inputData=inputData[order(inputData$utterance_id), ]
	if(inputType == "transcript") {
		
		inputData$window_id = NA
		inputData$window_start_seconds = NA
		inputData$window_end_seconds = NA		
		count = 1
		for(i in ceiling(max(inputData$utterance_end_seconds)/windowSize):0) {
			window_start_seconds = i*windowSize
			window_end_seconds = (i+1)*windowSize

			inputData$window_id = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, (i+1), inputData$window_id) 

			inputData$window_start_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_start_seconds, inputData$window_start_seconds) 

			inputData$window_end_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_end_seconds, inputData$window_end_seconds) 			

			time_window_info = data.frame((i+1), window_start_seconds, window_end_seconds)

			names(time_window_info) = c("window_id", "window_start_seconds", "window_end_seconds")

			if(count == 1) {
				res.line = time_window_info
			} else {
				res.line = rbind(res.line, time_window_info)
			}
			count = count + 1
		}

	}
	return(list(inputData, res.line[order(res.line$window_id), ]))
}


windowedTextConversationAnalysis = function(inputData, inputType, sentiment, speakerId, windowSize, sentiDone) {
	t.out.windowed = makeTimeWindows(inputData, inputType, windowSize)
	inputData = t.out.windowed[[1]]

	# Create a blank set that gives each user an opportunity to have an aggregate
	# metric during each of the time windows
	participants = sort(unique(inputData[,speakerId]))
	user_name = rep(participants,max(inputData$window_id))
	window_id = sort(rep(1:max(inputData$window_id), length(participants)))
	p3 = data.frame(user_name, window_id, stringsAsFactors=F)
	p4 = merge(p3, t.out.windowed[[2]], by="window_id")

	# Now, loop through the time windows 
	count = 1

	for(win in 1:max(inputData$window_id)) {

		windowed.input = inputData[inputData$window_id == win, ]

		if(nrow(windowed.input) > 0) {

			res.line = textConversationAnalysis(inputData=windowed.input, inputType="transcript", sentiment=sentiment, speakerId = speakerId, sentiDone=sentiDone)

			grp.res.line = res.line[[1]]
			grp.res.line$window_id = win

			ind.res.line = res.line[[2]]
			ind.res.line$window_id = win

			if(count == 1) {
				grp.res.out = grp.res.line
				ind.res.out = ind.res.line		
			} else {
				grp.res.out = rbind(grp.res.out, grp.res.line)
				ind.res.out = rbind(ind.res.out, ind.res.line)
			}
			count = count + 1
		}
	}	

	p5 = merge(p4, ind.res.out, by=c("user_name", "window_id"), all.x=T)
	p5$utterance_time_window_sum = ifelse(is.na(p5$utterance_time_window_sum), 0, p5$utterance_time_window_sum)
	p5$num_utterances = ifelse(is.na(p5$num_utterances), 0, p5$num_utterances)

	grp1 = merge(t.out.windowed[[2]], grp.res.out, by=c("window_id"), all.x=T)
	grp1$utterance_time_window_sum = ifelse(is.na(grp1$utterance_time_window_sum), 0, grp1$utterance_time_window_sum)
	grp1$num_utterances = ifelse(is.na(grp1$num_utterances), 0, grp1$num_utterances)	
	grp1$total_recorded_time = ifelse(is.na(grp1$total_recorded_time), 0, grp1$total_recorded_time)		

	grp1$num_unique_speakers = ifelse(is.na(grp1$num_unique_speakers), 0, grp1$num_unique_speakers)			

	
	return(list("TRANSCRIPT-LEVEL" = grp1, "SPEAKER-LEVEL" = p5))	
}

turnTaking = function(inputData, inputType, speakerId) {
	require(data.table)
	if(inputType=="transcript") {
		t.out = inputData

		# Get the names of the unique speakers in this file
		uniqueUsers = sort(unique(t.out[,speakerId]))

		# Create an empty matrix that will hold counts of the utterances
		userMatrix = matrix(data=rep(0, length(uniqueUsers)^2), nrow=length(uniqueUsers), ncol=length(uniqueUsers))
		rownames(userMatrix) = uniqueUsers
		colnames(userMatrix) = uniqueUsers		

		# Create an empty dyad-level dataset that will be easy to use and do any of the norming that
		# might be useful for SRM

		dyadData = data.frame(list(speakerBefore=sort(rep(uniqueUsers, length(uniqueUsers))), speakerCurrent = rep(uniqueUsers, length(uniqueUsers)), numUtterances = rep(0, length(uniqueUsers)^2)))

		# Go through the transcript, utterance-by-utterance to count the number of times a given speaker followed
		# someone in the matrix
		# Skipping the first and last utterances so that it is a balanced matrix
		for(i in 2:(nrow(t.out)-1)) {

			# this is name of the person who preceded the current speaker
			speakerBefore = t.out[(i-1), speakerId]
			speakerCurrent = t.out[i, speakerId]

			# Augment that cell in the matrix. The matrix is such that the row is the preceding speaker (from) and the 
			# column is the current speaker (to)
			userMatrix[speakerBefore, speakerCurrent] = userMatrix[speakerBefore, speakerCurrent] + 1

			# Augment that row in the dyadic dataset
			dyadData[dyadData$speakerBefore == speakerBefore & dyadData$speakerCurrent == speakerCurrent, "numUtterances"] = dyadData[dyadData$speakerBefore == speakerBefore & dyadData$speakerCurrent == speakerCurrent, "numUtterances"] + 1
		}

		# Create a few objects that give different normed versions of this
		rawCount = userMatrix
		rawCountNoDiag = userMatrix
		diag(rawCountNoDiag) = NA

		# This is the percent of utterances across the whole matrix (excluding the diagonal)
		pctByConvo = rawCountNoDiag/sum(rawCountNoDiag, na.rm=T)

		# This is the percent of utterances normed by the speaker. So it asks 
		# what percent of a given speakers utterances follow a given person
		pctBySpeaker = rawCountNoDiag
		for(c in 1:ncol(rawCountNoDiag)) {

			pctBySpeaker[,c] = rawCountNoDiag[,c]/sum(rawCountNoDiag[,c], na.rm=T)
		}

		# Create the normed variables for dyadData - excluding self for these
		dyadDt = data.table(dyadData)
		speakerAgg = data.frame(dyadDt[speakerBefore != speakerCurrent, list(sumSpeakerCurrentUtterances = sum(numUtterances)), by=list(speakerCurrent)])
		dyadData = merge(dyadData, speakerAgg, by="speakerCurrent")
		dyadData[dyadData$speakerBefore == dyadData$speakerCurrent, "sumSpeakerCurrentUtterances"] = NA		
		dyadData$pctBySpeakerCurrent = dyadData$numUtterances/dyadData$sumSpeakerCurrentUtterances
		dyadData$pctByConvo = dyadData$numUtterances/sum(dyadData[dyadData$speakerBefore != speakerCurrent, "numUtterances"])
		dyadData$pctByConvo = ifelse(dyadData$speakerBefore == dyadData$speakerCurrent, NA, dyadData$pctByConvo)

		# Create an individual-level file that represents the average percent of a speaker's utterances (and of the conversation) that come after one of the people
		dyadDt2 = data.table(dyadData)
		speakerAgg2 = data.frame(dyadDt2[speakerBefore != speakerCurrent, list(numAfterSpeaker = sum(numUtterances, na.rm=T), pctAfterSpeaker = mean(pctBySpeakerCurrent, na.rm=T), pctAfterConvo = mean(pctByConvo, na.rm=T)), by=list(speakerBefore)])
		names(speakerAgg2)[1] = speakerId



	}
	o.list = list("rawCount" = rawCount, "rawCountNoDiag" = rawCountNoDiag, "pctByConvo" = pctByConvo, "pctBySpeaker" = pctBySpeaker, "dyadData" = dyadData, "indivData" = speakerAgg2)
	return(o.list)
}


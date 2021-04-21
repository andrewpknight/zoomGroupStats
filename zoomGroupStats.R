############################################################
# Author: 			Andrew Knight (http://apknight.org)
	
# Last Update:		2021-04-21 15:00 US CDT
# Update Notes:		
#					- Some bug fixes 
#					- Major changes to all functions - basically, nothing 
#						from the past will work (sorry)
# 					- Preparations to convert this into a formal package

# I created this as a way to help people do social science research through web-based meetings (i.e., Zoom). 
# It's still a work in progress, but this is a start. If you would like to use it or help build it, 
# please reach out! 

# to cite this currently, please use: 
# Knight, A. P. (2021). zoomGroupStats: Using R to analyze Zoom recordings. http://apknight.org/zoomGroupStats.R. 

############################################################
# OVERVIEW OF FUNCTIONS 
############################################################
# This script contains functions to use for analyzing recorded Zoom sessions
# This is a work in progress and more are coming. 

# FUNCTIONS THAT ARE HEAVILY COMMENTED
# processZoomParticipantsInfo	Parses the downloaded meeting particiapnts file from Zoom
# processZoomChat			Parses the downloaded chat file from a recorded Zoom session
# processZoomTranscript		Parses the downloaded transcript from a recorded Zoom session
# textSentiment				Conducts a sentiment analysis on either the Chat or Transcript
# videoFaceAnalysis 		Analyzes the video from a Zoom session and outputs face/emotion measures
# textConversationAnalysis	Analyzes either chat or transcript and outputs conversation metrics

# FUNCTIONS THAT ARE IN ALPHA STAGE AND THAT I'M TESTING
# transcribeZoomAudio		uses AWS transcription service to process an audio file
# processZoomAudio			parse the output of the AWS transcription
# makeTimeWindows			Creates time windows in a transcript to do windowed analyses
# windowedTextConversationAnalysis	Conducts a windowed conversation analysis
# turnTaking				Does an analysis of conversation turn-taking

# Note you will require the following packages to run these: 
# reshape2
# stringr
# paws
# magick
# data.table
# jsonlite

# You will also require an aws account with privileges for rekognition and comprehend to use the 
# text analysis and video analysis. If you don't know how to do this, please: 
# Search online for (a) setting up AWS account; (b) setting up paws. I found the following useful: 
# https://github.com/paws-r/paws/blob/master/docs/credentials.md

# For doing face analysis, you'll also need to have ffmpeg installed on your machine (this change
# was motivated by the speed of video processing using ffmpeg)

# If, after you try you are still struggling, I can give guidance on this if useful--just contact me. 

########################
# This will load the current set of packages
########################
zoomGroupStatsDir = "http://apknight.org/zoomGroupStats_functions/"

source(paste(zoomGroupStatsDir, "makeTimeWindows.R", sep=""))
source(paste(zoomGroupStatsDir, "processZoomAudio.R", sep=""))
source(paste(zoomGroupStatsDir, "processZoomParticipantsInfo.R", sep=""))
source(paste(zoomGroupStatsDir, "processZoomTranscript.R", sep=""))
source(paste(zoomGroupStatsDir, "textConversationAnalysis.R", sep=""))
source(paste(zoomGroupStatsDir, "textSentiment.R", sep=""))
source(paste(zoomGroupStatsDir, "transcribeZoomAudio.R", sep=""))
source(paste(zoomGroupStatsDir, "turnTaking.R", sep=""))	
source(paste(zoomGroupStatsDir, "videoFaceAnalysis.R", sep=""))
source(paste(zoomGroupStatsDir, "windowedConversationAnalysis.R", sep=""))


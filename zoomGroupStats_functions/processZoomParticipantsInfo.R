############################################################
# processZoomParticipantsInfo Function
############################################################

# Zoom Meeting / Participants info parsing function

# This function parses the information from the downloadable meeting information 
# file in Zooms "reports" section. The function presumes that you have 
# checked the box to include the meeting information in the file. That means
# that there is a header (2 rows) containing the zoom meeting information. 
# Following that header are four columns: 
# Name (Original Name):		This is the name the user logged in first using
# User Email: 				The user's email address (if logged in to Zoom)
# Total Duration: 			How many minutes the user spent in the session
# Guest:					Is this person a guest or not?

# example call:			partInfo = processZoomParticipantsInfo("~/Desktop/zoom_meeting_participants_export.csv") 

# INPUT ARGUMENTS:	
# inputPath:			The path to the Zoom export file

# OUTPUT: This function outputs a list containing two data frames: 

####### LIST ITEM #1, meetInfo = MEETING-LEVEL INFORMATION #######
# There is a single record in this data frame, containing the following info: 
# meetingId:			Numeric meeting identifier from Zoom
# meetingTopic:			String meeting description
# meetingStartTime:		Start date-time of the meeting in YYYY-MM-DD HH:MM:SS
# meetingEndTime:		End date-time of the meeting in YYYY-MM-DD HH:MM:SS
# meetingDuration:		Number of minutes that the meeting lasted
# numParticipants:		Number of unique Zoom login names (original name) for this meeting

####### LIST ITEM #2, partInfo = PARTICIPANT-LEVEL INFORMATION #######
# There are N records, where N is the number of logins to this meeting, containing the following info: 
# userName: 				String user name for this user
# userEmail:				String email address attached to this user's Zoom account
# userDuration:				Number of minutes that this user was logged in
# userGuest:				Indicator of whether this person is a guest user of Zoom

processZoomParticipantsInfo = function(inputPath) {

	# convDate transforms the Zoom output for a timestamp into 
	# ISO 8601, but without the "T" delimiter: YYYY-MM-DD HH:MM:SS
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

	meetInfo = read.table(inputPath, header=F, nrows=1, skip=1, sep=",", stringsAsFactors=F, col.names=c("meetingId", "meetingTopic", "meetingStartTime", "meetingEndTime", "userEmail", "meetingDuration", "numParticipants", "blankCol"))
	meetInfo$blankCol = NULL

	# Change the date column to something more useable in the other functions	
	meetInfo$meetingStartTime = convDate(meetInfo$meetingStartTime)
	meetInfo$meetingEndTime = convDate(meetInfo$meetingEndTime)

	partInfo = data.frame(read.delim(inputPath, header=T, skip=3, sep=",", stringsAsFactors=F))
	names(partInfo) = c("userName", "userEmail", "userDuration", "userGuest")	

	outInfo = list(meetInfo = meetInfo, partInfo = partInfo)	

	return(outInfo)
}
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
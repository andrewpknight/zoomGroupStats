#' Parsed batch info file in a recorded 'Zoom' meeting 
#' 
#' @format A data frame with 3 rows of 13 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{fileRoot}{the prefix to the files for this particular meeting}
#' \item{participants}{binary indicating whether there is a participants file downloaded}
#' \item{transcript}{binary indicating whether there is a transcript file downloaded}
#' \item{chat}{binary indicating whether there is a chat file downloaded}
#' \item{video}{binary indicating whether there is a video file downloaded}
#' \item{sessionStartDateTime}{start of the actual session as a character YYYY-MM-DD HH:MM:SS}
#' \item{recordingStartDateTime}{start of the actual recording as a character YYYY-MM-DD HH:MM:SS}
#' \item{participants_processed}{binary indicating whether there is a participants file already processed}
#' \item{transcript_processed}{binary indicating whether there is a transcript file already processed}
#' \item{chat_processed}{binary indicating whether there is a chat file already processed}
#' \item{video_processed}{binary indicating whether there is a video file already processed}
#' \item{dirRoot}{character giving the directory in which all files will be found}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_batch_info"
#'
#'
#' Parsed spoken language in a 'Zoom' meeting.
#' 
#' @format A data frame with 30 rows of 12 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{userName}{'Zoom' display name attached to this speaker}
#' \item{utteranceId}{an incremented numeric identifier for a marked speech utterance}
#' \item{utteranceStartSeconds}{when the utterance started as the number of seconds from the start of the recording}
#' \item{utteranceStartTime}{timestamp for the start of the utterance}
#' \item{utteranceEndSeconds}{when the utterance ended as the number of seconds from the start of the recording}
#' \item{utteranceEndTime}{timestamp for the end of the utterance}
#' \item{utteranceTimeWindow}{duration of the utterance, in seconds}
#' \item{utteranceMessage}{the text of the utterance}
#' \item{utteranceLanguage}{language code of the utterance}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_transcript_processed"

#' Parsed spoken language in a 'Zoom' meeting with AWS-based sentiment analysis.
#' 
#' @format A data frame with 30 rows of 17 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{utteranceId}{an incremented numeric identifier for a marked speech utterance}
#' \item{userName}{'Zoom' display name attached to this speaker}
#' \item{utteranceStartSeconds}{when the utterance started as the number of seconds from the start of the recording}
#' \item{utteranceStartTime}{timestamp for the start of the utterance}
#' \item{utteranceEndSeconds}{when the utterance ended as the number of seconds from the start of the recording}
#' \item{utteranceEndTime}{timestamp for the end of the utterance}
#' \item{utteranceTimeWindow}{duration of the utterance, in seconds}
#' \item{utteranceMessage}{the text of the utterance}
#' \item{utteranceLanguage}{language code of the utterance}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' \item{aws_sentClass}{character giving the sentiment classification of this text}
#' \item{aws_positive}{probability that this text is mixed emotion}
#' \item{aws_negative}{probability that this text is negative}
#' \item{aws_neutral}{probability that this text is neutral}
#' \item{aws_mixed}{probability that this text is positive}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_transcript_sentiment_aws"

#' Parsed spoken language in a 'Zoom' meeting with syuzhet-based sentiment analysis.
#' 
#' @format A data frame with 30 rows of 23 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{utteranceId}{an incremented numeric identifier for a marked speech utterance}
#' \item{userName}{'Zoom' display name attached to this speaker}
#' \item{utteranceStartSeconds}{when the utterance started as the number of seconds from the start of the recording}
#' \item{utteranceStartTime}{timestamp for the start of the utterance}
#' \item{utteranceEndSeconds}{when the utterance ended as the number of seconds from the start of the recording}
#' \item{utteranceEndTime}{timestamp for the end of the utterance}
#' \item{utteranceTimeWindow}{duration of the utterance, in seconds}
#' \item{utteranceMessage}{the text of the utterance}
#' \item{utteranceLanguage}{language code of the utterance}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' \item{wordCount}{number of words in this utterance}
#' \item{syu_anger}{number of anger words}
#' \item{syu_anticipation}{number of anticipation words}
#' \item{syu_disgust}{number of disgust words}
#' \item{syu_fear}{number of fear words}
#' \item{syu_joy}{number of joy words}
#' \item{syu_sadness}{number of sadness words}
#' \item{syu_surprise}{number of surprise words}
#' \item{syu_trust}{number of trust words}
#' \item{syu_negative}{number of negative words}
#' \item{syu_positive}{number of positive words}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_transcript_sentiment_syu"

#' Parsed chat file in a 'Zoom' meeting
#' 
#' @format A data frame with 30 rows of 9 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{userName}{'Zoom' display name attached to this speaker}
#' \item{messageId}{an incremented numeric identifier for a marked chat message}
#' \item{messageSeconds}{when the message was posted as the number of seconds from the start of the recording}
#' \item{messageTime}{timestamp for message}
#' \item{message}{text of the message}
#' \item{messageLanguage}{language code of the message}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_chat_processed"


#' Parsed chat file in a 'Zoom' meeting with sentiment analysis using AWS
#' 
#' @format A data frame with 10 rows of 14 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{messageId}{an incremented numeric identifier for a marked chat message}
#' \item{userName}{'Zoom' display name attached to the messager}
#' \item{messageSeconds}{when the message was posted as the number of seconds from the start of the recording}
#' \item{messageTime}{timestamp for message}
#' \item{message}{text of the message}
#' \item{messageLanguage}{language code of the message}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' \item{aws_sentClass}{character giving the sentiment classification of this text}
#' \item{aws_positive}{probability that this text is mixed emotion}
#' \item{aws_negative}{probability that this text is negative}
#' \item{aws_neutral}{probability that this text is neutral}
#' \item{aws_mixed}{probability that this text is positive}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_chat_sentiment_aws"


#' Parsed chat file in a 'Zoom' meeting with sentiment analysis using syuzhet
#' 
#' @format A data frame with 30 rows of 30 variables: 
#' \describe{
#' \item{batchMeetingId}{a character meeting identification variable}
#' \item{messageId}{an incremented numeric identifier for a marked chat message}
#' \item{userName}{'Zoom' display name attached to the messager}
#' \item{messageSeconds}{when the message was posted as the number of seconds from the start of the recording}
#' \item{messageTime}{timestamp for message}
#' \item{message}{text of the message}
#' \item{messageLanguage}{language code of the message}
#' \item{userEmail}{character email address}
#' \item{userId}{numeric id of each speaker}
#' \item{wordCount}{number of words in this utterance}
#' \item{syu_anger}{number of anger words}
#' \item{syu_anticipation}{number of anticipation words}
#' \item{syu_disgust}{number of disgust words}
#' \item{syu_fear}{number of fear words}
#' \item{syu_joy}{number of joy words}
#' \item{syu_sadness}{number of sadness words}
#' \item{syu_surprise}{number of surprise words}
#' \item{syu_trust}{number of trust words}
#' \item{syu_negative}{number of negative words}
#' \item{syu_positive}{number of positive words}
#' }
#' @source \url{http://zoomgroupstats.org/}
"sample_chat_sentiment_syu"
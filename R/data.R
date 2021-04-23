#' Parsed spoken language in a Zoom meeting.
#' 
#' @format A data frame with 5 rows of 9 variables: 
#' \describe{
#' \item{utteranceId}{an incremented numeric identifier for a marked speech utterance}
#' \item{utteranceStartSeconds}{when the utterance started as the number of seconds from the start of the recording}
#' \item{utteranceStartTime}{timestamp for the start of the utterance}
#' \item{utteranceEndSeconds}{when the utterance ended as the number of seconds from the start of the recording}
#' \item{utteranceEndTime}{timestamp for the end of the utterance}
#' \item{utteranceTimeWindow}{duration of the utterance, in seconds}
#' \item{userName}{Zoom display name attached to this speaker}
#' \item{utteranceMessage}{the text of the utterance}
#' \item{utteranceLanguage}{language code of the utterance}
#' }
#' @source \url{http://apknight.org/}
"sample_transcript_processed"


#' Parsed spoken language in a Zoom meeting.
#' 
#' @format A data frame with 5 rows of 9 variables: 
#' \describe{
#' \item{utteranceId}{an incremented numeric identifier for a marked speech utterance}
#' \item{utteranceStartSeconds}{when the utterance started as the number of seconds from the start of the recording}
#' \item{utteranceStartTime}{timestamp for the start of the utterance}
#' \item{utteranceEndSeconds}{when the utterance ended as the number of seconds from the start of the recording}
#' \item{utteranceEndTime}{timestamp for the end of the utterance}
#' \item{utteranceTimeWindow}{duration of the utterance, in seconds}
#' \item{userName}{Zoom display name attached to this speaker}
#' \item{utteranceMessage}{the text of the utterance}
#' \item{utteranceLanguage}{language code of the utterance}
#' \item{sentClass}{character giving the sentiment classification of this text}
#' \item{mixed}{probability that this text is mixed emotion}
#' \item{negative}{probability that this text is negative}
#' \item{neutral}{probability that this text is neutral}
#' \item{positive}{probability that this text is positive}
#' }
#' @source \url{http://apknight.org/}
"sample_transcript_sentiment_processed"

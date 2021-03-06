% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batchVideoFaceAnalysis.R
\name{batchVideoFaceAnalysis}
\alias{batchVideoFaceAnalysis}
\title{Batch analyze faces in videos}
\usage{
batchVideoFaceAnalysis(
  batchInfo,
  imageDir,
  sampleWindow,
  facesCollectionID = NA
)
}
\arguments{
\item{batchInfo}{the batchInfo data.frame that is output from batchProcessZoomOutput}

\item{imageDir}{the path to the top-level directory of where all the images are stored}

\item{sampleWindow}{an integer indicating how frequently you have sampled images
in number of seconds.}

\item{facesCollectionID}{name of an 'AWS' collection with identified faces}
}
\value{
data.frame with one record for every face detected in each frame across all meetings. For each face, there is an abundance of information from 'AWS Rekognition'. This output is quite detailed. Note that there will be a varying number of faces per sampled frame in the video. Imagine that you have sampled the meeting and had someone rate each person's face within that sampled moment.
}
\description{
Using this function you can analyze attributes of facial expressions within
a batch of video files. This batch approach requires breaking the videos into
still frames in advance by using the batchGrabVideoStills() function.
}
\examples{
\dontrun{
  vidOut = batchVideoFaceAnalysis(batchInfo=zoomOut$batchInfo, 
  imageDir="~/Documents/meetingImages",
  sampleWindow = 300)
}
}

# zoomGroupStats

<!-- badges: start -->
<!-- badges: end -->

Social science research has been moving into the virtual realm for more than a decade. Beyond the rise of mTurk, Prolific, and other crowd-sourced participant pools, researchers have increasingly relied on web-based modes of data collection. 

The purpose of this package is to enable researchers to efficiently use the virtual meetings platform Zoom to collect data that illuminates how people interact with one another. The focal research areas that have motivated the package so far are group dynamics and interpersonal relations. However, the basic tools in this package could be of use to a broad range of research domains. (They also are helpful for supplementing instructor judgment in assessing class participation in virtual courses...). With this package you will be able to:

1. [quickly turn files downloaded from Zoom into datasets](http://zoomgroupstats.org/articles/part02-process-zoom-files.html)
1. [analyze the dynamics of spoken and text conversations in virtual meetings](http://zoomgroupstats.org/articles/part03-analyze-zoom-conversation-data.html)
1. [extract information from the video feeds of virtual meetings](http://zoomgroupstats.org/articles/part04-analyze-zoom-video-data.html)

This is a project that is actively in development. For the most up-to-date documentation, about `zoomGroupStats`, visit [http://zoomgroupstats.org](http://zoomgroupstats.org).

## Getting Started Conducting Research Using Zoom

For researchers who have deep expertise collecting data within a face-to-face lab or simply using a web browser, research using virtual meetings can be daunting. Yet, when viewed as a constellation of data streams, virtual meetings can be fairly straightforward. To help researchers ramp up efficiently, I am developing a [multi-part guide to aid in collecting research through Zoom](http://zoomgroupstats.org). In addition to detailing how to use `zoomGroupStats`, this guide offers best practices for configuring your Zoom subscription and a delineates a process to use when collecting data across many virtual meetings. 

Like with any research paradigm, investing time upfront to develop your virtual meeting process will pay dividends when it comes to analyzing data. *I highly recommend reviewing [this portion of the guide](http://zoomgroupstats.org/articles/part01-configure-zoom.html) before you begin collecting data.*

## Installation

This package is available through CRAN. It is also a package that I am actively developing. So, you might find value in installing the development version of the package through my github repository. The former will be stable. The latter will be dynamic and (I think) full of exciting new functionality. 

``` r
# Install zoomGroupStats from CRAN
install.packages("zoomGroupStats")

# Or, install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("andrewpknight/zoomGroupStats")
```

Finally, the `zoomGroupStats` package is an outgrowth of a primitive set of functions that I created in April of 2020. If you were using those functions and would like to continue doing so, you can access them using: 

``` r
source("http://apknight.org/zoomGroupStats_deprecated.R")
```

Note, however, that these functions will not be updated nor will they align with the documentation and vignettes that accompany the package version. 

## Examples

The [multi-part guide](http://zoomgroupstats.org) provides extensive detail about how to use this package. To get started quickly, though, here are some sample actions that you might take with this package to analyze your Zoom data. 


### Process information for a batch of meetings

Using a template and set of named Zoom files, you can input and parse information for a batch of Zoom meetings using a single function. This example would further output a *rosetta* file that can be helpful for resolving Zoom display name issues. [Instructions for using the template and naming your Zoom files can be found in this vignette](http://zoomgroupstats.org/articles/part02-process-zoom-files.html). 

``` r
batchOut = batchProcessZoomOutput(batchInput="./myMeetingsBatch.xlsx", exportZoomRosetta="./myMeetings_rosetta_original.xlsx")
```

### Add a unique individual identifier that you have manually adjusted

After you have aligned a unique individual identifier with the Zoom display names provided in the *rosetta* file, you can add this unique identifier to all of the Zoom datasets that you initially created (e.g., transcript, chat, participant info, meeting info). 

``` r
batchOutIds = importZoomRosetta(zoomOutput=batchOut, zoomRosetta="./myEditedRosetta.xlsx", 
meetingId="batchMeetingId")
```

### Conduct sentiment analysis

Measure the sentiment of the different text-based datasets that are in your Zoom output (e.g., chat, transcript). You can request two types of sentiment analysis--a lexicon-based analysis using the open source `syuzhet` package or a machine learning-based analysis using Amazon Web Services. To request the latter, [you must have appropriately configured your AWS credentials](https://github.com/paws-r/paws/blob/main/docs/credentials.md). 

``` r 
# Request sentiment analysis of transcribed audio 
transcriptSent = textSentiment(inputData=batchOutIds$transcript, idVars=c('batchMeetingId', 'utteranceId'), textVar='utteranceMessage', sentMethods=c('aws', 'syuzhet'), appendOut=TRUE, languageCodeVar='utteranceLanguage')
 
# Request sentiment analysis of text-based chat
transcriptSent = textSentiment(inputData=batchOutIds$chat, idVars=c('batchMeetingId', 'messageId'), textVar='message', sentMethods=c('aws', 'syuzhet'), appendOut=TRUE, languageCodeVar='messageLanguage')
```

### Conduct conversation analysis

Measure attributes of the conversation flow--either based on the transcribed audio or the text-based chat. You will receive measurements (e.g., speaking time, number of chat messages) at the level of the individual meeting participant and the meeting as a whole. If you have conducted a sentiment analysis, you can further request analysis of sentiment metrics. 

``` r 
# Request conversation analysis of transcribed audio with aws sentiment analysis completed already
convo.out = textConversationAnalysis(inputData=transcriptSent$aws, inputType='transcript', meetingId='batchMeetingId', speakerId='indivId', sentMethod="aws")
  
# Request conversation analysis of text-based chat without sentiment analysis
convo.out = textConversationAnalysis(inputData=batchOutIds$chat, inputType='chat', meetingId='batchMeetingId', speakerId='indivId')
```

### Conduct a windowed conversation analysis

In addition to measuring attributes of the conversation across an entire virtual meeting, you can analyze conversations in temporal *windows*--subsets of the overall meeting. By using the following function, you can break the virtual meeting into segments and output conversation metrics within each segment.

``` r
win.text.out = windowedTextConversationAnalysis(inputData=transcriptSent$aws, inputType="transcript", meetingId="batchMeetingId", speakerId="indivId", sentMethod="aws", timeVar="utteranceStartSeconds", windowSize=600)
```

### Analyze facial expressions in the video feed

Measure characteristics of the video feeds in a downloaded Zoom video file. This function currently focuses just on facial expressions. It requires either `magick` or `ffmpeg` to process the video file. And, it requires appropriately configured AWS credentials to analyze faces in the video. With these prerequisites, you can detect known individuals in a video and extract facial characteristics (e.g., emotional expressions).

``` r 
# First, break the videos down into image files: 
batchGrabVideoStills(batchInfo=batchOut$batchInfo, imageDir="~/Documents/myMeetings/videoImages", sampleWindow=60)

# Then, analyze the facial expressions, including an identified face collection
vidOut = batchVideoFaceAnalysis(batchInfo=batchOut$batchInfo, imageDir="~/Documents/myMeetings/videoImages", sampleWindow=60, facesCollectionID="group-r")
```
# This is the script for a trial run of the owl detector code for Jordia Sola
# V1 Jan 15 2021


# Load requires 
library(Hmisc)
library(tuneR)
library(seewave)
library(mclust)
library(e1071)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)
library(signal)

# Read in the required function
source('DetectAndClassifyOwl.R')

# Set the location of the sound files
# NOTE: since you are using a PC you might need to change the '/' to '\'
input.dir <-'/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/TestSoundFiles'
input.dir <-'/Users/denasmacbook/Downloads/refilesfordatabase'
# This should include a list of the sound files of interest. If it does not you will need to fix the file path name above!
list.files(input.dir)

# Set the location where you will save the sound files
# NOTE: Within this location there are two folders titled 'GMM' and 'SVM'- make sure your output directory follows this structure
output.dir <- '/Users/denasmacbook/Owl-Detector/test'

# Now we read in the features from the training data
OwlTrainingMFCCdf <- read.csv('OwlTrainingMFCC.csv')

# Check the structure of the data here
head(OwlTrainingMFCCdf)


# Run algorithm for detecting owls
DetectAndClassify.df <- DetectAndClassify(input=input.dir,
                                          audio.moth='FALSE',audio.detect='FALSE',
                                          min.freq = 500, max.freq = 1000,
                                          feature.df= OwlTrainingMFCCdf,
                                          min.signal.dur = 0.4,
                                          max.sound.event.dur = 2,
                                          target.signal = "owl",
                                          output.dir = output.dir,
                                          model.type.list=c("SVM","GMM"),
                                          probability.thresh = 0.5,
                                          noise.quantile.val=0.75,
                                          random.sample='NA',swift.time=FALSE,
                                          write.csv.output=FALSE
)

# Script to create spectrograms for verification
source('plotSoundevents.R')

# Code to create spectrograms from owl detections
pdf('owldetections.pdf') # This line indicates where the pdf of detections will be saved
plotSoundevents(input.dir='/Users/denasmacbook/Owl-Detector/test/GMM',
                nrow=2,ncol=1) # This line indicates the location of the sound files detected above
graphics.off()

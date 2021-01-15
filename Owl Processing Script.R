# R script to process sound files

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

# Set input directory
input.dir <- '/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/TestSoundFiles/'

# Set output directory
output.dir <- '/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/NoiseDetections/'

# Identify sound events for training
DetectOnly(input=input.dir, output.dir=output.dir,
           noise.quantile.val=0.5,
           min.freq = 500, max.freq = 1000,
           min.signal.dur = 0.4,
           max.sound.event.dur = 2,swift.time=FALSE,
           random.sample=FALSE)

# Calculate MFCCs
OwlTrainingMFCC <- calcMFCC(input.dir ='/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/OwlDetections')

call.timing.list.short <- list.files('/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/OwlDetections',pattern='.wav',full.names = F,recursive = T)


class <-  str_split_fixed(call.timing.list.short,pattern='_',
                                  n=2)[,1]

OwlTrainingMFCCdf <- do.call(rbind.data.frame, OwlTrainingMFCC)

OwlTrainingMFCCdf <- cbind.data.frame(class,OwlTrainingMFCCdf)
#write.csv(OwlTrainingMFCCdf,'OwlTrainingMFCC.csv',row.names = F)

# Test
input.dir <-'/Users/denasmacbook/Downloads/refilesfordatabase'

input.dir <-'/Users/denasmacbook/Box/Dena/gibbonRconsulting/Sola Angorra owls/TestSoundFiles'

output.dir <- '/Users/denasmacbook/Owl-Detector/test/'

# Run algorithm for batch processing
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

end_time <- Sys.time()
print(end_time - start_time)


plotSoundevents(input.dir='/Users/denasmacbook/Owl-Detector/test/GMM')
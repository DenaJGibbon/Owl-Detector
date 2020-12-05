DetectOnly <- function(input, feature.df,
                       min.freq = 500, max.freq = 1000,
                              noise.quantile.val=0.75,
                             
                              pattern.split = ".wav", min.signal.dur = 1,
                              max.sound.event.dur = 6, output = "wav",
                             
                              wav.output = "TRUE", output.dir = getwd(),
                             swift.time=TRUE,time.start=18,time.stop=23,
                              write.csv.output=TRUE,verbose=TRUE,
                              random.sample=100) {
  
 
  if ((wav.output == "TRUE" & output.dir == ""))
    stop("Specify output directory")
  
  
  contains.wav <- str_detect(input, '.wav')
  
  if (contains.wav == "FALSE") {
    list.file.input <- list.files(input, full.names = TRUE, recursive = T)
    list.file.input.short <- list.files(input, full.names = FALSE, recursive = T)
  } else {
    list.file.input <- input
  }
  
  if(swift.time==TRUE){
    number.of.slash <- str_count(list.file.input, pattern = "/")[1]
    base.file.name.all <- str_split_fixed(list.file.input, pattern = "/",n=(number.of.slash+1))[,number.of.slash+1]
    temp.name.all <- stringr::str_split_fixed(base.file.name.all, pattern = pattern.split, n = 2)[,1]
    times <- str_split_fixed(temp.name.all,pattern = '_',n=3)[,3]
    times <- as.numeric(substr(times,start=1,stop = 2))
    list.file.input <- list.file.input[which(times > time.start & times < time.stop)]
  }
  
  if (length(list.file.input) == 0 ) {
    print("No sound files detected")
    break
  }
  
  if(is.numeric(random.sample) == TRUE){
    
    list.file.input <-   list.file.input[sample(1:length(list.file.input),random.sample, replace=F)]
  }
  
  
  for( i in 1:length(list.file.input)){
    timing.df <- data.frame()
   
    
    contains.slash <- str_detect(list.file.input[i], pattern = "/")
    
    if(contains.slash=='TRUE'){
      number.of.slash <- str_count(list.file.input[i], pattern = "/")
      base.file.name <- str_split_fixed(list.file.input[i], pattern = "/",n=(number.of.slash+1))[,number.of.slash+1]
      temp.name <- stringr::str_split_fixed(base.file.name, pattern = pattern.split, n = 2)[1]
    } else{
      temp.name <- stringr::str_split_fixed(list.file.input[i], pattern = pattern.split, n = 2)[1]
      
    }
    
    # Convert .wav file to spectrogram
    if(verbose==TRUE){
      print(paste("Computing spectrogram for file", temp.name, i, 'out of', length(list.file.input)))
    }
    
    temp.wav <- readWave(list.file.input[i])
    swift.spectro <- specgram(x=temp.wav@left, n=1600,
                              Fs=temp.wav@samp.rate, overlap = 0)
    
    
    S <- abs(swift.spectro$S[2:(1600*8000/temp.wav@samp.rate),])   # magnitude in range 0<f<=4000 Hz.
    S <- S/max(S)         # normalize magnitude so that max is 0 dB.
    S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
    S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
    
    swift.spectro$S <- S
    
    # Identify the frequency band of interest
    min.freq.cols <- which.min(abs(round(swift.spectro$f, digits = 2) - min.freq))
    max.freq.cols <- which.min(abs(round(swift.spectro$f, digits = 2) - max.freq))
    
    
    # Calculate the column sums for each time window
    col.sum <- colSums(swift.spectro$S[min.freq.cols:max.freq.cols, ])
    
    
    # Calculate noise value
    noise.value <- quantile(unlist(col.sum), c(noise.quantile.val))
    
    # Determine which values are above specified cutoff
    list.sub <- which(col.sum > noise.value)
    call.timing <- split(list.sub, cumsum(c(1, diff(list.sub)) != 1))
    
    # Calculate minimum number of consecutive values above threshold to be considered signal
    number.time.windows.1sec <- min(which(swift.spectro$t > 1))
    signal.dur <- number.time.windows.1sec * min.signal.dur
    
    # Combine all potential sound events into a list
    call.timing.list <- as.list(call.timing[which(sapply(call.timing, length) > signal.dur)])
    
    # If user indicated maximum duration create list of sound events under certain duration
    if(max.sound.event.dur != 'NULL'){
      sound.event.index.max <- which.min(abs(swift.spectro$t - max.sound.event.dur))
      call.timing.list <- call.timing.list[which(sapply(call.timing.list, length) < sound.event.index.max)]
    }
    
    if (length(call.timing.list) >= 1) {
      
      
      subsamps <- lapply(1:length(call.timing.list),
                         function(i) extractWave(temp.wav,
                                                 from=swift.spectro$t[min(call.timing.list[[i]])],
                                                 to=swift.spectro$t[max(call.timing.list[[i]])], xunit = c("time"),plot=F,output="Wave"))
     
      lapply(1:length(subsamps),
                         function(i) writeWave(subsamps[[i]],
                                               filename = paste(output.dir,temp.name,swift.spectro$t[min(call.timing.list[[i]])],
                                                                swift.spectro$t[max(call.timing.list[[i]])],
                                                                '.wav',sep='_')))
      
      
      timing.df <- lapply(1:length(call.timing.list),
                         function(i) cbind.data.frame(swift.spectro$t[min(call.timing.list[[i]])],
                                                 swift.spectro$t[max(call.timing.list[[i]])]))
    
      timing.df <- do.call(rbind.data.frame,timing.df)
     
      colnames(timing.df) <- c('start.time','stop.time')
      file.name <- rep(temp.name,nrow(timing.df))
      timing.df <- cbind.data.frame(timing.df,file.name)
    
    
    timing.df <- rbind.data.frame(timing.df)
    
    if(write.csv.output==TRUE){
      csv.file.name <- paste(output.dir, '/', temp.name,'_timing.df.csv',sep='')
      write.csv(timing.df,csv.file.name,row.names = F)
      print(timing.df)
    }
    
    rm(timing.df)
    rm(swift.spectro)
    rm(subsamps)
    rm(temp.wav)
  }
  }
}




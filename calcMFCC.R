calcMFCC <-
  function(input.dir,  min.freq = 500, max.freq = 1000,
           n.windows = 9, num.cep = 12,pattern.split='_') {
    
    call.timing.list <- list.files(input.dir,pattern='.wav',full.names = T,recursive = T)
    
    call.timing.list.short <- list.files(input.dir,pattern='.wav',full.names = F,recursive = T)
    
    
    names.vector <- str_split_fixed(call.timing.list.short,pattern=
                                      pattern.split,n=2)[,1]
    
    subsamps <- lapply(1:length(call.timing.list),
                       function(i) readWave(call.timing.list[[i]]))
    
    
    
    mfcc.vector.list <- list()
    class <- str_split_fixed(call.timing.list.short,pattern = '_M',n=2)[,2]
    
    if(length(subsamps)==0){
      print('No Sound Files')
      break
    }
    
    for(x in 1:length(subsamps)){
      print(paste("processing sound event", x, 'out of',length(subsamps) ))
      
      short.wav <- subsamps[[x]]
      wav.dur <- duration(short.wav)
      win.time <- wav.dur/n.windows
      
      # Calculate MFCCs
      melfcc.output <- tuneR::melfcc(short.wav, minfreq = min.freq,
                                     hoptime = win.time, maxfreq = max.freq,
                                     numcep = num.cep, wintime = win.time)
      
      # Calculate delta cepstral coefficients
      deltas.output <- deltas(melfcc.output)
      
      # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
      mfcc.vector <- c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)
     
      mfcc.vector.list[[x]] <- mfcc.vector
    }
    
    mfcc.vector.df <- do.call(rbind.data.frame,mfcc.vector.list)
    colnames(mfcc.vector.df) <- seq(1,ncol(mfcc.vector.df),1)
    return( list(mfcc.vector.df))
    
    
  }

#' @title Create multiple spectrograms to inspect sound events
#' @description
#' @export
#'
plotSoundevents <- function(input.dir, nrow = 3, ncol = 2, from = 1, n.soundevents = length(list.wav.files), window.size = 1024, min.freq = 400, max.freq = 2500, class.label = "sound event", label.wav.name = TRUE, 
                            return.table = TRUE) {
  
  list.wav.files <- list.files(input.dir, pattern = c(".wav", ".WAV"), full.names = TRUE)
  list.wav.files.short <- list.files(input.dir, pattern = c(".wav", ".WAV"), full.names = FALSE)
  
  par(mfrow = c(nrow, ncol))
  
  spectro.df <- list()
  for (a in from:n.soundevents) {
    print(paste("processing", a))
    if (label.wav.name == TRUE) {
      class.label <- stringr::str_split_fixed(list.wav.files.short[a], n = 2, pattern = ".wav")[, 1]
    }
    short.wav <- tuneR::readWave(list.wav.files[a])
    
    zcolors = colorRampPalette (c('dark blue','blue','cyan','light green','yellow',
                                  'orange','red', 'brown'))
    dynamicrange = 30
    zrange = c(dynamicrange,0)
    nlevels = abs (zrange[1] - zrange[2]) * 1.2

    levels = pretty(zrange, nlevels)
    zcolors = zcolors(length(levels) - 1)

    Fs <-short.wav@samp.rate
    step <- trunc(5*Fs/1000)             # one spectral slice every 5 ms
    window <- trunc(20*Fs/1000)          # 40 ms data window
    fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
    spg <- specgram(short.wav@left, fftn, Fs, window, window-step)
    S <- abs(spg$S[2:(fftn*3000/Fs),])   # magnitude in range 0<f<=4000 Hz.
    S <- S/max(S)         # normalize magnitude so that max is 0 dB.

    Sdb <- 20*log10(S) # Convert to dB

    Sdb[which(Sdb < (-1 * dynamicrange))] = -1 * dynamicrange

    # png(filename = paste('CodaSpectrograms/',group,'_',temp.wav.updated, c,'coda.png'), width=1000)
    image(t(Sdb),col=zcolors, axes = FALSE,useRaster = TRUE,main =strwrap(paste(class.label, a),width = 15))
    
    sound.event <- paste(class.label, a)
    temp.df <- cbind.data.frame(sound.event, list.wav.files.short[a])
    colnames(temp.df) <- c("sound.event", "file.name")
    spectro.df <- rbind.data.frame(spectro.df, temp.df)
  }
  
  if (return.table == "TRUE") 
    return(list(sound.event.table = spectro.df))
}


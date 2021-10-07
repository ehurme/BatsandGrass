# Vesper file check
library(pacman)
p_load(seewave, tuneR, foreach, doParallel)

hd <- "D:/"
folders <- c("rawdata_batandinsectscreening",
             "scent-experiment")

n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK",
  outfile=""
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

k = 1
for(k in 1:length(folders)){
  folder <- folders[k]
  files <- {}
  wd <- paste0(hd, folder, "/")
  try(files <- list.files(wd, pattern = ".WAV", full.names = TRUE, recursive = TRUE, ignore.case = TRUE))

  # which(files == "F:/rawdata_batandinsectscreening//Hockgraben/nach_Maht/Audiomoth/23.08_Tag1/Mitte/20200823-HGm_203900.WAV")

  # files[2379]
  j = 1
  foreach(j = 1:20) %dopar% {
          # length(files))


    # create empty variable for bat call
    b <- {}

    # import audio file
    b <- tuneR::readWave(files[j]) # , ".wav"))
    # filter out low and high freq noise
    b <- seewave::ffilter(b, from = 3000, to = 90000, bandpass = TRUE, output = "Wave")

    # break up file path into each folder element
    elements <- strsplit(files[j], "/")

    # create path to new file
    output <- paste0(paste(c(hd, unlist(elements)[2:(length(unlist(elements))-1)], "Spectro",
                             unlist(elements)[length(unlist(elements))]), collapse="/"),".jpg")

    # create folder for spectrograms
    if(!dir.exists(paste(c(hd, unlist(elements)[2:(length(unlist(elements))-1)], "Spectro"),
                         collapse = "/"))){ # check if folder already exists
      dir.create(paste(c(hd, unlist(elements)[2:(length(unlist(elements))-1)], "Spectro"),
                       collapse = "/"), ) # if not create folder
    }

    # plot full amplitude spectrogram of the file
    if(!file.exists(output)){ # check if the file already exists
      jpeg(filename = output, width = 1000, height = 600, )
        plot(b, main = unlist(elements)[length(unlist(elements))])
      dev.off()
    }

  }
}

    #
    d <- duration(b)
    fs <- b@samp.rate
    count <- 0
    i = 28
    for(i in 0:(d*2)){
      c <- {}
      try(c <- cutw(b, f = fs, from = i/2, to = i/2+.5))

      new_filename <-  paste0(paste(c(hd, unlist(elements)[2:(length(unlist(elements))-1)], "Spectro",
                                      unlist(elements)[length(unlist(elements))]), collapse="/"),
                              "_", sprintf("%03d", i), ".jpg")

      if(length(c) > 0 & !file.exists(new_filename)){
        jpeg(filename = new_filename,
             width = 1000, height = 600)
        #layout(cbind(1:2))

        spectro(c, osc = TRUE, colbg = "white", ovlp = 85, f = fs,
                main = paste0(unlist(elements)[length(unlist(elements))],
                              "_", sprintf("%03d", i)),
                dB = "A", heights = c(1,1),
                scale = TRUE, flim = c(0, 60),
                collevels=seq(-45,0,0.5), fastdisp = TRUE)
        # plot(b)
        dev.off()
      }
    }
    # c <- {}
    # try(c <- cutw(b, f = 120000, from = floor(d+0.5), to = d))
    #
    # if(length(c) > 0){
    #   # count <- count+1
    #   jpeg(filename = paste0(bat, "_", # sprintf("%06d", count), "_",
    #                          files[j], "_", sprintf("%03d", floor(d*2)+1), "_short.jpg"), width = 1000, height = 600)
    #   #layout(cbind(1:2))
    #
    #   spectro(c, osc = TRUE, colbg = "white", ovlp = 85, f = 120000,
    #           main = paste(bat, files[j], floor(d*2)+1, sep = "_"), dB = "A", heights = c(1,1),
    #           scale = TRUE, flim = c(0, 40), collevels=seq(-45,0,0.5))
    #   # plot(b)
    #   dev.off()
    #
    # }
  }
}
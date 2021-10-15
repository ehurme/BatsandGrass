library(pacman)

p_load(warbleR, seewave, tuneR)
       # install = TRUE,
       # update = TRUE)

hd <- "D:/batsandgrass/"

years <- 2020:2021
l = 2
for(l in 1:length(years)){
  folders <- list.dirs(path = paste0(hd, years[l]), recursive = FALSE)
  k = 1
  for(k in 1:length(folders)){
    folder <- folders[k]
    dirs <- {}
    # wd <- paste0(hd, folder, "/")
    try(dirs <- list.dirs(folder, full.names = TRUE, recursive = TRUE))
    i = 8
    for(i in 1:length(dirs)){
      print(dirs[i])
      print(paste("wav files:", length(list.files(path = dirs[i], pattern = ".wav", ignore.case = TRUE))))
      if(length(list.files(path = dirs[i], pattern = ".wav", ignore.case = TRUE)) > 0){
        print(paste("spectro files:", length(list.files(path = dirs[i], pattern = ".jpeg", ignore.case = TRUE))))
        if(length(list.files(path = dirs[i], pattern = ".jpeg", ignore.case = TRUE)) <=
           length(list.files(path = dirs[i], pattern = ".wav", ignore.case = TRUE))){
          try(full_spectrograms(sxrow = 0.5, rows = 10,
                                pal = seewave::reverse.gray.colors.2, wl = 1024,
                                flim = c(20,80), overwrite = FALSE,
                                fast.spec = TRUE, parallel = 40, it = "jpeg",
                                path = dirs[i]))
        }
      }
    }
  }
}


# ovlp = 10 to speed up function
# tiff image files are better quality and are faster to produce
full_spectrograms(sxrow = 0.5, rows = 10, pal = seewave::reverse.gray.colors.2, wl = 1024,
                  flim = c(20,80), overwrite = FALSE,
                  fast.spec = TRUE, parallel = 40, it = "jpeg",
                  path = "/Users/Edward/Downloads/")

# We can zoom in on the frequency axis by changing flim,
# the number of seconds per row, and number of rows
full_spectrograms(flist = sub, flim = c(2, 10), sxrow = 6, rows = 15, ovlp = 10, it = "tiff")

# Make long spectrograms for the xeno-canto sound files
full_spectrograms(flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg", flist = fl)

# Concatenate full_spectrograms image files into a single PDF per recording
# full_spectrograms images must be jpegs to do this
full_spectrograms2pdf(keep.img = FALSE, overwrite = TRUE)


# Select a subset of sound files
# Reinitialize the wav object
wavs <- list.files(pattern = ".wav$", ignore.case = TRUE)

# Set a seed so we all have the same results
set.seed(1)
sub <- wavs[sample(1:length(wavs), 3)]

# Run auto_detec() on subset of recordings
# The data frame object output is printed to the console, we are not saving this in an object yet, since we are just playing around with argument settings
# you can run this in parallel to speed up computation time
auto_detec(flist = sub, bp = c(1, 10), threshold = 10, mindur = 0.05,
           maxdur = 0.5, envt="abs", ssmooth = 300, ls = TRUE, res = 100,
           flim = c(1, 12), wl = 300, set = TRUE, sxrow = 6, rows = 15, redo = FALSE)

auto_detec(flist = sub, bp = c(2, 10), threshold = 20, mindur = 0.09,
           maxdur = 0.22, envt = "abs", ssmooth = 900, ls = TRUE, res = 100,
           flim= c(1, 12), wl = 300, set =TRUE, sxrow = 6, rows = 15, redo = TRUE,
           it = "tiff", img = TRUE, smadj = "end")

Phae.ad <- auto_detec(bp = c(2, 10), threshold = 20, mindur = 0.09, maxdur = 0.22,
                      envt = "abs", ssmooth = 900, ls = TRUE, res = 100,
                      flim = c(2, 10), wl = 300, set =TRUE, sxrow = 6, rows = 15,
                      redo = TRUE, it = "tiff", img = TRUE, smadj = "end")
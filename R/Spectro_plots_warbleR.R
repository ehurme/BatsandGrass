library(pacman)

p_load(warbleR, install = TRUE, update = TRUE)

hd <- "D:/"
folders <- c("rawdata_batandinsectscreening",
             "scent-experiment")

for(k in 1:length(folders)){
  folder <- folders[k]
  files <- {}
  wd <- paste0(hd, folder, "/")
  try(files <- list.files(wd, pattern = ".WAV", full.names = TRUE, recursive = TRUE, ignore.case = TRUE))

  wavs <- files[1:10]

  # Print this object to see all sound files
  # 6 sound files from xeno-canto
  wavs

# Select a subset of recordings to explore full_spectrograms() arguments
# Based on the list of wav files we created above
sub <- wavs[c(1, 5)]

# How long are these files? this will determine number of pages returned by full_spectrograms
duration_wavs(sub)

# ovlp = 10 to speed up function
# tiff image files are better quality and are faster to produce
full_spectrograms(flist = sub, ovlp = 10, it = "tiff")

# We can zoom in on the frequency axis by changing flim,
# the number of seconds per row, and number of rows
full_spectrograms(flist = sub, flim = c(2, 10), sxrow = 6, rows = 15, ovlp = 10, it = "tiff")

# Make long spectrograms for the xeno-canto sound files
full_spectrograms(flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg", flist = fl)

# Concatenate full_spectrograms image files into a single PDF per recording
# full_spectrograms images must be jpegs to do this
full_spectrograms2pdf(keep.img = FALSE, overwrite = TRUE)
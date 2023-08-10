datapath <- file.path("data-raw", "EP24A2.txt")
ldlroc <- read.table(file = datapath, sep = "\t", header = T)

usethis::use_data(ldlroc, overwrite = TRUE)

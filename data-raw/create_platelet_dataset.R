datapath <- file.path("data-raw", "EP09A3.txt")
platelet <- read.table(file = datapath, sep = "\t", header = T)
platelet$Sample <- paste0("ID", platelet$Sample)

usethis::use_data(platelet, overwrite = TRUE)

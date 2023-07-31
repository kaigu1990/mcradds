datapath <- file.path("data-raw", "EP28A3.txt")
data <- read.table(file = datapath, sep = "\t", header = T)

set.seed(12369)
calcium <- data %>%
  tibble::rowid_to_column() %>%
  dplyr::group_by(rowid) %>%
  dplyr::group_map(~ data.frame(
    Value = rep(.x$Value, .x$Combined),
    Group = c(rep("F", .x$Women), rep("M", .x$Men))
  )) %>%
  purrr::map_dfr(dplyr::bind_rows) %>%
  dplyr::slice_sample(n = 240) %>%
  tibble::rowid_to_column("Sample") %>%
  dplyr::mutate(Sample = paste0("ID", Sample))

usethis::use_data(calcium, overwrite = TRUE)

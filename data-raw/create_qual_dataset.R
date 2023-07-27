set.seed(1567)

d1 <- sample(1:100, 12)
d2 <- sample(101:200, 12)

rand <- rbinom(n = 200, size = 1, prob = 0.7)
qualData <- data.frame(
  Sample = paste0("ID", 1:200),
  ID = 1:200,
  ComparativeN = rand,
  CandidateN = rand
) %>%
  dplyr::mutate(
    CandidateN = as.integer(dplyr::case_when(
      ID %in% d1 ~ abs(CandidateN - 1),
      ID %in% d2 ~ abs(CandidateN - 1),
      .default = CandidateN
    ))
  ) %>%
  dplyr::select(-ID)
usethis::use_data(qualData, overwrite = TRUE)

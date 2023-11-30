adsl <- haven::read_xpt("../../Documents/phuse-scripts/data/adam/cdiscpilot01/adsl.xpt") %>%
  dplyr::select(STUDYID, USUBJID, SUBJID, SITEID, ARM, AGE, AGEGR1, RACE,
                SEX, BMIBL, BMIBLGR1, HEIGHTBL, WEIGHTBL)

set.seed(12345)
adsl_sub <- rbind(
  adsl[adsl$ARM == "Placebo", ] %>% dplyr::sample_n(60),
  adsl[adsl$ARM == "Xanomeline High Dose", ] %>% dplyr::sample_n(60)
) %>%
  dplyr::mutate(
    TRTP = dplyr::case_when(
      ARM == "Xanomeline High Dose" ~ "Xanomeline",
      TRUE ~ ARM
    ),
    TRTPN = dplyr::case_when(
      TRTP == "Xanomeline" ~ 1,
      TRUE ~ 2
    )
  ) %>%
  dplyr::select(STUDYID, USUBJID, SUBJID, SITEID, TRTP, TRTPN, AGE, AGEGR1,
                RACE, SEX, BMIBL, BMIBLGR1, HEIGHTBL, WEIGHTBL) %>%
  dplyr::arrange(SUBJID)

usethis::use_data(adsl_sub, overwrite = TRUE)


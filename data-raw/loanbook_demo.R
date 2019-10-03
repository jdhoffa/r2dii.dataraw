# Source: Googlesheet provided by Klaus:
"https://docs.google.com/spreadsheets/d/
1QHMtoX0j0XMDa33atBPaAYqk__aPQNuIK1v_jsLLd9c/edit#gid=1236522351"

path <- here::here("data-raw/Loanbook - Fake_lbk_SG_based_v4.csv")
loanbook_demo <- readr::read_csv(path)

usethis::use_data(loanbook_demo, overwrite = TRUE)

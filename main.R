library(plumber)

pr("testplumber.R") %>%
  pr_run(port = 8000)

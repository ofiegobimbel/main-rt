library(plumber)
# library(logger)
# library(glue)

# config <- config::get()

# if (!fs::dir_exists(config$log_dir)) {
#   fs::dir_create(config$log_dir)
# }
# log_appender(appender_tee(tempfile("plumber_", config$log_dir, ".log")))

convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}

# pr("testplumber.R") %>%
#   pr_run(port = 8000)

pr <- plumb("testplumber.R")

pr$registerHooks(
  list(
    preroute = function() {
      tictoc::tic()
    },
    postroute = function(req, res) {
      end <- tictoc::toc(quiet = TRUE)
      cat("time:", as.character(Sys.time()), "\t| remote addr:", convert_empty(req$REMOTE_ADDR), "\t| user agent:", convert_empty(req$HTTP_USER_AGENT), "\t| host:", convert_empty(req$HTTP_HOST), "\t| req method:", convert_empty(req$REQUEST_METHOD), "\t| path:", convert_empty(req$PATH_INFO), "\t| status:", convert_empty(res$status), "\t| exec time:", round(end$toc - end$tic, digits = getOption("digits", 5)), "\n")
      # log_info('{convert_empty(req$REMOTE_ADDR)} "{convert_empty(req$HTTP_USER_AGENT)}" {convert_empty(req$HTTP_HOST)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(res$status)} {round(end$toc - end$tic, digits = getOption("digits", 5))}')
    }
  )
)

pr

pr_run(pr, port = 8000)
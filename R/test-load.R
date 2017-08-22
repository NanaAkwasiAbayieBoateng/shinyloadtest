#' Run load tests for a Shiny application
#'
#' @param testFile The file containing a test script to run. Test script can be
#'   generated from \code{recordTest(load_mode = TRUE)}.
#' @param url Web address of the deployed Shiny application.
#' @param numConcurrent Number of concurrent connections to simulate.
#' @param testDurationSec Total length of test. Best practice is to run tests
#'   for > 3x the time of an individual session
#'
#' @details This function simulates load against a deployed Shiny app. The
#'   function creates \code{numConcurrent} parallel processes that each launch a
#'   phantomJS process that browses to \code{url} and drives the app through the
#'   \code{testFile}. Tests should be generated using the \code{recordTest}
#'   function with \code{load_mode = TRUE}. Timing information for each process
#'   is written to a directory and then aggregated and returned.
#'   \code{numConcurrent} connections are maintained for \code{testDurationSec}
#'   seconds.
#'
#' @return Returns a list with two entries, a data frame with timing information
#'   for successful sessions and a numeric entry with the number of failed
#'   sessions.
#'
#' @import     processx
#' @importFrom assertthat assert_that
#'
#' @export
loadTest <- function(testFile = "./tests/myloadtest.R",
                     url = NULL,
                     numConcurrent = 4,
                     testDurationSec = 360
                     ) {

  # Validate Inputs

  assert_that(file.exists(testFile))

  if (!grepl("^http(s?)://", url))
    stop(paste0("URL ", url," does not appear to be for a deployed Shiny app"))

  assert_that(is_count(numConcurrent))
  assert_that(is_count(testDurationSec))


  # storage
  t.update <- 10 # update print every 30 seconds
  tasks <- list()
  active <- vector()
  t.start <- Sys.time()
  t.print <- Sys.time()
  c <- 1



  # keep numConcurrent running through the duration testDurationSec
  while (difftime(Sys.time(), t.start, units = "secs")  < testDurationSec) {

    if (difftime(Sys.time(), t.print, units = "secs") > t.update) {
      print(paste0("Connections Active: ", sum(active)))
      print(paste0("Time Remaining: ", testDurationSec - difftime(Sys.time(),t.start, units = "secs")) )
      t.print <- Sys.time()
    }

    if (sum(active) < numConcurrent) {
      cmd <- paste0("RScript -e \"library(shinyloadtest); options(target.url = '", url, "'); options(connection.id = '", c,"'); source('", testFile, "')\"")
      tasks[[c]] <- processx::process$new(commandline = cmd, stdout = "|")
      active[c] <- 1
      c <- c + 1
    }

    # check for results
    for (i in seq_along(tasks)) {
      if (active[i] > 0) {
        p <- tasks[[i]]
        if (!p$is_alive()) {
          active[i] <- 0
          tasks[[i]] <- new.env() # forces processx to close connection
        }
      }
    }

  }

  # check remaining tasks
  while (sum(active) > 0) {

    for (i in seq_along(tasks)) {
      if (active[i] > 0) {
        p <- tasks[[i]]
        if (!p$is_alive()) {
          active[i] <- 0
          tasks[[i]] <- new.env() # forces processx to close connection
        }
      }
    }
  }

  # shiny load driver writes out a file per connection
  # we just need to read and collapse those files
  dirs <- list.dirs(recursive = FALSE, full.names = FALSE)
  timing_dir <- dirs[grepl(pattern = "-timing", fixed = TRUE, dirs)]
  files <- list.files(timing_dir)
  data <- list()
  for (i in seq_along(files)) {
    data[[i]] <- readRDS(file.path(timing_dir, files[i]))
  }

  successes <- do.call(rbind, data)
  successes$connection <- as.numeric(successes$connection)

  list(successes = successes, errors = c - length(unique(successes$connection)))

}

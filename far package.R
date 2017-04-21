install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd()
create("fars")

#' Loads a CSV file
#' 
#' This function reads in raw data from a csv file
#' 
#' @param A character string providing the name of the csv file to load
#' @details Will return an error message if the file does not exist
#' @return The function returns a tibble (data.frame) from the CSV file loaded
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df 
#' @examples \dontrun{
#' accident_2014 <- fars_read("./data/accident_2014.csv.bz2")
#' accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Creates a new filename
#' 
#' This function makes a string representing a filename based upon the year entered
#' 
#' @param year an integer representing year of the accident data, raw data inputting is perceived as a string
#' @return Returns a string in format 'accident_%d.csv.bz2' with %d represting the imported year
#' @example 
#' make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads the month and year from accient files and generates tables with month and year from data
#' 
#' This function generates tables subtracting month and year from data
#'
#' @details Returns NULL and warning if the file does not exist
#' @param years A vector or list of the years in numeric or integer format
#' @return Returns a list of tibbles (data frames) with the same number or rows as the data in "accident_<year>.csv.bz2' files and two columns-month and years.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @examples \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013,2014))
#' #Results in a warning 
#' fars_read_years(2016)
#' }
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize records per month for a series of years provided 
#' 
#' This function generates a table showing when accidents occur with data for month and year
#' 
#' @param years A vector of integers representing a series of years
#' @return returns a tibble (data.frame) from a series of years (\code{years}) as input and with the number of records per month, by year
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @examples \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(2013:2015)
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map the locations of accients from FARS data
#' 
#' This function generates a map showing input data graphically in a map displyaing the location of accidents for a given state and year
#' 
#' @param state.num an integer giving the index value of a state
#' @param year an integer providing the year of interest
#' @return This function makes a plot that represents locations of accidents
#' @details If there are no matches of \code{state.num} in a data set, it will resut in an error
#' @details \code{\link[maps]{map}} will draw a state map
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr %in%
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples \dontrun{
#' fars_map_state(1,2013)
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
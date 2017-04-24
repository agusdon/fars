#' Loads a CSV file
#'
#' @description
#' The function loads a CSV file defined by \code{filename} argument and returns
#' a tibble. If the path is incorrect the function will end with an error.
#'
#' @param filename Path to the CSV file (character)
#'
#' @return The function returns a tibble (data.frame) based on the CSV file.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a filename
#'
#' @description
#' The function creates a filename for a .csv.bz2 file based on the \code{year}
#' argument in a form "accident_<year>.csv.bz2". It requires a numerical or
#' integer input otherwise ends with an error.
#'
#' @param year Numerical of integer input indicating a year of a data set.
#'
#' @return Returns a character string in a format "accident_<year>.csv.bz2" that
#' can be used as a file name
#'
#' @examples
#' \dontrun{
#' makefilename(2016)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads month and year from accident files
#'
#' @description
#' The function accepts a vector or list of years and returns a list of data
#' frames with MONTH and year columns based on data in "accident_<year>.csv.bz2
#' files. The files need to be located in the working directory.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return Returns a list of tibbles (data frames) with the same number of rows
#' as the data in "accident_<year>.csv.bz2" files and two columns - MONTH and
#' year. Returns NULL and a warning if the file does not exist.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014))
#'
#' # Results in a warning
#' fars_read_years(2016)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~year) %>%
                             dplyr::select_("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Counts number of accidents per month and year
#'
#' Based on the list of years, the function calculates the number of accidents
#' in the US on a monthly basis. The accident files need to be in the working
#' directory, the years can be passed as a list or a vector.
#'
#' @param years A vector or list of years (numeric or integer) that will be
#' searched in the data
#'
#' @return Returns a pivot tibble (data frame) with months in rows and selected
#' years in columns containing the number of accidents. Returns a warning for
#' every input year that does not exist in the datasets. Returns an error (no
#' results) if a different than numeric or integer input is presented.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2016)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_("year", "MONTH") %>%
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_("year", "n")
}

#' Plots the accidents on a US state map
#'
#' The function accepts a state number and year and plots the accidents in a
#' simple map. The state number should be integer or numerical and should exist
#' in the FARS data, otherwise the function terminates with an error. Also
#' returns an error if the data file for the year input does not exist.
#'
#' @param state.num The number of a state in the US as used in the FARS data
#' sets. Should be numeric or integer.
#' @param year The year of analysis (numeric or integer)
#'
#' @return Returns a plot of the accidents based on the \code{state.num} and
#' \code{year} inputs. Returns an error if the state or year do not exist in the
#' data set.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45, 2015)
#'
#' # Results in an error
#' fars_map_state(45, 2016)
#' fars_map_state(60, 2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
        })
}

#' File reading function
#'
#' This is a function that reading files and returne "tbl_df" object
#'
#' @param filename A character string that contain file name
#'
#' @return This function returns a "tbl_df" object
#'
#' @examples
#' \dontrun{fars_read(filename = "accident_2015.csv.bz2")}
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


#' File name making function
#'
#' This is a function that reading files and returns string with complete
#' file name object
#'
#' @param year A integer with year
#'
#' @return This function returns character with accident file name
#'
#' @export
#'
#' @examples
#' \dontrun{make_filename(2014)}
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Return list with data frame objects
#'
#' @param years A vector of the years that you need
#'
#' @return Returns a list with data.frame objects
#' which contain columns MONTH and year
#' @export
#'
#' @examples
#' \dontrun{
#'   years_range <- 2013:2015
#'   result <- fars_read_years(years_range)
#' }
#'
#' @import dplyr
#' @import readr
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



#' Return summaries output by years
#'
#' @param years summarized vector of the years
#'
#' @return "tbl_df" with summary data showing the
#' number of incidents by month and year.
#'
#' @export
#'
#' @examples
#' \dontrun{fars_summarize_years(2015)}
#'
#' @import dplyr
#' @import readr
#' @import tidyr
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Building map of incidents
#'
#' This function draws a map with incidents
#' for the selected state and year
#'
#' @param state.num state number
#' @param year The year for data collecting
#'
#' @return make a map in window
#'
#' @export
#'
#' @examples
#' \dontrun{fars_map_state(4, 2015)}
#'
#' @import dplyr
#' @import readr
#' @import tidyr
#' @import maps
#' @import graphics
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
  })
}

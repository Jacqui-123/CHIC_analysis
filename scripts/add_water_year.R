
#NOTE:
#This function has code from the addWaterYear function from the dataRetrieval library in R
#code was copied from https://rdrr.io/cran/dataRetrieval/src/R/addWaterYear.R
#and altered to allow a user to input the month that they want the water year to begin
#if you don't need to select an alternative month for the water year to begin
#and it can start in october, then you can apply addWaterYear from the dataRetrieval library,
#or else just use: add_water_year(data, month = 10)

add_water_year <- function(rawData, month) {
  allowedDateColNames <- c("dateTime", "Date", "ActivityStartDate", "ActivityEndDate")
  allowedWYColNames <- c("waterYear", "waterYear", "ActivityStartWaterYear", "ActivityEndWaterYear")
  names(allowedWYColNames) <- allowedDateColNames
  # only allow WY to be added if there is an appropriate date column
  if (all(!allowedDateColNames %in% names(rawData))) {
    stop("specified date column does not exist in supplied data frame")
  }

  # set the name of the date column(s) to use for calculating the WY &
  # if the WY column already exists, do not add another (rm that date col
  # from the list that will be looped over)
  dateColNames <- names(rawData)[names(rawData) %in% allowedDateColNames]
  dateColNames <- dateColNames[!allowedWYColNames[dateColNames] %in% names(rawData)]

  for (dateCol in dateColNames) {
    dateColWY <- allowedWYColNames[dateCol]

    # calculate WY & add as new column
    rawData[[dateColWY]] <- calcWaterYear(rawData[[dateCol]], month)

    # move waterYear so that it is always comes right after dateTime
    dateCol_i <- which(names(rawData) == dateCol)
    dateColWY_i <- which(names(rawData) == dateColWY)
    everything_else <- which(!(names(rawData) %in% c(dateCol, dateColWY)))
    everything_else <- everything_else[!everything_else %in% c(1:dateCol_i, dateColWY_i)]

    rawData <- rawData[, c(1:dateCol_i, dateColWY_i, everything_else)]
  }

  return(rawData)
}

#' Extract WY from a date
#'
#' Determine the correct water year based on a calendar date.
#'
#' @param dateVec vector of dates as character ("YYYY-DD-MM"),
#' Date, or POSIXct. Numeric does not work.
#'
#' @details This function calculates a water year based on the USGS
#' definition that a water year starts on October 1 of the year before,
#' and ends on September 30. For example, water year 2015 started on
#' 2014-10-01 and ended on 2015-09-30.
#'
#' @return numeric vector indicating the water year
#' @export
#' @examples
#' x <- seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "month")
#' calcWaterYear(x)
#'
#' y <- c("2010-01-01", "1994-02", "1980", "2009-11-01", NA)
#' calcWaterYear(y)
#'
calcWaterYear <- function(dateVec, month) {
  if (is.numeric(dateVec)) {
    message("dateVec is numeric, with insufficient information to determine water year.")
    return(rep(NA, length(dateVec)))
  }

  dateTimeVec <- tryCatch(
    {
      as.POSIXlt(dateVec)
    },
    error = function(e) {
      date_vec <- tryCatch(
        {
          as.Date(dateVec)
        },
        error = function(e) {
          return(rep(NA, length(dateVec)))
        }
      )


      if (any(is.na(date_vec))) {
        dateVec <- as.character(dateVec)
        dateVec[grep("^(\\d{4}-\\d{2}$)", dateVec)] <- paste0(dateVec[grep("^(\\d{4}-\\d{2}$)", dateVec)], "-01")
        dateVec <- as.Date(dateVec)
      }
      dateTimeVec <- as.POSIXlt(dateVec)
      return(dateTimeVec)
    }
  )

  # POSIXlt years start at 100, POSIXlt months start at 0
  calYear <- dateTimeVec$year + 1900
  calMon <- dateTimeVec$mon + 1

  # when the date is NA, it should not try to add 1
  whichPastMonth <- calMon >= month
  whichPastMonth[is.na(whichPastMonth)] <- FALSE

  # add one to the year if it is in October or after
  waterYear <- calYear
  waterYear[whichPastMonth] <- calYear[whichPastMonth] + 1

  return(waterYear)
}




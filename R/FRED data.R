#' Query FRED data
#'
#' @description This function could use to query the FRED data and return as a
#'    data frame. You must get your personal APIkey from FRED website. Check
#'    FRED website "https://research.stlouisfed.org/docs/api/api_key.html"
#'    to get the APIkey.
#'
#'
#' @param FRED_APIkey Your APIKey within quotation mark
#'
#' @param FRED_series_id The data that you want to query, Id could be found from
#'     the data page right side of name inside of brackets. No Default.
#'
#' @param FRED_observation_start Data start time, format is "YYYY-MM-DD".
#'     Default is "1970-01-01"
#'
#' @param FRED_observation_end Data end time, format is "YYYY-MM-DD".
#'     Default is "2020-12-31"
#'
#' @return
#' @export
#'
#' @examples FRED_DATA("Your_FRED_APIkey","Data_Query","1970-01-01","2020-12-31")
FRED_DATA <- function(FRED_APIkey,
                      FRED_series_id,
                      FRED_observation_start <- "1970-01-01",
                      FRED_observation_end <- "2020-12-31"){

  FRED_URL <- "https://api.stlouisfed.org/fred/series/observations"

  FRED_Parameters <- paste("?api_key=",FRED_APIkey,
                           "&file_type=json",
                           "&series_id=",FRED_series_id,
                           "&observation_start=",FRED_observation_start,
                           "&observation_end=",FRED_observation_end,
                           sep = "")

  Path <- paste0(FRED_URL,FRED_Parameters,sep = "")

  first_query <- fromJSON(Path)

  FRED_query <- first_query$observations

  rownames(FRED_query) <- FRED_query$date

  FRED_query <- FRED_query["value"]

  FRED_query$value <- as.numeric(FRED_query$value)

  return(FRED_query)
}

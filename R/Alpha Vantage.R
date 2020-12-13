#' Query Stock data
#'
#' @description This function could use to query the Alpha Vantage stock data
#'    and return as a data frame. You must get your personal APIkey from FRED website.
#'    Check Alpha Vantage website "https://www.alphavantage.co/support/#api-key"
#'    to get the APIkey.
#'
#' @param Alpha_Vantage_symbol The data that you want to query, Id could be found from
#'     the website "https://www.alphavantage.co/documentation/" . No Default.
#'
#' @param Alpha_Vantage_outputsize  "compact" or "full"
#'     compact returns only the latest 100 data points in the intraday time series;
#'     full returns the full-length intraday time series.
#'     The "compact" option is recommended if you would like to reduce the data size of each API call.
#'
#' @param Alpha_Vantage_APIkey Your APIKey within quotation mark
#'
#' @return
#' @export
#'
#' @examples Recent_stock_price("IBM","full","Your_APIkey")
Recent_stock_price <- function(Alpha_Vantage_symbol,Alpha_Vantage_outputsize,Alpha_Vantage_APIkey){

  Alpha_Vantage_URL <- "https://www.alphavantage.co/query"


  Alpha_Vantage_Parameters <- paste("?function=TIME_SERIES_INTRADAY",
                                    "&symbol=",Alpha_Vantage_symbol,
                                    "&interval=1min",
                                    "&outputsize=",Alpha_Vantage_outputsize,
                                    "&apikey=",Alpha_Vantage_APIkey,
                                    sep = "")

  Alpha_Vantage_Path <- paste(Alpha_Vantage_URL,Alpha_Vantage_Parameters,sep = "")

  Alpha_Vantage_query <- fromJSON(Alpha_Vantage_Path)

  stock_data <- Alpha_Vantage_query$`Time Series (1min)`

  stock_data_df <- as.data.frame(do.call(rbind, stock_data))

  stock_data_df_edit <- as.data.frame(lapply(stock_data_df, unlist))

  stock_data_df_edit <- as.data.frame(lapply(stock_data_df_edit, as.numeric))

  rownames(stock_data_df_edit) <- rownames(stock_data_df)

  return(stock_data_df_edit)

}

#' Mean Absolute Percentage Error (MAPE)
#'
#' @description Measures absolute error as a percentage of the forecast.
#'     Function don't have default value, setting by yourself.
#'
#' @param Real This is a numeric vector, it must be the smae lenght with
#'     Forecast vector.
#'
#' @param Forecast  This is a numeric vector, it must be the smae lenght with
#'     Real vector.
#'
#' @return
#' @export
#'
#' @examples Mean_Absolute_Percentage_Error(Numeric_vector1,Numeric_vector2)
Mean_Absolute_Percentage_Error <- function (Real,Forecast) {
  absPecent <- c()
  for (i in 1:length(Real)){
    absPecent[i] = abs((Real[i] - Forecast[i])/Real[i])
  }
  tot = sum(absPecent)
  MAPE = tot / (100*length (tot))
  print(MAPE)
}

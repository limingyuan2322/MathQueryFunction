#' Mean Absolute Deviation (MAD)
#' @description Measures average absolute deviation of forecast from actuals.
#'     Function don't have default value, setting by yourself.
#'
#' @param Real This is a numeric vector, it must be the smae lenght with
#'     Forecast vector.
#'
#' @param Forecast This is a numeric vector, it must be the smae lenght with
#'     Real vector.
#'
#' @return
#' @export
#'
#' @examples Mean_Absolute_Deviation(Numeric_vector1,Numeric_vector2)
Mean_Absolute_Deviation <- function (Real,Forecast) {
  absdiff <- c()
  for (i in 1:length(Real)){
    absdiff[i] = abs(Real[i] - Forecast[i])
  }
  tot = sum(absdiff)
  MAD = tot / length (tot)
  print(MAD)
}

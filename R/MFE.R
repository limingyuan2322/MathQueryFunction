#' Mean Forecast Error (MFE)
#' @description Measures average deviation of forecast from actual.
#'     Function don't have default value, setting by yourself.
#'
#' @param Real This is a numeric vector, it must be the smae lenght with
#'     Forecast vector.
#' @param Forecast This is a numeric vector, it must be the smae lenght with
#'     Real vector.
#'
#' @return
#' @export
#'
#' @examples
#' Mean_Forecast_Error(Numeric_vector1,Numeric_vector2)
Mean_Forecast_Error <- function (Real,Forecast) {
  diff = c()
  for (i in 1:length(Real)){
    diff[i] = Real[i] - Forecast[i]
  }
  tot = sum(diff)
  MFE = tot / length (tot)
  print(MFE)
}

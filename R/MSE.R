#' Standard Squared Error (MSE)
#'
#' @describeIn Measures variance of forecast error.
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
#' @examples Standard_Squared_Error(Numeric_vector1,Numeric_vector2)
Standard_Squared_Error <- function (Real,Forecast) {
  diffsqr <- c()
  for (i in 1:length(Real)){
    diffsqr[i] = ((Real[i] - Forecast[i])^2)
  }
  tot = sum(diffsqr)
  MSE = tot / (100*length (tot))
  print(MSE)
}

#' Assign fiscal quarter to a date
#'
#' Returns the fiscal quarter for the given date. This function is a modified version of lubridate::quarter.
#' The original function had an issue and the solution was copied from \url{https://github.com/tidyverse/lubridate/issues/682}
#'
#' @param x Date for which the fiscal quarter is desired
#' @param fiscal_start The month in which the fiscal year begins e.g 7 for Australia
#' @return numeric vector with the fiscal year and the quarter
#'
#' @family data time functions
#' @seealso \url{https://github.com/tidyverse/lubridate/issues/682}  for the original issue
#' @export
#' @examples
#' fiscal_quarter(Sys.Date())



fiscal_quarter <- function(x, fiscal_start = 1) {
    fs <- fiscal_start - 1
    shifted <- seq(fs, 11 + fs) %% 12 + 1
    m <- lubridate::month(x)
    quarters <- rep(1:4, each = 3)
    s <- match(m, shifted)
    q <- quarters[s]
    #if (with_century) {
    #    uq <- quarters[m]
        inc_year <- (m >= fiscal_start) * (fiscal_start != 1)
        lubridate::year(x) + inc_year + q/10
    #}
    #else q
}





# fiscal_year <- function(x, with_century = FALSE, fiscal_start = 1) {
#     fs <- fiscal_start - 1
#     shifted <- seq(fs, 11 + fs) %% 12 + 1
#     m <- lubridate::month(x)
#     quarters <- rep(1:4, each = 3)
#     s <- match(m, shifted)
#     q <- quarters[s]
#     if (with_century) {
#         uq <- quarters[m]
#         inc_year <- (m >= fiscal_start) * (fiscal_start != 1)
#         lubridate::year(x) + inc_year + q/10
#     }
#     else q
# }
#
#


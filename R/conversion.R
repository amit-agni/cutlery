#' @title Bulk type conversion of columns to factors
#'
#' @description
#' Converts multiple columns of same type to factor
#' Possible enhancements : Specify the columns to be converted
#'
#' @param x data.frame (or data.table)
#' @param typeToConvert type of the columns to be converted
#'
#' @return data.table
#'
#' @author Amit Agni
#' @family type conversion functions
#' @seealso
#' @export
#' @examples
#' DT <- data.frame(c1=c("a","b","c")
#' ,c11 = c("a","b","c")
#' ,c2 = 1:3
#' ,c3 = c(1.1,1.2,1.3)
#' ,stringsAsFactors = F)
#' DT <- DTconvert2factor(DT,"int")

DTconvert2factor <- function(x,typeToConvert ="chr") {
  data.table::setDT(x)
  fn <- dplyr::case_when(typeToConvert == "chr" ~ "is.character"
                  ,typeToConvert == "num" ~ "is.numeric"
                  ,typeToConvert == "int" ~ "is.integer")
  cols<- names(Filter(fn, head(x)))
  #Alternative : names(which(lapply(DT,fn)==T))
  x[, (cols) := lapply(.SD, as.factor),.SDcols = cols]
  return(x)

}


#' @title replace NaN and Inf with NA
#'
#' @description
#' Takes out \code{NaN} and \code{Inf} and replaces them with \code{NA}
#'
#' @param x vector
#'
#' @return Returns vector with with replaced \code{NA}values.
#' @export
#'
#' @examples
#' test <- list(a = c("a", "b", NA),
#'              b = c(NaN, 1,2, -Inf),
#'              c = c(TRUE, FALSE, NA))
#'
#' lapply(test, to_na)
#'
#' ## Output
#' # $a
#' # [1] "a" "b" NA
#'
#' # $b
#' # [1] NA  1  2 NA
#'
#' # $c
#' # [1] TRUE FALSE NA
#'
#' @author Daniel Luettgau
#' @note --- Idea for improvement
#'
#' Add args to flexible select which scenarios should be set NA
#'
#'   - nan, infinite, other defined values
#'
to_na <- function(x) {
  # check input
  if (!is.vector(x)) {
    stop("input must be a vector")
  }

  # convert input
  if (is.character(x)) {
    return(x)
  } else {
    ifelse(is.infinite(x) | is.nan(x),  NA, x)
  }
}



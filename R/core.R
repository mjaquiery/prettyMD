# Presentation functions --------------------------------------------------

#' Format number to be a string beginning with a decimal point.
#' @param num number to convert to string
#' @param precision decimal places to preserve
#' @param isProportion whether to strip leading 0 for 0.x values
#' @param truncateZeros whether to strip trailing 0s
#' @param ... discarded arguments used to allow overflowed calls from other functions
#' @return \code{num} stripped of leading 0s and rounded to \code{precision} decimal places
#'
#' @examples
#' data.frame(input = c(seq(-10,10),100), output = num2str(exp(c(seq(-10,10),100)), 4))
#' data.frame(input = c(seq(-10,10),100), output = num2str(exp(c(seq(-10,10),100)), 4, isProportion = T))
#'
#' @export
num2str <- function(num, precision = 2, isProportion = F, truncateZeros = F, ...) {
  if(length(num) > 1)
    return(sapply(num, function(x) num2str(num = x,
                                           precision = precision,
                                           isProportion = isProportion,
                                           truncateZeros = truncateZeros)))
  if(is.nan(num) | is.na(num))
    return(as.character(num))
  num <- round(num, precision)
  # if we hit scientific notation then give up!
  if(grepl('e', num, fixed = T))
    return(as.character(num))
  # leading 0 stripping
  if(abs(num) < 1 & isProportion)
    if(num == 0)
      x <- '.'
  else
    x <- sub('^-?0\\.', ifelse(num < 0, '-.', '.'), as.character(num))
  else
    x <- as.character(num)
  if(truncateZeros)
    return(x)
  # string manipulation to pad 0s
  dot <- regexpr('.', x, fixed = T)
  if(dot == -1) {
    x <- paste0(x,'.')
    dot <- regexpr('.', x, fixed = T)
  }
  right <- substr(x, dot, dot+precision) # portion of x after 0
  right <- paste0(right, strrep('0',precision-nchar(right)+1))
  x <- paste0(substr(x, 1, dot-1), right)
  return(x)
}

#' Wrapper for \code{num2str(..., isProportion = T)}
#' @inheritDotParams num2str
#'
#'@examples
#' data.frame(input = c(seq(-10,10),100),
#'   num = num2str(exp(c(seq(-10,10),100)), 4),
#'   prop = prop2str(exp(c(seq(-10,10),100)), 4)
#' )
#'
#' @export
prop2str <- function(...) {
  return(num2str(..., isProportion = T))
}

#' Print the mean and CIs of a vector
#' @param vector data in
#' @param label markdown prefix for the stats
#' @param conf.int width of the confidence intervals, NA to suppress
#' @param na.rm whether NA values are removed before averaging
#' @param decimals decimal places to round to
#' @param isProportion whether to print the values as proportions (strip leading 0)
#' @param showRange whether to include the range of the data
#' @return string representation of the mean, CIs, and range of the \code{vector}
#'
#' @examples
#' md.mean(rnorm(1000), label = '*M*|random')
#' md.mean(rnorm(1000), label = '*M*|random', conf.int = NA) # no longer needs lsr package
#' md.mean(runif(1000), label = 'Mean probability', isProportion = T, showRange = T)
#'
#' @export
md.mean <- function(vector, label = '*M*', decimals = 2, na.rm = F, conf.int = .95, isProportion = F, showRange = F) {
  mu <- mean(vector, na.rm = na.rm)
  out <- paste0(label,' = ', num2str(mu,decimals, isProportion = isProportion))

  if(!is.na(conf.int)) {
     if (!requireNamespace("lsr", quietly = TRUE)) {
      stop("Package \"lsr\" needed to calculate confidence intervals. Please install it or use md.mean(..., conf.int = NA).",
           call. = FALSE)
    }
    ci <- lsr::ciMean(vector, conf = conf.int, na.rm = na.rm)
    ci.low <- ci[1]
    ci.high <- ci[2]
    out <- paste0(out, ' [', num2str(ci.low,decimals), ', ', num2str(ci.high,decimals),']')
  }
  if(showRange) {
    r <- num2str(range(vector, na.rm = na.rm), decimals, isProportion = isProportion)
    out <- paste0(out, ' range = (',
                  r[[1]], ', ', r[[2]], ')')
  }

  return(out)
}

#' Extract the Bayes Factor from a Bayesian test
#' @param bayesTest the test whose parameter will be extracted
#' @inheritDotParams num2str
#'
#' @examples
#' \dontrun{
#'
#' data <- data.frame(x = rnorm(100), y = rnorm(100, 0.2)) # two normal distributions with some overlap
#' md.BF(BayesFactor::ttestBF(data$x, data$y, paired = T))
#' }
#'
#' @export
md.BF <- function(bayesTest, ...) {
  out <- paste(name, '=', num2str(exp(bayesTest@bayesFactor$bf)), ...)
  return(out)
}

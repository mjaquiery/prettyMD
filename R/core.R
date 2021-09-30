# Presentation functions --------------------------------------------------

#' Format number to be a string beginning with a decimal point.
#' @param num number to convert to string
#' @param precision decimal places to preserve
#' @param isProportion whether to strip leading 0 for 0.x values
#' @param truncateZeros whether to strip trailing 0s
#' @param prefix prefix for the number string (e.g. " = ")
#' @param minPrefix for transforming numbers like .000 into < .001
#' @param ... discarded arguments used to allow overflowed calls from other functions
#' @return \code{num} stripped of leading 0s and rounded to \code{precision} decimal places
#'
#' @importFrom stringr str_detect str_replace
#'
#' @examples
#' data.frame(input = c(seq(-10,10),100), output = num2str(exp(c(seq(-10,10),100)), 4))
#' data.frame(input = c(seq(-10,10),100), output = num2str(exp(c(seq(-10,10),100)), 4, isProportion = TRUE))
#'
#' @export
num2str <- function(
  num,
  precision = 2,
  isProportion = F,
  truncateZeros = F,
  prefix = "",
  minPrefix = NA,
  ...
) {
  scipen <- options(scipen = 999)
  if (length(grep("tibble", sessionInfo())))
    if (tibble::is_tibble(num))
      return(num2str.tibble(
        num,
        precision = precision,
        isProportion = isProportion,
        truncateZeros = truncateZeros,
        prefix = prefix,
        minPrefix = minPrefix,
        ...
      ))
  if (length(num) > 1)
    return(sapply(num, function(x) num2str(
      num = x,
      precision = precision,
      isProportion = isProportion,
      truncateZeros = truncateZeros,
      prefix = prefix,
      minPrefix = minPrefix,
      ...
    )))
  if (!is.numeric(num) | is.nan(num) | is.na(num))
    return(as.character(num))
  num <- round(num, precision)
  # if we hit scientific notation then give up!
  if (grepl('e', num, fixed = T))
    return(as.character(num))
  # leading 0 stripping
  if (abs(num) < 1 & isProportion)
    if (num == 0)
      x <- '.'
  else
    x <- sub('^-?0\\.', ifelse(num < 0, '-.', '.'), as.character(num))
  else
    x <- as.character(num)
  if (truncateZeros)
    return(ifelse(x == "0.", "0", x))
  # string manipulation to pad 0s
  dot <- regexpr('.', x, fixed = T)
  if (dot == -1) {
    x <- paste0(x,'.')
    dot <- regexpr('.', x, fixed = T)
  }
  right <- substr(x, dot, dot + precision) # portion of x after 0
  right <- paste0(right, strrep('0',precision - nchar(right) + 1))
  x <- paste0(substr(x, 1, dot - 1), right)

  # Adding < .001 notation if required
  if (!is.na(minPrefix))
    x <- ifelse(
      str_detect(x, '^0?\\.?0*$'),
      paste0(minPrefix, str_replace(x, '0$', '1')),
      paste0(prefix, x)
    )
  else
    x <- paste0(prefix, x)

  options(scipen = scipen)
  return(x)
}

#' Wrapper for \code{num2str(..., isProportion = T)}
#' @param x number to stringify
#' @param precision number of decimal places to preserve
#' @param minPrefix prefix for numbers that would render as .0
#' @inheritDotParams num2str
#'
#'@examples
#' data.frame(input = c(seq(-10,10),100),
#'   num = num2str(exp(c(seq(-10,10),100)), 4),
#'   prop = prop2str(exp(c(seq(-10,10),100)), 4)
#' )
#'
#' @export
prop2str <- function(x, precision = 3, ...) {
  return(num2str(x, precision, isProportion = T, ...))
}

#' Wrapper for \code{num2str(..., isProportion = T, prefix = " = ", minPrefix = " < ")}
#' Designed for reporting p-values
#' @param x number to convert to string
#' @param precision decimal places to preserve
#' @param isProportion whether to strip leading 0 for 0.x values
#' @param prefix prefix for the number string (e.g. " = ")
#' @param minPrefix for transforming numbers like .000 into < .001
#' @inheritDotParams num2str
#'
#'@examples
#' data.frame(input = c(seq(-10,10),100),
#'   num = num2str(exp(c(seq(-10,10),100)), 4),
#'   p = p2str(exp(c(seq(-10,10),100)), 4)
#' )
#'
#' @return \code{x} stripped of leading 0s and rounded to \code{precision} decimal places
#'
#' @export
p2str <- function(x, precision = 3, prefix = " = ", minPrefix = " < ", ...) {
  return(
    num2str(
      x,
      precision,
      prefix = prefix,
      minPrefix = minPrefix,
      isProportion = T,
      ...
    )
  )
}

#' Format Bayes factors in a nice way using scientific notation where necessary
#' @param bf Bayes factor to represent
#' @param width number of numbers to show
#'
#' @examples
#'  bfs <- c(1/2342356, 1/23424, 1/2343, 1/234, 1/2, 2, 56, 872, 99887, 2342e7)
#'  bf2str(bfs)
#'
#'  # Drop trailing decimal points
#'  bf2str(c(3.2, 30.2, 300, 300.45, 3000.25))
#'
#' @export
bf2str <- function(bf, width = 3) {
  prefix = ifelse(bf < 1, '1/', '')
  x <- ifelse(bf < 1, 1/bf, bf)
  nub <- x

  while (any(nub >= 10)) {
    nub <- ifelse(nub >= 10, nub / 10, nub)
  }

  nub.c <- ifelse(grepl('\\.', as.character(nub)), nub, paste0(nub, '.'))
  nub.c <- paste0(nub.c, strrep(0, width))

  e <- log(x / nub, base = 10)

  eLen <- nchar(as.character(e))

  x.c <- ifelse(
    e < width,
    substr(x, 1, width + 1),
    substr(nub.c, 1, ifelse(width - (eLen - 1) == 2, 3, width - (eLen - 1)))
  )
  x.c <- ifelse(
    grepl('\\.$', x.c),
    substr(x.c, 1, nchar(x.c) - 1),
    x.c
  )

  e.c <- ifelse(
    e < width,
    '',
    paste0('e', e)
  )

  paste0(prefix, x.c, e.c)
}

#' Format entries in a tibble using num2str
#' @param tbl tibble to execute num2str
#' @param precision decimal places to preserve
#' @param isProportion whether to strip leading 0 for 0.x values
#' @param truncateZeros whether to strip trailing 0s
#' @param minPrefix for transforming numbers like .000 into < .001
#' @inheritDotParams num2str
#'
#' @description
#' Parameters for \code{num2str} can be specified as vectors indicating the
#' columns to which values apply, with NA interpreted as using the default value
#'
#' @examples
#' library(tibble)
#' x <- tibble(chr = "character", int = 1:10, prop = runif(10), tProp = runif(10))
#' num2str.tibble(x, isProportion = c(NA, NA, T, T), truncateZeros = c(F, F, F, T))
#'
#' @export
num2str.tibble <- function(tbl,
                           precision = 2,
                           isProportion = F,
                           truncateZeros = F,
                           prefix = "",
                           minPrefix = NA,
                           ...) {
  if (length(precision) == 1)
    precision <- rep(precision, ncol(tbl))
  if (length(isProportion) == 1)
    isProportion <- rep(isProportion, ncol(tbl))
  if (length(truncateZeros) == 1)
    truncateZeros <- rep(truncateZeros, ncol(tbl))

  precision[is.na(precision)] <- 2
  isProportion[is.na(isProportion)] <- F
  truncateZeros[is.na(truncateZeros)] <- F

  for (i in 1:ncol(tbl)) {
    tbl[, i] <- num2str(tbl[[i]],
                        precision = precision[i],
                        isProportion = isProportion[i],
                        truncateZeros = truncateZeros[i],
                        prefix = prefix,
                        minPrefix = minPrefix)
  }

  tibble::as_tibble(tbl)
}

#' Format s with = if it's not 0, or < otherwise
#' @param s string to format
#' @param non_equal_char character to use if x != 0
#' @param sep separator between the relationship character and s
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' lteq('.0001')
#' lteq('.000')
#'
#' p <- t.test(rnorm(100, 1))$p.value
#' p
#' lteq(p)
#' # string format first!
#' lteq(prop2str(p, precision = 3))
#'
#' @export
lteq <- function(s, non_equal_char = '<', sep = ' ') {
  x <- ifelse(str_detect(s, '[^\\.0]'), '=', non_equal_char)

  paste0(x, sep, str_replace(s, '0$', '1'))
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
#' md.mean(runif(1000), label = 'Mean probability', isProportion = TRUE, showRange = TRUE)
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
#' # two normal distributions with some overlap
#' data <- data.frame(x = rnorm(100), y = rnorm(100, 0.2))
#' md.BF(BayesFactor::ttestBF(data$x, data$y, paired = TRUE))
#' }
#'
#' @export
md.BF <- function(bayesTest, ...) {
  out <- paste('BF =', bf2str(exp(bayesTest@bayesFactor$bf)), ...)
  return(out)
}

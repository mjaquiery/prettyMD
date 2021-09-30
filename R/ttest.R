
# T-test functions --------------------------------------------------------

#' Appropriately formatted t-test output
#' @param result t.test output to format
#' @param decimals number of decimal places for means
#' @param decimals.p number of decimal places for p-values, defaults to
#'   \code{decimals}+1
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100, 0.2)) # two normal distributions with some overlap
#' md.t(t.test(data$x, data$y, paired = TRUE), decimals = 3)
#'
#' @export
md.t <- function(result, decimals = 2, decimals.p = NULL) {
  if (is.null(decimals.p))
    decimals.p <- decimals + 1
  df <- num2str(result$parameter, decimals, truncateZeros = T)
  stat <- num2str(result$statistic, decimals)
  p <- ifelse(result$p.value < 10^-decimals.p,
              paste0('< .', strrep('0', decimals.p - 1), '1'),
              paste0(p2str(result$p.value, precision = decimals.p)))
  out <- paste0('_t_(', df, ') = ', stat,
                ', _p_ ', p)
  return(out)
}

#' Run a t-test and return neatly formatted output \code{x}, and either \code{y}
#' or \code{mu} must be specified.
#' @param x vector for testing
#' @param y additional vector for testing. If singular, functions as \code{mu}
#' @param labels vector of labels for x and y. If NULL, use attr(., 'label'), NA
#'   to suppress this behaviour.
#' @param mu value against which x will be tested
#' @param paired whether observations are paired (e.g. repeated measures)
#' @inheritDotParams md.mean
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100, 0.2)) # two normal distributions with some overlap
#' tt <- md.ttest(data$x, data$y, c('Mean~Control~', 'Mean~Treatment~'), paired = TRUE)
#' cat(tt)
#'
#' @return formatted string like t(df) = 3.68, p < .05, d = 0.23, BF = 4.50; M1
#'   = 3.05 [1.23, 4.55], M2 = 5.12 [4.20, 5.99]
#'
#' @export
md.ttest <- function(x, y = NULL, labels = NULL, mu = NULL, paired = F, ...) {
  if (is.null(labels)) {
    if (!is.null(attr(x, 'label')))
      labels <- attr(x, 'label')
    if (!all(is.null(y)) && !is.null(attr(y, 'label')))
      labels <- c(labels, attr(y, 'label'))
  }
  if (!all(is.null(labels)) && all(is.na(labels)))
      labels <- NULL

  # coerce to numeric vectors and drop labels
  x <- as.numeric(x); y <- as.numeric(y)
  pkgs <- list()
  for (pkg in c('lsr', 'BayesFactor'))
    pkgs[[pkg]] <- requireNamespace(pkg, quietly = T)

  oneSided <- (length(y) == 1 | !is.null(mu))
  if (is.null(labels)) {
    if (oneSided)
      labels <- '*M*'
    else
      labels <- c('*M1*', '*M2*')
  }
  # support for one-sample tests
  if (oneSided) {
    y = ifelse(is.null(mu), y, mu)
    .t <- md.t(t.test(x, mu = y, paired = paired))
    .d <- ifelse(pkgs[['lsr']],
                 paste(', _d_ =', num2str(lsr::cohensD(x, mu = y), ...)),
                 NULL)
    .bf <- ifelse(pkgs[['BayesFactor']],
                  paste(',', md.BF(BayesFactor::ttestBF(x, mu = y, paired = paired))),
                  NULL)
    means <- paste0('; ', md.mean(x, label = labels[1], ...), ', $\\mu$ = ', y)
  } else {
    .t <- md.t(t.test(x, y, paired = paired))
    .d <- ifelse(pkgs[['lsr']],
                 paste(', *d* =', num2str(lsr::cohensD(x, y), ...)),
                 NULL)
    .bf <- ifelse(pkgs[['BayesFactor']],
                  paste(',', md.BF(BayesFactor::ttestBF(x, y, paired = paired))),
                  NULL)
    means <- paste0('; ', md.mean(x, label = labels[1], ...), ', ',
                    md.mean(y, label = labels[2], ...))
  }
  out <- paste0(.t, .d, .bf, means)
}

#' Return only the BayesFactor and means part of a t-test
#' @inheritDotParams md.ttest
#' @export
md.ttestBF <- function(...) {
  out <- md.ttest(...)
  # string slicing
  out <- sub('^.*, BF~H1:H0~ = ', 'BF~H1:H0~ = ', out)
  return(out)
}

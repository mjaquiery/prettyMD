# Table outputs -----------------------------------------------------------

#' Format ANOVA results as a neat kableExtra printed table. Designed with
#' ezANOVA in mind.
#' @param ANOVA ANOVA results to format
#' @param ... passed on to \code{kableExtra::kable}
#'
#' @return formatted version of ANOVA for inclusion in a report
#'
#' @examples
#' \dontrun{
#' ezOutput <- ez::ezANOVA(...)
#' kableANOVA(ezOutput$ANOVA, caption = "Table of ANOVA results")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @import kableExtra
#' @export
kableANOVA <- function(ANOVA, ...) {

  tmp <- ANOVA %>%
    mutate(`F` = num2str(`F`),
           p = num2str(p, precision = 3, isProportion = T),
           `p<.05` = if_else(`p<.05` == "", "", "$\\text{*}$"),
           ges = num2str(ges, precision = 3, isProportion = T))
  rownames(tmp) <- NULL

  # add degrees of freedom to F
  names(tmp)[names(tmp) == 'F'] <-
    paste0('$F$(', ANOVA$DFn[1], ', ', ANOVA$DFd[1], ')')

  tmp %>%
    select(Effect,
           starts_with('$F'),
           `$p$` = p,
           ` ` = `p<.05`,
           `$\\eta^2$` = ges) %>%
    kable(align = c('l', 'r', 'r', 'c', 'r'),
          escape = F,
          ...) %>%
    kable_styling() %>%
    column_spec(1, bold = T) %>%
    footnote(
      general_title = "",
      general = paste0("Degrees of freedom: ", tmp$DFn, ", ", tmp$DFd)[1]
    )
}

#' Format LM results as a neat kableExtra printed table.
#'
#' @param LM lm results to format
#' @param ... passed on to \code{kableExtra::kable}
#'
#' @return formatted version of LM for inclusion in a report
#'
#' @examples
#' r <- lm(mpg ~ cyl * wt, data = mtcars)
#' kableLM(r)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select as_tibble
#' @import kableExtra
#'
#' @export
kableLM <- function(LM, ...) {

  s <- summary(LM)

  p <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = F) %>%
    num2str(precision = 3, isProportion = T) %>%
    lteq()

  r2 <- num2str(s$adj.r.squared, precision = 3, isProportion = T) %>%
    lteq()

  s$coefficients %>%
    as_tibble(rownames = "Effect") %>%
    mutate(`p<.05` = if_else(`Pr(>|t|)` < .05, '$*$', '')) %>%
    select(Effect,
           Estimate,
           SE = `Std. Error`,
           `$t$` = `t value`,
           `$p$` = `Pr(>|t|)`,
           ` ` = `p<.05`) %>%
    kable(align = c('l', 'r', 'r', 'r', 'r', 'c'),
          escape = F,
          ...) %>%
    kable_styling() %>%
    column_spec(1, bold = T) %>%
    footnote(
      general_title = "",
      general = paste0(
        "Model fit: $F$(", round(s$fstatistic[1], 1), ", ",
        round(s$fstatistic[2], 1), ") = ",
        round(s$fstatistic[3], 2), "; $p$ ", p,
        "; $R^2_{adj}$ ", r2
      )
    )
}

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
#' @export
kableANOVA <- function(ANOVA, ...) {
  require(magrittr)
  require(tibble)
  require(dplyr)
  require(kableExtra)

  tmp <- ANOVA %>%
    mutate(`F` = num2str(`F`),
           p = num2str(p, precision = 3, isProportion = T),
           `p<.05` = if_else(`p<.05` == "", "", "$\\text{*}$"),
           ges = num2str(ges, precision = 3, isProportion = T))
  rownames(tmp) <- NULL

  tmp %>%
    select(Effect,
           `$F$` = `F`,
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

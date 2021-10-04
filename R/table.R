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
           p = p2str(p, prefix = "", minPrefix = "< "),
           `p<.05` = if_else(`p<.05` == "", "", "$\\text{*}$"),
           ges = prop2str(ges))
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
#' @importFrom dplyr mutate select as_tibble if_else
#' @import kableExtra
#'
#' @export
kableLM <- function(LM, ...) {

  s <- summary(LM)

  p <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = F) %>%
    p2str(prefix = "", minPrefix = "< ")

  r2 <- prop2str(s$adj.r.squared, minPrefix = NA) %>%
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
    mutate(
      Estimate = num2str(Estimate),
      SE = num2str(SE),
      `$t$` = num2str(`$t$`),
      `$p$` = p2str(`$p$`, prefix = "", minPrefix = "< ")
    ) %>%
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
      ),
      escape = F
    )
}

#' Return a row with columns named for the coefficients and values containing
#' the requested property for each of models
#'
#' @param models list of (lm) models to extract property from
#' @param property to extract
#'
#' @importFrom dplyr bind_cols full_join
#'
.modelValues <- function(models, property = "Estimate") {
  out <- NULL
  for (i in 1:length(models)) {
    m <- summary(models[[i]])
    if (!(property %in% colnames(m$coefficients)))
      warning(paste0("Property '", property, "' not found in model ", i))

    n <- which(colnames(m$coefficients) == property)
    tmp <- m$coefficients[, n]
    tmp <- bind_cols(model = i, as_tibble(t(tmp)))

    if (is.null(out))
      out <- tmp
    else
      out <- full_join(out, tmp, by = names(tmp)[names(tmp) %in% names(out)])
  }

  out
}

#' Fetch model properties for all models in a list
#'
#' @param models list of (lm) models for which to fetch properties
#'
#' @importFrom dplyr bind_rows
#'
.modelProperties <- function(models) {
  out <- NULL
  for (i in 1:length(models)) {
    m <- summary(models[[i]])
    tmp <- tibble(
      model = i,
      dfn = m$fstatistic[2],
      dfd = m$fstatistic[3],
      f = m$fstatistic[1],
      pf = pf(f, dfn, dfd, lower.tail = F),
      rsq_adj = m$adj.r.squared
    )
    out <- if (is.null(out)) tmp else bind_rows(out, tmp)
  }

  out
}

#' Produce a neat table showing comparisons between various models
#'
#' @param models list of lm models to compare with anova()
#' @param ... passed on to \code{kableExtra::kable}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols left_join bind_rows rename
#' @import kableExtra
#' @importFrom tibble tribble tibble
#'
#' @examples
#' m1 <- lm(Sepal.Length ~ Species, iris)
#' m2 <- lm(Sepal.Length ~ Species + Petal.Length, iris)
#' m3 <- lm(Sepal.Length ~ Species + Petal.Length + Petal.Width, iris)
#' m4 <- lm(Sepal.Length ~ Species + Petal.Length + Petal.Width + Sepal.Width, iris)
#' kableModelComparison(list(m1, m2, m3, m4))
#'
#' @export
kableModelComparison <- function(models, ...) {
  # Form table of estimates
  est <- .modelValues(models)
  p <- .modelValues(models, 'Pr(>|t|)')

  # Form table of model properties
  props <- .modelProperties(models)

  # Comparison stats
  a <- do.call(anova, models)
  # Note: F = delta_R^2 F-test; f = model F-test
  props <- bind_cols(props, a[, c('F', 'Pr(>F)')])

  tbl <- NULL

  for (i in 1:length(models)) {
    tmp <- tibble()
    for (v in 2:ncol(est)) {
      tmp <- bind_rows(
        tmp,
        tribble(
          ~var, ~property, ~value,
          colnames(est)[v], '$\\beta$', num2str(as.numeric(est[i, v])),
          colnames(p)[v], '$p$', p2str(as.numeric(p[i, v]), prefix = "", minPrefix = "< ")
        )
      )
    }

    # Comparison stats
    tmp <- bind_rows(
      tmp,
      tribble(
        ~var, ~property, ~value,
        '$F$', '$df$', paste0(props$dfn[i], ' / ', props$dfd[i]),
        '$F$', '$F$', num2str(props$f[i]),
        '$F$', '$p$', p2str(props$pf[i], prefix = "", minPrefix = "< "),
        '$R^2_{adj}$', '$R^2_{adj}$', prop2str(props$rsq_adj[i], minPrefix = NA)
      )
    )

    if (i == 1)
      tmp <- bind_rows(
        tmp,
        tribble(
          ~var, ~property, ~value,
          '$R^2_{adj}$', '$\\Delta$', '',
          '$R^2_{adj}$', '$F$', '',
          '$R^2_{adj}$', '$p$', '',
        )
      )
    else
      tmp <- bind_rows(
        tmp,
        tribble(
          ~var, ~property, ~value,
          '$R^2_{adj}$', '$\\Delta$', prop2str(props$rsq_adj[i] - props$rsq_adj[i - 1], minPrefix = NA),
          '$R^2_{adj}$', '$F$', num2str(props$`F`[i]),
          '$R^2_{adj}$', '$p$', p2str(props$`Pr(>F)`[i], prefix = "", minPrefix = "< ")
        )
      )
    tmp$value <- ifelse(is.na(tmp$value), "", tmp$value)
    names(tmp)[3] <- i

    tbl <- if (is.null(tbl)) tmp else left_join(tbl, tmp, by = c('var', 'property'))
  }

  # Print tbl neatly
  tbl %>%
    rename(` ` = var, `  ` = property) %>%
    kable(
      align = c('l', 'c', rep('r', length(models))),
      escape = F,
      ...
    ) %>%
    kable_styling() %>%
    collapse_rows(1:2, valign = "middle") %>%
    add_header_above(c(" " = 2, "Model" = length(models)))
}

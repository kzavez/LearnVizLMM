#' Model equation in 'LaTeX' format
#'
#' @description
#' `extract_equation()` takes the [nlme::lme()] or [lme4::lmer()] code for
#' fitting a linear mixed effect model and returns the corresponding model
#' equation written in 'LaTeX' notation.
#'
#' @param model Code for fitting a [nlme::lme()] or [lme4::lmer()] model given
#'  as a string.
#' @param cat_vars Optional character vector of the names of categorical
#'  predictor variables included in the `model`. Default is `NULL`, which assumes
#'  that all predictor variables are numeric.
#' @param cat_vars_nlevels Optional numeric vector of the number of levels (i.e.
#'  categories) for each variable in `cat_vars`. Must be a vector of same length
#'  as `cat_vars`. Values must be whole numbers greater than `1` and less than
#'  `10`. Only applies if `cat_vars` is not `NULL`.
#' @param output_type Output type can be `"latex"` (default), `"string"`, or
#' `"none"`.
#'
#' @return None (invisible NULL) (`output_type = "latex"`), a string
#'  (`output_type = "string"`), or no output (`output_type = "none"`).
#' @export
#'
#' @examples
#' # Different ways to write the same lme model
#' extract_equation(model = "lme(score ~ age, random=~age|subject)")
#' extract_equation(model = "lme(score ~ age, random=list(subject=~age))")
#'
#' # Correlated vs. Uncorrelated
#' extract_equation(model = "lmer(score ~ age + (age|subject))")
#' extract_equation(model = "lmer(score ~ age + (age||subject))")
#'
#' # Add a categorical predictor and interaction
#' extract_equation(model = "lmer(score ~ age*treat + (age|subject))",
#'                  cat_vars = "treat",
#'                  cat_vars_nlevels = 3)
extract_equation <- function(model,
                             cat_vars = NULL,
                             cat_vars_nlevels = NULL,
                             output_type = "latex") {

  make_vt_output <- make_vt(model = model,
                            cat_vars = cat_vars,
                            cat_vars_nlevels = cat_vars_nlevels)
  vt_out <- make_vt_output[[1]]
  y <- make_vt_output[[2]]
  n_gf <- make_vt_output[[3]]
  gf_description <- make_vt_output[[4]]
  fixed_intercept <- make_vt_output[[7]]
  n_re <- make_vt_output[[8]]
  gf_intercept <- make_vt_output[[9]]
  gf_re_corr <- make_vt_output[[10]]
  gf_all <- make_vt_output[[11]]
  df_varcov_list <- make_vt_output[[12]]
  fixed <- make_vt_output[[16]]
  random <- make_vt_output[[17]]
  model_call <- make_vt_output[[18]]

  eq <- "\\begin{aligned}\n  \\operatorname{outcome}_{index1} &="
  index_cross <- c("i", "ij", "ijk", "ijkl")
  index_nest <- c("i", "j(i)", "k(ij)", "l(ijk)")
  index_crosswith <- c("i", "j", "k(j)")
  index_crosswithin <- c("i", "j(i)", "k(i)")
  index_general <- c("i", "j", "k", "l")
  index_end <- c("a", "b", "c")
  re_coef <- c("u_", "v_", "w_")

  eq <- stringr::str_replace_all(eq, "index1", index_cross[(n_gf + 1)])
  eq <- stringr::str_replace_all(eq, "outcome", y)

  vt_out$eq_term <- vt_out$updated_term
  for (i in 1:nrow(vt_out)) {
    if (stringr::str_detect(vt_out$eq_term[i], stringr::fixed("I("))) {
      vt_out$eq_term[i] <- stringr::str_remove(vt_out$eq_term[i], stringr::fixed("I("))
      vt_out$eq_term[i] <- stringr::str_remove(vt_out$eq_term[i], stringr::fixed(")"))
    }
  }

  vt_fe <- vt_out[which(vt_out$effect == "fixed"), ]
  vt_fe2 <- vt_fe[which(vt_fe$eq_term == "1"), ]
  not_only_intercept <- (nrow(vt_fe) - nrow(vt_fe2))
  if (not_only_intercept == 0) {
    vt_out$coef_no <- NA
    vt_out$coef_no[which(vt_out$eq_term == 1)] <- 0
    na_coef_no <- nrow(vt_out[which(is.na(vt_out$coef_no)), ])
    if (na_coef_no >= 1) {
      na_idx <- which(is.na(vt_out$coef_no))
      for (i in 1:na_coef_no) {
        vt_out$coef_no[na_idx[i]] <- i
      }
    }
  } else {
    vt_fe <- vt_fe[which(vt_fe$eq_term != "1"), ]
    vt_fe$coef_no <- 1:nrow(vt_fe)
    vt_fe <- vt_fe[,c("eq_term", "coef_no")]
    vt_out <- dplyr::left_join(vt_out, vt_fe, by = "eq_term")
    vt_out$coef_no[which(vt_out$eq_term == 1)] <- 0
    na_coef_no <- nrow(vt_out[which(is.na(vt_out$coef_no)), ])
    if (na_coef_no >= 1) {
      na_idx <- which(is.na(vt_out$coef_no))
      for (i in 1:na_coef_no) {
        vt_out$coef_no[na_idx[i]] <- nrow(vt_fe) + i
      }
    }
  }
  vt_out_fe <- vt_out

  fe <- if (fixed_intercept == "yes") "\\beta_{0}" else ""
  if (not_only_intercept != 0) {
    for (i in 1:nrow(vt_fe)) {
      if (i %in% c(5, 9, 13, 17, 21)) {
        fe <- paste0(fe, " \\\\\n  &+ \\beta_{", vt_fe$coef_no[i],
                     "}(\\operatorname{", vt_fe$eq_term[i], "})")
      } else {
        fe <- paste0(fe, " + \\beta_{", vt_fe$coef_no[i],
                     "}(\\operatorname{", vt_fe$eq_term[i], "})")
      }
    }
    if (stringr::str_starts(fe, stringr::fixed("+"))) {
      fe <- stringr::str_sub(fe, start = 2)
    }
  }
  eq <- paste(eq, fe)

  ra_list <- list("", "", "")
  for (i in 1:n_gf) {
    df_varcov <- df_varcov_list[[i]]
    vt_re_sub <- vt_out[which(vt_out$group3 == gf_all[i]), ]
    df_varcov$variable <- vt_re_sub$eq_term
    df_varcov$coef_no <- as.character(vt_re_sub$coef_no)

    if (n_re[i] == 1) {
      ra <- "z_{indexAindex1} &\\sim N \\left(0, \\tau^2_{z_{indexA}} \\right), \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
      ra <- stringr::str_replace_all(ra, "indexA", df_varcov$coef_no[1])
    }
    if (n_re[i] == 2) {
      if (gf_re_corr[i] == "yes") {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{cc}\n \\tau^2_{z_{indexA}} & \\tau_{z_{indexA}z_{indexB}} \\\\\n \\tau_{z_{indexB}z_{indexA}} & \\tau^2_{z_{indexB}}\n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
      } else {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{cc}\n \\tau^2_{z_{indexA}} & 0 \\\\\n 0 & \\tau^2_{z_{indexB}}\n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
      }
      ra <- stringr::str_replace_all(ra, "indexA", df_varcov$coef_no[1])
      ra <- stringr::str_replace_all(ra, "indexB", df_varcov$coef_no[2])
    }
    if (n_re[i] == 3) {
      if (gf_re_corr[i] == "no") {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1} \\\\\n &z_{indexCindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{ccc}\n \\tau^2_{z_{indexA}} & 0 & 0 \\\\\n 0 & \\tau^2_{z_{indexB}} & 0 \\\\\n 0 & 0 & \\tau^2_{z_{indexC}} \n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
      } else {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1} \\\\\n &z_{indexCindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{ccc}\n \\tau^2_{z_{indexA}} & \\tau_{z_{indexA}z_{indexB}} & \\tau_{z_{indexA}z_{indexC}} \\\\\n \\tau_{z_{indexB}z_{indexA}} & \\tau^2_{z_{indexB}} & \\tau_{z_{indexB}z_{indexC}} \\\\\n \\tau_{z_{indexC}z_{indexA}} & \\tau_{z_{indexC}z_{indexB}} & \\tau^2_{z_{indexC}} \n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
        if (gf_re_corr[i] == "complex") {
          if (df_varcov$m1[2] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexB}z_{indexA}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexA}z_{indexB}}"), "0")
          }
          if (df_varcov$m1[3] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexC}z_{indexA}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexA}z_{indexC}}"), "0")
          }
          if (df_varcov$m2[3] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexB}z_{indexC}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexC}z_{indexB}}"), "0")
          }
        }
      }
      ra <- stringr::str_replace_all(ra, "indexA", df_varcov$coef_no[1])
      ra <- stringr::str_replace_all(ra, "indexB", df_varcov$coef_no[2])
      ra <- stringr::str_replace_all(ra, "indexC", df_varcov$coef_no[3])
    }
    if (n_re[i] == 4) {
      if (gf_re_corr[i] == "no") {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1} \\\\\n &z_{indexCindex1} \\\\\n &z_{indexDindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \\\\\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{cccc}\n \\tau^2_{z_{indexA}} & 0 & 0 & 0 \\\\\n 0 & \\tau^2_{z_{indexB}} & 0 & 0 \\\\\n 0 & 0 & \\tau^2_{z_{indexC}} & 0 \\\\\n 0 & 0 & 0 & \\tau^2_{z_{indexD}} \n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
      } else {
        ra <- "\\left(\n \\begin{array}{c} \n \\begin{aligned}\n &z_{indexAindex1} \\\\\n &z_{indexBindex1} \\\\\n &z_{indexCindex1} \\\\\n &z_{indexDindex1}\n \\end{aligned}\n \\end{array}\n\\right)\n &\\sim N \\left(\n\\left(\n \\begin{array}{c} \n \\begin{aligned}\n & 0 \\\\\n & 0 \\\\\n & 0 \\\\\n & 0 \n \\end{aligned}\n \\end{array}\n\\right)\n, \n\\left(\n \\begin{array}{cccc}\n \\tau^2_{z_{indexA}} & \\tau_{z_{indexA}z_{indexB}} & \\tau_{z_{indexA}z_{indexC}} & \\tau_{z_{indexA}z_{indexD}} \\\\\n \\tau_{z_{indexB}z_{indexA}} & \\tau^2_{z_{indexB}} & \\tau_{z_{indexB}z_{indexC}} & \\tau_{z_{indexB}z_{indexD}} \\\\\n \\tau_{z_{indexC}z_{indexA}} & \\tau_{z_{indexC}z_{indexB}} & \\tau^2_{z_{indexC}} & \\tau_{z_{indexC}z_{indexD}} \\\\\n \\tau_{z_{indexD}z_{indexA}} & \\tau_{z_{indexD}z_{indexB}} & \\tau_{z_{indexD}z_{indexC}} & \\tau^2_{z_{indexD}} \n \\end{array}\n\\right)\n \\right),\n \\text{ for groupingfactor index2 = 1,} \\dots \\text{,index3}"
        if (gf_re_corr[i] == "complex") {
          if (df_varcov$m1[2] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexB}z_{indexA}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexA}z_{indexB}}"), "0")
          }
          if (df_varcov$m1[3] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexC}z_{indexA}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexA}z_{indexC}}"), "0")
          }
          if (df_varcov$m1[4] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexD}z_{indexA}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexA}z_{indexD}}"), "0")
          }
          if (df_varcov$m2[3] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexB}z_{indexC}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexC}z_{indexB}}"), "0")
          }
          if (df_varcov$m2[4] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexB}z_{indexD}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexD}z_{indexB}}"), "0")
          }
          if (df_varcov$m3[4] == 0) {
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexD}z_{indexC}}"), "0")
            ra <- stringr::str_replace(ra, stringr::fixed("\\tau_{z_{indexC}z_{indexD}}"), "0")
          }
        }
      }
      ra <- stringr::str_replace_all(ra, "indexA", df_varcov$coef_no[1])
      ra <- stringr::str_replace_all(ra, "indexB", df_varcov$coef_no[2])
      ra <- stringr::str_replace_all(ra, "indexC", df_varcov$coef_no[3])
      ra <- stringr::str_replace_all(ra, "indexD", df_varcov$coef_no[4])
    }

    if (gf_intercept[i] != "no") {
      re <- "z_{0index1}"
      df_varcov <- df_varcov[which(df_varcov$variable != "1"), ]
    } else {
      re <- ""
    }
    if (n_re[i] >= 2) {
      for (j in 1:nrow(df_varcov)) {
        re <- paste0(re, " + z_{", df_varcov$coef_no[j],
                     "index1}(\\operatorname{", df_varcov$variable[j], "})")
      }
      if (stringr::str_starts(re, stringr::fixed(" +"))) {
        re <- stringr::str_sub(re, start = 3)
      }
    }

    if (gf_description == "nested") {
      re <- stringr::str_replace_all(re, "index1", index_nest[i])
      ra <- stringr::str_replace_all(ra, "index1", index_nest[i])
    }else if (gf_description == "crossed with nested") {
      re <- stringr::str_replace_all(re, "index1", index_crosswith[i])
      ra <- stringr::str_replace_all(ra, "index1", index_crosswith[i])
    }else if (gf_description == "crossed within nested") {
      re <- stringr::str_replace_all(re, "index1", index_crosswithin[i])
      ra <- stringr::str_replace_all(ra, "index1", index_crosswithin[i])
    } else {
      re <- stringr::str_replace_all(re, "index1", index_general[i])
      ra <- stringr::str_replace_all(ra, "index1", index_general[i])
    }
    ra <- stringr::str_replace_all(ra, "index2", index_general[i])
    ra <- stringr::str_replace_all(ra, "index3", index_end[i])
    re <- stringr::str_replace_all(re, "z_", stringr::fixed(re_coef[i]))
    ra <- stringr::str_replace_all(ra, "z_", stringr::fixed(re_coef[i]))
    ra <- stringr::str_replace_all(ra, "groupingfactor", gf_all[i])
    eq <- paste(eq, "\\\\\n  &+", re)
    ra_list[[i]] <- ra
  }

  eq <- paste0(eq, " \\\\\n &+ \\epsilon_{", index_cross[(n_gf + 1)], "}")
  for (i in 1:n_gf) {
    eq <- paste(eq, "\\\\\n", ra_list[[i]])
  }
  eq <- paste0(eq,
               " \\\\\n \\epsilon_{",
               index_cross[(n_gf + 1)],
               "} &\\sim N \\left(0, \\sigma^2 \\right), \\text{ for Observation ",
               index_general[(n_gf + 1)],
               " = 1,} \\dots \\text{,n}  \n\\end{aligned}")

  eq <- paste0(c("$$\n", eq, "\n$$\n"), collapse = "")
  if (output_type == "latex"){
    cat(eq)
  } else if (output_type == "string") {
    eq
  }

}

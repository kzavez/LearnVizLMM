#' Roles of variables
#'
#' @description
#' `extract_variables()` returns a data frame of information of the variables in
#' a [nlme::lme()] or [lme4::lmer()] model. The columns of the data frame
#' include: `Effect` (whether the effect is random or fixed), `Group` (group or
#' grouping factor associated with random effects), `Term` (notation used to
#' include the variable in the model), `Description` (description of the `Term`),
#' and `Parameter` (parameter estimated when the model is fit).
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
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # lme()
#' extract_variables(model = "lme(Score~type,random=list(School=pdDiag(~1+type),Class=~1))",
#'                   cat_vars = "type",
#'                   cat_vars_nlevels = 2)
#' extract_variables(model = "lme(weight~1+Time+I(Time^2),random=~Time+I(Time^2)|ID)")
#'
#' # lmer()
#' extract_variables(model = "lmer(Strength ~ 1 + (1|Machine) + (1|Worker))")
#' extract_variables(model = "lmer(score ~ age*treat + (age|subject))",
#'                   cat_vars = "treat",
#'                   cat_vars_nlevels = 3)
extract_variables <- function(model,
                              cat_vars = NULL,
                              cat_vars_nlevels = NULL) {

  make_vt_output <- make_vt(model = model,
                            cat_vars = cat_vars,
                            cat_vars_nlevels = cat_vars_nlevels)
  vt_out <- make_vt_output[[1]]
  n_gf <- make_vt_output[[3]]
  gf_description <- make_vt_output[[4]]
  n_re <- make_vt_output[[8]]
  gf_intercept <- make_vt_output[[9]]
  gf_re_corr <- make_vt_output[[10]]
  gf_all <- make_vt_output[[11]]
  df_varcov_list <- make_vt_output[[12]]
  random_contains_list <- make_vt_output[[13]]
  random_df <- make_vt_output[[14]]
  bar_where_int <- make_vt_output[[15]]
  random <- make_vt_output[[17]]
  model_call <- make_vt_output[[18]]

  # Make rdf - an updated random data frame
  rdf <- data.frame(group2 = random_df$c3, group3 = random_df$name2,
                    bar_type = random_df$c2,
                    term1 = random_df$c1_old, term2 = random_df$c1_old,
                    term3 = random_df$c1_old, dots = 0)
  for (i in 1:nrow(rdf)) {
    if (stringr::str_detect(rdf$term1[i], stringr::fixed("+||+"))) {
      if (model_call == "lmer") {
        if (gf_intercept[i] == "yes") {
          replaceA <- paste0("|", rdf$group2[i], ")+(")
          rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("+||+"), stringr::fixed(replaceA))
          rdf$term2[i] <- paste0("(", rdf$term2[i], "|", rdf$group2[i], ")")
        } else {
          replaceA <- paste0("|", rdf$group2[i], ")+(")
          rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("+||+"), stringr::fixed(replaceA))
          rdf$term2[i] <- paste0("(1+", rdf$term2[i], "|", rdf$group2[i], ")")
        }
        term2_split <- stringr::str_split_fixed(rdf$term2[i], stringr::fixed(")+("), 2)
        rdf$term2[i] <- paste0(term2_split[1, 1], ")")
        rdf$term3[i] <- paste0("(", term2_split[1, 2])
        check_varcov <- df_varcov_list[[i]]
        row_varcov <- nrow(check_varcov)
        if (check_varcov[1, 3] == 0) {
          rdf$dots[i] <- 1
        } else if (check_varcov[1, 4] == 0 && row_varcov >= 3) {
          rdf$dots[i] <- 2
        } else if (check_varcov[1, 5] == 0 && row_varcov == 4) {
          rdf$dots[i] <- 3
        }

      } else if (model_call == "lme") {
        if (gf_intercept[i] == "yes") {
          rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("+||+"), stringr::fixed(",~"))
          rdf$term2[i] <- paste0(rdf$group3[i], "=pdBlocked(list(~", rdf$term2[i], "))")
        } else {
          if (bar_where_int[i] == 1) {
            rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("+||+"), stringr::fixed(",~"))
            rdf$term2[i] <- paste0(rdf$group3[i], "=pdBlocked(list(~1+", rdf$term2[i], "))")
          } else if (bar_where_int[i] == 2) {
            rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("+||+"), stringr::fixed(",~1+"))
            rdf$term2[i] <- paste0(rdf$group3[i], "=pdBlocked(list(~", rdf$term2[i], "))")
          }
        }
      }
    } else {
      if (model_call == "lmer") {
        if (gf_intercept[i] == "added") {
          rdf$term2[i] <- paste0("(1+", rdf$term2[i], rdf$bar_type[i], rdf$group2[i], ")")
        } else {
          rdf$term2[i] <- paste0("(", rdf$term2[i], rdf$bar_type[i], rdf$group2[i], ")")
        }
      }
      if (model_call == "lme" && random_contains_list == "no" && gf_re_corr[i] == "yes") {
        if (gf_description == "nested") {
          random <- stringr::str_remove_all(as.character(random), " ")
          rdf$term2[i] <- random
          if (gf_intercept[i] == "added") {
            rdf$term2[i] <- stringr::str_replace(rdf$term2[i], stringr::fixed("~"), stringr::fixed("~1+"))
          }
        } else {
          if (gf_intercept[i] == "added") {
            rdf$term2[i] <- paste0("~1+", rdf$term2[i], "|", rdf$group2[i])
          } else {
            rdf$term2[i] <- paste0("~", rdf$term2[i], "|", rdf$group2[i])
          }
        }
      }
      if (model_call == "lme" && gf_re_corr[i] == "no") {
        if (gf_intercept[i] == "added") {
          rdf$term2[i] <- paste0(rdf$group3[i], "=pdDiag(~1+", rdf$term2[i], ")")
        } else {
          rdf$term2[i] <- paste0(rdf$group3[i], "=pdDiag(~", rdf$term2[i], ")")
        }
      }
      if (model_call == "lme" && random_contains_list == "yes" && gf_re_corr[i] == "yes") {
        if (gf_intercept[i] == "added") {
          rdf$term2[i] <- paste0(rdf$group3[i], "=~1+", rdf$term2[i])
        } else {
          rdf$term2[i] <- paste0(rdf$group3[i], "=~", rdf$term2[i])
        }
      }
    }
  }
  rdf <- rdf[,c("group2", "group3", "term2", "term3", "dots")]
  vt_out <- dplyr::left_join(vt_out, rdf, by = c("group2", "group3"))
  vt_out$term2[which(is.na(vt_out$term2))] <- vt_out$raw_term[which(is.na(vt_out$term2))]
  vt_out$term3[which(is.na(vt_out$term3))] <- ""
  vt_out$dots[which(is.na(vt_out$dots))] <- 0

  vt_out$count <- 1
  start_idx <- 1
  for (i in 2:nrow(vt_out)) {
    group_2_match <- vt_out$group3[start_idx]
    if (vt_out$group3[i] == group_2_match){
      vt_out$count[i] <- vt_out$count[i - 1] + 1
    } else {
      start_idx <- i
    }
    if (vt_out$dots[i] != 0 && vt_out$dots[i] < vt_out$count[i]) {
      vt_out$term2[i] <- vt_out$term3[i]
    }
  }
  vt_out$term3 <- vt_out$term2
  gf_parts <- unique(vt_out$term3[which(vt_out$effect == "random")])
  for (i in 1:length(gf_parts)) {
    rterm <- unique(vt_out$raw_term[which(vt_out$term3 == gf_parts[i])])
    if (length(rterm) >= 2) {
      for (j in 1:length(rterm)) {
        rtermj <- rterm[j]
        not_rtermj <- rterm[-j]
        for (k in 1:nrow(vt_out)) {
          if (vt_out$term3[k] == gf_parts[i] && vt_out$raw_term[k] == rtermj) {
            for (t in 1:length(not_rtermj)) {
              vt_out$term2[k] <- stringr::str_replace(vt_out$term2[k],
                                                      stringr::fixed(not_rtermj[t]),
                                                      stringr::fixed("..."))
            }
          }
        }
      }
    }
  }
  vt_out <- vt_out[,1:8]
  vt_out1 <- vt_out

  vt_out$parameter <- vt_out$updated_term
  vt_out$parameter <- stringr::str_replace_all(vt_out$parameter, stringr::fixed("*"), stringr::fixed(":"))
  vt_out$parameter[which(vt_out$parameter == "1" & vt_out$effect == "fixed")] <- "(Intercept)"
  vt_out$parameter[which(vt_out$parameter == "1" & vt_out$effect == "random")] <- "Intercept"
  vt_out$parameter[which(vt_out$effect == "random")] <-
    paste0("SD (", vt_out$parameter[which(vt_out$effect == "random")], ")")

  vt_fe <- vt_out[which(vt_out$effect == "fixed"), ]
  vt_re <- vt_out[which(vt_out$effect == "random"), ]
  vt_re1 <- vt_re[which(vt_re$group3 == gf_all[1]), ]
  vt_re2 <- vt_re[which(vt_re$group3 == gf_all[2]), ]
  vt_re3 <- vt_re[which(vt_re$group3 == gf_all[3]), ]
  vt_cov <- data.frame(effect = rep("random", 10), group = "",
                       group2 = "", group3 = "", raw_term = "",
                       updated_term = "", ref_level = "",
                       term2 = "", parameter = "")
  vt_cov_list <- list(vt_cov, vt_cov, vt_cov)

  # add in correlation
  for (i in 1:n_gf) {
    vt_cov_update <- vt_cov_list[[i]]
    gf_group_info <- unique(vt_re[which(vt_re$group3 == gf_all[i]), c("group", "group2", "group3")])
    vt_cov_update[, c("group", "group2", "group3")] <- gf_group_info
    df_varcov <- df_varcov_list[[i]]
    df_varcov$variable[which(df_varcov$variable == "1")] <- "Intercept"
    df_varcov$variable <- stringr::str_replace_all(df_varcov$variable, stringr::fixed("*"), stringr::fixed(":"))
    if (n_re[i] == 2) {
      if (df_varcov$m1[2] != 0) {
        vt_cov_update$parameter[1] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[2], ")")
      }
    }
    if (n_re[i] == 3) {
      if (df_varcov$m1[2] != 0) {
        vt_cov_update$parameter[1] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[2], ")")
      }
      if (df_varcov$m1[3] != 0) {
        vt_cov_update$parameter[2] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[3], ")")
      }
      if (df_varcov$m2[3] != 0) {
        vt_cov_update$parameter[3] <- paste0("Cor (", df_varcov$variable[2], ",",
                                             df_varcov$variable[3], ")")
      }
    }
    if (n_re[i] == 4) {
      if (df_varcov$m1[2] != 0) {
        vt_cov_update$parameter[1] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[2], ")")
      }
      if (df_varcov$m1[3] != 0) {
        vt_cov_update$parameter[2] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[3], ")")
      }
      if (df_varcov$m1[4] != 0) {
        vt_cov_update$parameter[3] <- paste0("Cor (", df_varcov$variable[1], ",",
                                             df_varcov$variable[4], ")")
      }
      if (df_varcov$m2[3] != 0) {
        vt_cov_update$parameter[4] <- paste0("Cor (", df_varcov$variable[2], ",",
                                             df_varcov$variable[3], ")")
      }
      if (df_varcov$m2[4] != 0) {
        vt_cov_update$parameter[5] <- paste0("Cor (", df_varcov$variable[2], ",",
                                             df_varcov$variable[4], ")")
      }
      if (df_varcov$m3[4] != 0) {
        vt_cov_update$parameter[6] <- paste0("Cor (", df_varcov$variable[3], ",",
                                             df_varcov$variable[4], ")")
      }
    }
    vt_cov_list[[i]] <- vt_cov_update
  }
  vt <- rbind(vt_fe, vt_re1, vt_cov_list[[1]], vt_re2, vt_cov_list[[2]], vt_re3, vt_cov_list[[3]])
  vt[nrow(vt), ] <- c("random", "", "", "", "", "", "", "", "SD (Residual)")
  vt <- vt[which(vt$parameter != ""), ]

  # Add in descriptions
  vt$desc1 <- ""
  vt$desc1[which(vt$term2 == "1")] <- "intercept"
  vt$desc1[which(stringr::str_detect(vt$parameter, stringr::fixed(":")) & vt$effect == "fixed")] <- "interaction"
  vt$desc1[which((vt$raw_term %in% cat_vars) & vt$effect == "fixed")] <- "categorical"
  vt$desc1[which(vt$desc1 == "" & vt$effect == "fixed")] <- "numeric"

  vt$desc1[which(vt$updated_term == "1" & vt$effect == "random")] <-
    paste0(vt$group3[which(vt$updated_term == "1" & vt$effect == "random")],
           "-specific intercepts")
  vt$desc1[which((!vt$raw_term %in% c("", "1")) & vt$effect == "random")] <-
    paste0(vt$group3[which((!vt$raw_term %in% c("", "1")) & vt$effect == "random")],
           "-specific slopes for ",
           vt$updated_term[which((!vt$raw_term %in% c("", "1")) & vt$effect == "random")])

  vt_final <- vt[,c("effect", "group", "term2", "desc1", "parameter")]
  colnames(vt_final) <- c("Effect", "Group", "Term", "Description", "Parameter")
  rownames(vt_final) <- NULL
  vt_final
  # return(list(rdf, vt_out1, vt_final))
  # return(list(random_df, rdf))
}




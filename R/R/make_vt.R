# The functions extract_equation() and extract_variables() require variable information
make_vt <- function(model,
                    cat_vars = NULL,
                    cat_vars_nlevels = NULL) {

  extract_gf_output <- extract_gf(model = model,
                                  cat_vars = cat_vars,
                                  cat_vars_nlevels = cat_vars_nlevels)
  re <- extract_gf_output[[1]]
  model_eq <- extract_gf_output[[2]]
  random_df <- extract_gf_output[[3]]
  n_gf <- extract_gf_output[[4]]
  gf_description <- extract_gf_output[[5]]
  gf_cross <- extract_gf_output[[6]]
  gf_nest <- extract_gf_output[[7]]
  random_contains_list <- extract_gf_output[[8]]
  fixed <- extract_gf_output[[9]]
  random <- extract_gf_output[[10]]
  model_call <- extract_gf_output[[11]]

  # Define y, fixed_intercept, and fe_df$raw_term  --------------
  fixed <- stringr::str_remove_all(as.character(fixed), " ")
  if (!stringr::str_detect(fixed, stringr::fixed("~"))) {
    cli::cli_abort(c(
      "Can't separate outcome from fixed effects.",
      "i" = "Trying using 'outcome ~ 1 + FE + ...' to specify the fixed effects."
    ))
  }
  fe_split <- stringr::str_split_fixed(fixed, stringr::fixed("~"), 2)
  y <- fe_split[1, 1]
  mod_fe <- fe_split[1, 2]
  mod_fe <- paste0("1+", mod_fe)
  mod_fe <- stringr::str_replace_all(mod_fe, stringr::fixed(")-1"), stringr::fixed(")+-1"))
  mod_fe_split <- unlist(stringr::str_split(mod_fe, stringr::fixed("+")))
  fixed_intercept <- "yes"
  fe_df <- data.frame(effect = rep("fixed", 50), group = "",
                      group2 = "", group3 = "", raw_term = "")
  for (j in 1:length(mod_fe_split)) {
    if (mod_fe_split[j] == "0" || mod_fe_split[j] == "-1") {
      fixed_intercept <- "no"
    } else {
      if (stringr::str_detect(mod_fe_split[j], stringr::fixed("I("))) {
        if (!stringr::str_detect(mod_fe_split[j], stringr::fixed(")"))) {
          get_rest <- stringr::str_split_fixed(mod_fe_split[(j + 1)], stringr::fixed(")"), 2)
          mod_fe_split[j] <- paste0(mod_fe_split[j], "+", get_rest[1, 1], ")")
          mod_fe_split[(j + 1)] <- get_rest[1, 2]
        }
      } else {
        if (stringr::str_detect(mod_fe_split[j], stringr::fixed("-1"))) {
          mod_fe_split[j] <- stringr::str_remove(mod_fe_split[j], stringr::fixed("-1"))
          fixed_intercept <- "no"
        }
      }
      fe_df$raw_term[j] <- mod_fe_split[j]
    }
  }
  if (fixed_intercept == "no") {
    fe_df <- fe_df[which(fe_df$raw_term != "1"), ]
  }
  fe_df <- unique(fe_df[which(fe_df$raw_term != ""), ])

  # Handle Dups
  gf_re_corr <- c("yes", "yes", "yes")
  bar_where_int <- c(0, 0, 0)
  random_df$c1_old <- random_df$c1
  for (i in 1:nrow(random_df)) {
    mod_re <- random_df$c1[i]
    if (stringr::str_detect(mod_re, stringr::fixed("||"))) {
      mod_bar_split <- unlist(stringr::str_split(mod_re, stringr::fixed("+||+")))
      n_rm_intercept <- 0
      n_bar_parts <- length(mod_bar_split)
      gf_re_corr[i] <- "complex"
      for (j in 1:length(mod_bar_split)) {
        mod_bar_split2 <- paste0("+", mod_bar_split[j], "+")
        if (stringr::str_detect(mod_bar_split2, stringr::fixed("+0+")) ||
            stringr::str_detect(mod_bar_split2, stringr::fixed("-1+"))) {
          n_rm_intercept <- n_rm_intercept + 1
        } else {
          bar_where_int[i] <- j
        }
      }
      check_diff <- n_bar_parts - n_rm_intercept
      if (check_diff != 1) {
        cli::cli_abort(c(
          "Only one random intercept should be specified per grouping factor."
        ))
      } else {
        if (bar_where_int[i] == 2) {
          random_df$c1[i] <- paste0(mod_bar_split[2], stringr::fixed("+||+"),
                                    mod_bar_split[1])
        } else if (bar_where_int[i] == 3) {
          random_df$c1[i] <- paste0(mod_bar_split[3], stringr::fixed("+||+"),
                                    mod_bar_split[1], stringr::fixed("+||+"),
                                    mod_bar_split[2])
        } else if (bar_where_int[i] == 4) {
          random_df$c1[i] <- paste0(mod_bar_split[4], stringr::fixed("+||+"),
                                    mod_bar_split[1], stringr::fixed("+||+"),
                                    mod_bar_split[2], stringr::fixed("+||+"),
                                    mod_bar_split[3])
        }
        random_df$c1_old <- random_df$c1
        c1_update0 <- paste0("+", random_df$c1[i], "+")
        c1_update0 <- stringr::str_replace_all(c1_update0, stringr::fixed("+0+"), stringr::fixed("+"))
        c1_update0 <- stringr::str_replace_all(c1_update0, stringr::fixed("-1+"), stringr::fixed("+"))
        if (stringr::str_starts(c1_update0, stringr::fixed("+"))) {
          c1_update0 <- stringr::str_sub(c1_update0, start = 2)
        }
        if (stringr::str_ends(c1_update0, stringr::fixed("+"))) {
          c1_update0 <- stringr::str_sub(c1_update0, end = -2)
        }
        random_df$c1[i] <- c1_update0
      }
    }
  }


  n_re <- c(0, 0, 0)
  gf_intercept <- c("yes", "yes", "yes")
  re_df_empty <- data.frame(effect = rep("random", 20),
                            group = "", group2 = "", group3 = "",
                            raw_term = "")
  re_df_list <- list(re_df_empty, re_df_empty, re_df_empty)
  for (i in 1:nrow(random_df)) {
    mod_re <- random_df$c1[i]
    mod_re <- paste0("1+",mod_re)
    mod_re <- stringr::str_replace_all(mod_re, stringr::fixed(")-1"), stringr::fixed(")+-1"))
    mod_re_split <- unlist(stringr::str_split(mod_re, stringr::fixed("+")))
    re_df <- re_df_list[[i]]
    for (j in 1:length(mod_re_split)) {
      if (mod_re_split[j] == "0" || mod_re_split[j] == "-1") {
        gf_intercept[i] <- "no"
      } else {
        if (stringr::str_detect(mod_re_split[j], stringr::fixed("I("))) {
          if (!stringr::str_detect(mod_re_split[j], stringr::fixed(")"))) {
            get_rest <- stringr::str_split_fixed(mod_re_split[(j + 1)], stringr::fixed(")"), 2)
            mod_re_split[j] <- paste0(mod_re_split[j], "+", get_rest[1, 1], ")")
            mod_re_split[(j + 1)] <- get_rest[1, 2]
          }
        } else {
          if (stringr::str_detect(mod_re_split[j], stringr::fixed("-1"))) {
            mod_re_split[j] <- stringr::str_remove(mod_re_split[j], stringr::fixed("-1"))
            gf_intercept[i] <- "no"
          }
        }
        re_df$raw_term[j] <- mod_re_split[j]
      }
    }
    if (gf_intercept[i] == "no") {
      re_df <- re_df[which(re_df$raw_term != "1"), ]
    }
    if (length(which(re_df$raw_term == "1")) == 1) {
      gf_intercept[i] <- "added"
    }
    re_df <- unique(re_df[which(re_df$raw_term != ""), ])
    n_re[i] <- nrow(re_df)
    re_df$group <- random_df$name[i]
    if(random_df$c2[i] == "||") {
      gf_re_corr[i] <- "no"
    }
    re_df$group2 <- random_df$c3[i]
    re_df$group3 <- random_df$name2[i]
    re_df_list[[i]] <- re_df
  }
  e_df <- rbind(fe_df, re_df_list[[1]], re_df_list[[2]], re_df_list[[3]])
  e_df <- unique(e_df[which(e_df$raw_term != ""), ])


  e_df$star <- sapply(e_df$raw_term, function(x) stringr::str_detect(x, stringr::fixed("*")))
  e_df_star <- data.frame(effect = rep("", 100),
                          group = "", group2 = "", group3 = "",
                          raw_term = "")
  star_idx <- 0
  for (i in 1:nrow(e_df)) {
    if (e_df$star[i]) {
      star_split <- stringr::str_split(e_df$raw_term[i], stringr::fixed("*"), simplify = TRUE)[1, ]
      for (j in 1:length(star_split)) {
        e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                         e_df$group2[i], e_df$group3[i], star_split[j])
        star_idx <- star_idx + 1
      }
      if (length(star_split) == 3) {
        e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                         e_df$group2[i], e_df$group3[i],
                                         paste0(star_split[1], "*", star_split[2]))
        star_idx <- star_idx + 1
        e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                         e_df$group2[i], e_df$group3[i],
                                         paste0(star_split[1], "*", star_split[3]))
        star_idx <- star_idx + 1
        e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                         e_df$group2[i], e_df$group3[i],
                                         paste0(star_split[2], "*", star_split[3]))
        star_idx <- star_idx + 1
      } else if (length(star_split) >= 4) {
        cli::cli_abort(c(
          "This function only works for 3 interactions specified using '*'."
        ))
      }
      e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                       e_df$group2[i], e_df$group3[i], e_df$raw_term[i])
    } else {
      e_df_star[(star_idx + i), ] <- c(e_df$effect[i], e_df$group[i],
                                       e_df$group2[i], e_df$group3[i], e_df$raw_term[i])
    }
  }
  e_df_star <- unique(e_df_star[which(e_df_star$raw_term != ""), ])

  e_df_star$cat <- ""
  e_df_star$cat_levels <- ""
  e_df_star$cat2 <- ""
  e_df_star$cat_levels2 <- ""
  e_df_star$cat3 <- ""
  e_df_star$cat_levels3 <- ""
  if (!is.null(cat_vars)) {
    if (is.null(cat_vars_nlevels)) {
      cli::cli_abort(c(
        "Can't find 'cat_vars_nlevels'.",
        "i" = "To use 'cat_vars', please specify 'cat_vars_nlevels'."
      ))
    }
    if (length(cat_vars) != length(cat_vars_nlevels)) {
      cli::cli_abort(c(
        "'cat_vars' must be the same length as 'cat_vars_nlevels'."
      ))
    }
    for (i in 1:length(cat_vars)) {
      if (!cat_vars_nlevels[i] %in% c(2:10)) {
        cli::cli_abort(c(
          "Check 'cat_vars_nlevels'."
        ))
      }
      for (j in 1:nrow(e_df_star)) {
        if (stringr::str_detect(e_df_star$raw_term[j], cat_vars[i])) {
          if (e_df_star$cat[j] == "") {
            e_df_star$cat[j] <- cat_vars[i]
            e_df_star$cat_levels[j] <- cat_vars_nlevels[i]
          } else if (e_df_star$cat2[j] == "") {
            e_df_star$cat2[j] <- cat_vars[i]
            e_df_star$cat_levels2[j] <- cat_vars_nlevels[i]
          } else if (e_df_star$cat3[j] == "") {
            e_df_star$cat3[j] <- cat_vars[i]
            e_df_star$cat_levels3[j] <- cat_vars_nlevels[i]
          } else {
            cli::cli_abort(c(
              "Can't have more than a three-way interaction with 'cat_vars'."
            ))
          }
        }
      }
    }
  }

  e_df_cat <- data.frame(effect = rep("", 500),
                         group = "", group2 = "", group3 = "",
                         raw_term = "",
                         updated_term = "",
                         ref_level = "")
  if (!is.null(cat_vars)) {
    cat_idx <- 0
    for (j in 1:nrow(e_df_star)) {
      if (e_df_star$cat[j] != "") {
        cat <- e_df_star$cat[j]
        cat_levels <- LETTERS[c(1:e_df_star$cat_levels[j])]
        save_idx <- c(rep(0, 10))
        for (k in 1:length(cat_levels)) {
          rlevel <- paste0(cat, cat_levels[1])
          rl <- stringr::str_replace(e_df_star$raw_term[j], cat, rlevel)
          klevel <- paste0(cat, cat_levels[k])
          ut <- stringr::str_replace(e_df_star$raw_term[j], cat,klevel)
          e_df_cat[(cat_idx + j), ] <- c(e_df_star$effect[j], e_df_star$group[j],
                                      e_df_star$group2[j], e_df_star$group3[j],
                                      e_df_star$raw_term[j], ut, rl)
          save_idx[k] <- cat_idx + j
          cat_idx <- cat_idx + 1
        }
        cat_idx <- cat_idx - 1

        if (e_df_star$cat2[j] != "") {
          save_idx <- save_idx[which(save_idx != 0)]
          save_df <- e_df_cat[save_idx, ]
          cat2 <- e_df_star$cat2[j]
          cat_levels2 <- LETTERS[c(1:e_df_star$cat_levels2[j])]
          save_idx_update <- save_idx
          for (k in 2:length(cat_levels2)) {
            e_df_cat[save_idx_update, ] <- save_df[c(1:length(cat_levels)), ]
            klevel2 <- paste0(cat2, cat_levels2[k])
            e_df_cat$updated_term[save_idx_update] <-
              stringr::str_replace(e_df_cat$updated_term[save_idx_update],
                                   cat2, klevel2)
            e_df_cat$ref_level[save_idx_update] <-
              stringr::str_replace(e_df_cat$ref_level[save_idx_update],
                                   cat2, klevel2)
            save_idx_update <- save_idx_update + length(cat_levels)
          }
          save_idx_update <- save_idx_update - length(cat_levels)
          cat_idx <- max(save_idx_update) - j
        }

        if (e_df_star$cat3[j] != "") {
          save_idx2 <- c(min(save_idx):max(save_idx_update))
          save_df2 <- e_df_cat[save_idx2, ]
          cat3 <- e_df_star$cat3[j]
          cat_levels3 <- LETTERS[c(1:e_df_star$cat_levels3[j])]
          save_idx_update3 <- save_idx2
          for (k in 2:length(cat_levels3)) {
            e_df_cat[save_idx_update3, ] <- save_df2[c(1:nrow(save_df2)), ]
            klevel3 <- paste0(cat3, cat_levels3[k])
            e_df_cat$updated_term[save_idx_update3] <-
              stringr::str_replace(e_df_cat$updated_term[save_idx_update3],
                                   cat3, klevel3)
            e_df_cat$ref_level[save_idx_update3] <-
              stringr::str_replace(e_df_cat$ref_level[save_idx_update3],
                                   cat3, klevel3)
            save_idx_update3 <- save_idx_update3 + nrow(save_df2)
          }
          save_idx_update3 <- save_idx_update3 - nrow(save_df2)
          cat_idx <- max(save_idx_update3) - j
        }


      } else {
        e_df_cat[(cat_idx + j), ] <- c(e_df_star$effect[j], e_df_star$group[j],
                                    e_df_star$group2[j], e_df_star$group3[j],
                                    e_df_star$raw_term[j], e_df_star$raw_term[j], "")
      }
    }
  } else {
    e_df_cat <- e_df_star
    e_df_cat$updated_term <- e_df_cat$raw_term
    e_df_cat$ref_level <- ""
  }
  e_df_cat <- e_df_cat[which(e_df_cat$effect != ""), ]
  e_df_cat <- e_df_cat[,c("effect", "group", "group2", "group3",
                          "raw_term", "updated_term", "ref_level")]

  all_intercepts <- c(fixed_intercept, gf_intercept)
  gf_all <- unique(e_df_cat$group3)
  e_df_cat$intercept <- ""
  for (i in 1:length(gf_all)) {
    if (all_intercepts[i] == "yes" || all_intercepts[i] == "added") {
      e_df_cat$intercept[which(e_df_cat$group3 == gf_all[i])] <- "yes"
    } else {
      e_df_cat$intercept[which(e_df_cat$group3 == gf_all[i])] <- "no"
    }
  }
  e_df_cat$rm_rows <- c(rep(0, nrow(e_df_cat)))
  if (!is.null(cat_vars)) {
    all_refs <- unique(e_df_cat$ref_level)
    all_refs <- all_refs[which(all_refs != "")]
    for (i in 1:nrow(e_df_cat)) {
      for (j in 1:length(all_refs)) {
        if (e_df_cat$updated_term[i] == all_refs[j] && e_df_cat$intercept[i] == "yes") {
          e_df_cat$rm_rows[i] <- 1
        }
      }
    }
  }
  e_df_cat <- e_df_cat[which(e_df_cat$rm_rows == 0), ]
  e_df_cat <- e_df_cat[,c("effect", "group", "group2", "group3",
                          "raw_term", "updated_term", "ref_level")]

  gf_all <- gf_all[which(gf_all != "")]
  df_varcov_list <- re_df_list
  for (i in 1:length(gf_all)) {
    df_with <- e_df_cat[which(e_df_cat$group3 == gf_all[i]), ]
    df_without <- df_with[which(df_with$raw_term != "||"), ]
    n_re[i] <- nrow(df_without)
    df_row_diff <- nrow(df_with) - nrow(df_without)
    if (n_re[i] >= 5) {
      cli::cli_abort(c(
        "This function only works for 1 to 4 random effects per grouping factor."
      ))
    } else {
      df_varcov <- data.frame(variable = "",
                              m1 = c(11, 21, 31, 41),
                              m2 = c(12, 22, 32, 42),
                              m3 = c(13, 23, 33, 43),
                              m4 = c(14, 24, 34, 44))
      df_varcov$variable[1:nrow(df_without)] <- df_without$updated_term
      if (df_row_diff >= 1) {
        idx_bar <- which(df_with$raw_term == "||")
        if (idx_bar == 2) {
          df_varcov[2:4, 2] <- 0
          df_varcov[1, 3:5] <- 0
        }
        if (idx_bar == 3) {
          df_varcov[3:4, 2] <- 0
          df_varcov[1, 4:5] <- 0
          df_varcov[3:4, 3] <- 0
          df_varcov[2, 4:5] <- 0
        }
        if (idx_bar == 4) {
          df_varcov[1:3, 5] <- 0
          df_varcov[4, 2:4] <- 0
        }
      } else {
        if (gf_re_corr[i] == "no") {
          df_varcov$m1[which(df_varcov$m1 != 11)] <- 0
          df_varcov$m2[which(df_varcov$m2 != 22)] <- 0
          df_varcov$m3[which(df_varcov$m3 != 33)] <- 0
          df_varcov$m4[which(df_varcov$m4 != 44)] <- 0
        }
      }
      df_varcov <- df_varcov[which(df_varcov$variable != ""), ]
      df_varcov_list[[i]] <- df_varcov
    }
  }
  vt_out <- e_df_cat[which(e_df_cat$raw_term != "||"), ]

  return(list(vt_out, y, n_gf, gf_description,
              gf_cross, gf_nest, fixed_intercept, n_re,
              gf_intercept, gf_re_corr, gf_all, df_varcov_list,
              random_contains_list, random_df, bar_where_int,
              fixed, random, model_call))
}

# All functions require information about the random effects
extract_gf <- function(model,
                       cat_vars = NULL,
                       cat_vars_nlevels = NULL) {

  model <- as.character(model)
  model <- stringr::str_remove_all(model, " ")
  m <- stringr::str_split_fixed(model, stringr::fixed("("), 2)
  model_call <- m[1, 1]
  m[1, 2] <- stringr::str_sub(m[1, 2], end = -2)

  if (stringr::str_detect(m[1, 2], stringr::fixed(","))) {
    mod_split <- stringr::str_split_fixed(m[1, 2], stringr::fixed(","), 2)
    model_eq <- mod_split[1, 1]
    model_inputs <- mod_split[1, 2]
  } else {
    model_eq <- m[1, 2]
    model_inputs <- ""
  }

  # Define fixed and random --------------
  fixed <- ""
  random <- ""
  random_contains_list <- "no"
  if (model_call == "lme") {
    fixed <- model_eq
    fixed <- stringr::str_remove(fixed, stringr::fixed("fixed="))
    check_random_lme <- stringr::str_detect(model_inputs, "random=")
    if (check_random_lme == FALSE) {
      cli::cli_abort(c(
        "Can't find random effects.",
        "i" = "For a lme() model, use 'random=' to specify the random effects."
      ))
    } else {
      if (stringr::str_detect(model_inputs, stringr::fixed("random=list"))) {
        random_contains_list <- "yes"
        random_lme_split <- stringr::str_split_fixed(model_inputs, stringr::fixed("random="), 2)
        idx_closed <- unlist(gregexpr(')', random_lme_split[1, 2], fixed = TRUE))
        idx_open <- unlist(gregexpr('(', random_lme_split[1, 2], fixed = TRUE))
        if (length(idx_open) == 1) {
          random_lme_split2 <- stringr::str_split_fixed(random_lme_split[1, 2], stringr::fixed(")"), 2)
          random <- paste0(random_lme_split2[1, 1], ")")
        } else {
          idx_df <- data.frame(idx = c(idx_open, idx_closed),
                               type = c(rep("open", length(idx_open)),
                                        rep("closed", length(idx_closed))),
                               type2 = "")
          idx_df <- idx_df[order(idx_df$idx), ]
          idx_final <- 0
          while (idx_final == 0) {
            if (nrow(idx_df) == 1) {
              cli::cli_abort(c(
                "Double check parentheses in model input."
              ))
            }
            for (i in 2:nrow(idx_df)) {
              if (idx_df$type[i - 1] == "open" && idx_df$type[i] == "closed") {
                idx_df$type2[i - 1] <- "pair"
                idx_df$type2[i] <- "pair"
              }
            }
            if (idx_df$type2[1] == "pair") {
              idx_final <- idx_df$idx[2]
            } else {
              idx_df <- idx_df[idx_df$type2 != "pair", ]
            }
          }
          random <- stringr::str_sub(random_lme_split[1, 2], 1, idx_final)
        }
      } else {
        model_inputs_split <- stringr::str_split_fixed(model_inputs, stringr::fixed(","), 7)
        find_random_lme <- stringr::str_detect(model_inputs_split, "random=")
        random <- model_inputs_split[1, which(find_random_lme)]
        random <- stringr::str_remove(random, stringr::fixed("random="))
      }
    }
    rm(check_random_lme)

  } else if (model_call == "lmer") {
    model_eq <- stringr::str_remove(model_eq, stringr::fixed("formula="))
    if (!stringr::str_detect(model_eq, stringr::fixed("|"))) {
      cli::cli_abort(c(
        "Can't find random effects.",
        "i" = "For a lmer() model, specify random effects in the formula input."
      ))
    }
    lmer_split <- stringr::str_split_fixed(model_eq, stringr::fixed("|"), 7)
    lmer_split <- data.frame(lmer_split)
    if (lmer_split[1, 7] == "") {
      lmer_split <- lmer_split[, -7]
      if (lmer_split[1, 6] == "") {
        lmer_split <- lmer_split[, -6]
        if (lmer_split[1, 5] == "") {
          lmer_split <- lmer_split[, -5]
          if (lmer_split[1, 4] == "") {
            lmer_split <- lmer_split[, -4]
            if (lmer_split[1, 3] == "") {
              lmer_split <- lmer_split[, -3]
            }
          }
        }
      }
    }
    get_fixed <- lmer_split[1, 1]
    get_fixed_split <- stringr::str_split_fixed(get_fixed, stringr::fixed("+("), 2)
    fixed <- get_fixed_split[1, 1]
    lmer_split[1, 1] <- paste0("(", get_fixed_split[1, 2])
    random <- lmer_split[1, 1]
    for (j in 2:ncol(lmer_split)) {
      get_random <- lmer_split[1, j]
      if (get_random == "") {
        random <- paste0(random, "||")
      } else {
        random <- paste0(random, "|", get_random)
      }
    }
    random <- stringr::str_replace_all(random, stringr::fixed("|||"), stringr::fixed("||"))
  }
  fixed <- stringr::str_remove_all(as.character(fixed), " ")
  random <- stringr::str_remove_all(as.character(random), " ")
  model_eq <- fixed

  # Define re, add re to model_eq, and start random_df --------------
  random_df <- data.frame(c1 = rep("", 50), c2 = "", c3 = "")
  random_df_skip <- 0
  if (random_contains_list == "yes") {
    random_lme_split <- stringr::str_split_fixed(random, stringr::fixed("list("), 2)
    random_lme <- stringr::str_sub(random_lme_split[1, 2], end = -2)
    n_equals <- stringr::str_count(random_lme, stringr::fixed("="))
    if (n_equals == 0) {
      cli::cli_abort(c(
        "Double check random effects notation in model input."
      ))
    }
    if (n_equals >= 4) {
      cli::cli_abort(c(
        "Can't run function with more than 3 grouping factors."
      ))
    }
    random_lme <- stringr::str_replace_all(random_lme, stringr::fixed(",~"), stringr::fixed(";~"))
    r_lme_split <- stringr::str_split_fixed(random_lme, stringr::fixed(","), n_equals)
    re <- ""
    re_right <- ""
    for (i in 1:n_equals) {
      r_lme_split[1, i] <- stringr::str_replace_all(r_lme_split[1, i], stringr::fixed(";"), stringr::fixed(","))
      if (stringr::str_detect(r_lme_split[1, i], stringr::fixed("=~"))) {
        r_lme_split2 <- stringr::str_split_fixed(r_lme_split[1, i], stringr::fixed("=~"), 2)
        re_right <- paste0(re_right, ":", r_lme_split2[1, 1])
        re <- paste0(re, "+(", r_lme_split2[1, 2], "|", re_right, ")")
        random_df[(i + random_df_skip), ] <- c(r_lme_split2[1, 2], "|", stringr::str_sub(re_right, start = 2))
      }
      if (stringr::str_detect(r_lme_split[1, i], stringr::fixed("pdDiag"))) {
        r_lme_split[1, i] <- stringr::str_remove(r_lme_split[1, i], stringr::fixed("pdDiag("))
        r_lme_split[1, i] <- stringr::str_sub(r_lme_split[1, i], end = -2)
        r_lme_split2 <- stringr::str_split_fixed(r_lme_split[1, i], stringr::fixed("=~"), 2)
        re_right <- paste0(re_right, ":", r_lme_split2[1, 1])
        re <- paste0(re, "+(", r_lme_split2[1, 2], "||", re_right, ")")
        random_df[(i + random_df_skip), ] <- c(r_lme_split2[1, 2], "||", stringr::str_sub(re_right, start = 2))
      }
      if (stringr::str_detect(r_lme_split[1, i], stringr::fixed("pdBlocked"))) {
        r_lme_split[1, i] <- stringr::str_remove(r_lme_split[1, i], stringr::fixed("pdBlocked(list("))
        r_lme_split[1, i] <- stringr::str_sub(r_lme_split[1, i], end = -3)
        corr_split <- stringr::str_split_fixed(r_lme_split[1, i], stringr::fixed("="), 2)
        n_comma <- stringr::str_count(corr_split[1, 2], stringr::fixed(","))
        corr_split2 <- stringr::str_split_fixed(corr_split[1, 2], ",", (n_comma + 1))
        re_right <- paste0(re_right, ":", corr_split[1, 1])
        for (j in 1:(n_comma + 1)) {
          r_lme_split_sub <- paste(corr_split[1, 1],corr_split2[1, j], sep = "=")
          r_lme_split2 <- stringr::str_split_fixed(r_lme_split_sub, stringr::fixed("=~"), 2)
          re <- paste0(re, "+(", r_lme_split2[1, 2], "|", re_right, ")")
          random_df[(i + random_df_skip + j - 1), ] <-
            c(r_lme_split2[1, 2], "|", stringr::str_sub(re_right, start = 2))
        }
        random_df_skip <- n_comma
      }
    }
    re <- gsub(pattern = "|:", replacement = "|", x = re, fixed = TRUE)
    model_eq <- paste0(model_eq, re)
    re <- stringr::str_sub(re, start = 2)

  } else {
    r_lme_split <- stringr::str_split_fixed(random, stringr::fixed("~"), 2)
    if (r_lme_split[1, 1] == "pdDiag(" && model_call == "lme") {
      r_lme_split[1, 2] <- stringr::str_sub(r_lme_split[1, 2], end = -2)
      re <- paste0("(", r_lme_split[1, 2], "||AddGF)")
      random_df[1, ] <- c(r_lme_split[1, 2], "||", "AddGF")
      model_eq <- paste(model_eq, re, sep = "+")
    } else {
      if (!stringr::str_detect(r_lme_split[1, 2], stringr::fixed("|")) && model_call == "lme") {
        re <- paste0("(", r_lme_split[1, 2], "|AddGF)")
        random_df[1, ] <- c(r_lme_split[1, 2], "|", "AddGF")
        model_eq <- paste(model_eq, re, sep = "+")
      } else {
        if (model_call == "lme") {
          re <- paste0("(", r_lme_split[1, 2], ")")
        } else {
          re <- random
        }
        n_rparts <- stringr::str_count(re, stringr::fixed(")+("))
        re_sep <- stringr::str_sub(re, 2, -2)
        re_sep2 <- stringr::str_split_fixed(re_sep, stringr::fixed(")+("), (n_rparts + 2))
        re2 <- ""
        for (k in 1:(n_rparts + 1)) {
          if (stringr::str_detect(re_sep2[1, k], stringr::fixed("||"))) {
            re_sep3 <- stringr::str_split_fixed(re_sep2[1, k], stringr::fixed("||"), 2)
            random_df[(k + random_df_skip), ] <- c(re_sep3[1, 1], "||", re_sep3[1, 2])
          } else {
            re_sep3 <- stringr::str_split_fixed(re_sep2[1, k], stringr::fixed("|"), 2)
            random_df[(k + random_df_skip), ] <- c(re_sep3[1, 1], "|", re_sep3[1, 2])
          }
          n_slash <- stringr::str_count(re_sep3[1, 2], stringr::fixed("/"))
          if (n_slash >= 1) {
            re_sep3_slash <- stringr::str_split_fixed(re_sep3[1, 2], stringr::fixed("/"), (n_slash + 1))
            re_right <- ""
            for (l in 1:(n_slash + 1)) {
              re_right <- paste(re_right, re_sep3_slash[1, l], sep = ":")
              random_df[(k + random_df_skip + l - 1), ] <- c(random_df[(k + random_df_skip), 1],
                                                             random_df[(k + random_df_skip), 2],
                                                             stringr::str_sub(re_right, start = 2))
              re2 <- paste0(re2, "+(",
                            random_df[(k + random_df_skip), 1],
                            random_df[(k + random_df_skip), 2],
                            stringr::str_sub(re_right, start = 2), ")")
            }
            random_df_skip <- n_slash
          } else {
            re2 <- paste0(re2, "+(", re_sep2[1, k], ")")
          }
        }
        model_eq <- paste0(model_eq, re2)
        re <- stringr::str_sub(re2, start = 2)
      }
    }
  }

  # Combine rows with the same GF
  model_eq <- stringr::str_remove_all(model_eq, " ")
  re <- stringr::str_remove_all(re, " ")
  random_df <- random_df[!duplicated(random_df), ]
  random_df <- random_df[1:(nrow(random_df) - 1), ]
  check_dups <- nrow(random_df) - length(unique(random_df$c3))
  if (check_dups >= 1) {
    check_c3 <- unique(random_df$c3)
    for (i in 1:length(check_c3)) {
      find_dups <- random_df[which(random_df$c3 == check_c3[i]), ]
      if (nrow(find_dups) >= 2) {
        update_c1 <- ""
        for (j in 1:nrow(find_dups)) {
          update_c1 <- paste(update_c1, find_dups$c1[j], sep = "+||+")
        }
        random_df[which(random_df$c3 == check_c3[i]), 1] <- stringr::str_sub(update_c1, start = 5)
      }
    }
  }

  random_df <- unique(random_df)
  c3_split <- stringr::str_split_fixed(random_df$c3, stringr::fixed(":"), 3)
  random_df$random <- c3_split[, 1]
  random_df$nested1 <- c3_split[, 2]
  random_df$nested2 <- c3_split[, 3]
  random_df$name <- ""
  for (i in 1:nrow(random_df)) {
    if (stringr::str_detect(random_df$c3[i], stringr::fixed(":"))) {
      if (nrow(random_df) == 1) {
        c3_new <- stringr::str_remove(random_df$c3[i], stringr::fixed(":"))
        model_eq <- stringr::str_replace_all(model_eq, random_df$c3[i], c3_new)
        re <- stringr::str_replace_all(re, random_df$c3[i], c3_new)
        random_df$c3[i] <- c3_new
        random_df$random[i] <- random_df$c3[i]
        random_df$nested1[i] <- ""
        random_df$nested2[i] <- ""
        random_df$name[i] <- random_df$c3[i]
      } else {
        if (!random_df$random[i] %in% random_df$c3[-i]) {
          c3_new <- stringr::str_remove(random_df$c3[i], stringr::fixed(":"))
          model_eq <- stringr::str_replace_all(model_eq, random_df$c3[i], c3_new)
          re <- stringr::str_replace_all(re, random_df$c3[i], c3_new)
          random_df$c3[i] <- c3_new
          random_df$random[i] <- random_df$c3[i]
          random_df$nested1[i] <- ""
          random_df$nested2[i] <- ""
          random_df$name[i] <- random_df$c3[i]
        } else {
          random_df$name[i] <- stringr::str_replace_all(random_df$c3[i], stringr::fixed(":"), stringr::fixed("_"))
        }
      }
    } else {
      random_df$name[i] <- random_df$c3[i]
    }
  }
  random_df <- unique(random_df)
  random_df_right <- unique(random_df[, 3:7])

  n_gf <- nrow(random_df_right)
  if (n_gf >= 4) {
    cli::cli_abort(c(
      "Can't run function with more than 3 grouping factors."
    ))
  }
  gf_description <- ""
  gf_nest <- c("", "", "")
  gf_cross <- c("", "", "")
  if (n_gf == 3) {
    n_nested2 <- 3 - length(which(random_df_right$nested2 == ""))
    n_nested1 <- 3 - length(which(random_df_right$nested1 == ""))
    if(n_nested2 == 1) {
      row_nest <- which(random_df_right$nested2 != "")
      gf_nest <- random_df_right[row_nest, c("random", "nested1", "nested2")]
      gf_description <- "nested"
    } else {
      if (n_nested1 == 0) {
        gf_cross <- random_df_right$random
        gf_description <- "crossed"
      }
      if (n_nested1 == 1) {
        gf_description <- "crossed with nested"
        row_nest <- which(random_df_right$nested1 != "")
        gf_nest[1:2] <- random_df_right[row_nest, c("random", "nested1")]
        gf_cross[1] <- random_df_right$random[which(random_df_right$random != gf_nest[1])]
      }
      if (n_nested1 == 2) {
        gf_description <- "crossed within nested"
        gf_nest[1] <- random_df_right$random[1]
        gf_cross[1:2] <- random_df_right$nested1[which(random_df_right$nested1 != "")]
      }
    }
  }
  if (n_gf == 2) {
    n_nested1 <- 2 - length(which(random_df_right$nested1 == ""))
    if(n_nested1 == 1) {
      row_nest <- which(random_df_right$nested1 != "")
      gf_nest[1:2] <- random_df_right[row_nest, c("random", "nested1")]
      gf_description <- "nested"
    } else {
      gf_cross[1:2] <- random_df_right$random
      gf_description <- "crossed"
    }
  }
  if (n_gf == 1) {
    gf_cross[1] <- random_df_right$random
  }

  if (gf_description == "nested") {
    random_df <- random_df[with(random_df, order(c3)), ]
  }
  if (gf_description == "crossed with nested") {
    random_df$order <- 3
    random_df$order[which(random_df$random == gf_cross[1])] <- 1
    random_df$order[which(random_df$random == gf_nest[1])] <- 2
    random_df <- random_df[with(random_df, order(order)), ]
    random_df <- random_df[, 1:7]
  }
  if (gf_description == "crossed within nested") {
    random_df$order <- 3
    random_df$order[which(random_df$random == gf_nest[1])] <- 1
    random_df$order[which(random_df$nested1 == gf_cross[1])] <- 2
    random_df <- random_df[with(random_df, order(order)), ]
    random_df <- random_df[, 1:7]
  }
  random_df$name2 <- random_df$name
  for (i in 1:nrow(random_df)) {
    if (random_df$nested2[i] != "") {
      random_df$name[i] <- paste0(random_df$nested2[i], " (within ", random_df$nested1[i], " and ", random_df$random[i], ")")
      random_df$name2[i] <- random_df$nested2[i]
    } else {
      if (random_df$nested1[i] != "") {
        random_df$name[i] <- paste0(random_df$nested1[i], " (within ", random_df$random[i], ")")
        random_df$name2[i] <- random_df$nested1[i]
      }
    }
  }

  return(list(re, model_eq, random_df, n_gf,
              gf_description, gf_cross, gf_nest, random_contains_list,
              fixed, random, model_call))
}

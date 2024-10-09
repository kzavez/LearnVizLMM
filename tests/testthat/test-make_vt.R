
expect_error(make_vt("lme(Y = 1, random=list(School=~1,Class=~1), data)"),
             "Can't separate outcome from fixed effects.")
expect_error(make_vt("lmer(Y + 1+Time+(Time|ID), data)"),
             "Can't separate outcome from fixed effects.")

test_that("fixed_intercept is correct", {
  expect_equal(make_vt("lme(Y ~ 1, random=~1|GF)")[[7]], "yes")
  expect_equal(make_vt("lme(Y ~ Time, random=~1|GF)")[[7]], "yes")
  expect_equal(make_vt("lme(Y ~ 1 + Time -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ 1 + Time + -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ Time -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ Time + -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ 1 + Time +I(Time-10), random=~1|GF)")[[7]], "yes")
  expect_equal(make_vt("lme(Y ~ Time +I(Time-10), random=~1|GF)")[[7]], "yes")
  expect_equal(make_vt("lme(Y ~ 1 + Time +I(Time-10) -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ 1 + Time +I(Time-10) + -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ Time +I(Time-10) -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ Time +I(Time-10) + -1, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ Time + 0, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ 1 + Time + 0, random=~1|GF)")[[7]], "no")
  expect_equal(make_vt("lme(Y ~ 1 + Time +I(Time+0), random=~1|GF)")[[7]], "yes")
  expect_equal(make_vt("lme(Y ~ 1 + Time +I(Time+0) + 0, random=~1|GF)")[[7]], "no")
})
#> Test passed

expect_error(make_vt("lmer(Y ~ 1+Time+(1|ID)+(Time|ID), data)"),
             "Only one random intercept should be specified per grouping factor.")
expect_error(make_vt("lmer(Y ~ 1+Time+(1|ID)+(1+Time|ID), data)"),
             "Only one random intercept should be specified per grouping factor.")

test_that("random_df$c1 for dups is correct", {
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(0+Time|ID), data)")[[14]][1,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(Time-1|ID), data)")[[14]][1,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time-1|ID)+(1|ID), data)")[[14]][1,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(Time-1|ID)+(1|GF), data)")[[14]][1,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID1)+(Time-1|ID1)+(1|ID2)+(Time-1|ID2), data)")[[14]][1,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID1)+(Time-1|ID1)+(1|ID2)+(Time-1|ID2), data)")[[14]][2,1], "1+||+Time")
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)-1| ID), data)")[[14]][1,1], "1+||+Time+I(Time^2)")
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)+0| ID), data)")[[14]][1,1], "1+||+Time+I(Time^2)")
  expect_equal(make_vt("lme(Y ~ 1 + Time + I(Time^2), random=list(ID=pdBlocked(list(~1,~Time+I(Time^2)-1))))")[[14]][1,1], "1+||+Time+I(Time^2)")
})
#> Test passed


test_that("gf_intercept is correct", {
  expect_equal(make_vt("lmer(Y ~ 1+(1|GF), data)")[[9]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(0+Time|ID), data)")[[9]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time+0|ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1+Time-1|ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1+Time+-1|ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time-1|ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time+-1|ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1+Time+I(Time-10)| ID), data)")[[9]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(Time+I(Time-10)| ID), data)")[[9]], c("added", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(Time+I(Time-10)-1| ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(Time+I(Time-10)+-1| ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(0+Time+I(Time^2)|| ID), data)")[[9]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)-1| ID), data)")[[9]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+(1|GF1)+(wonder|GF2)+(1|GF3), data)")[[9]], c("yes", "added", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+(1|District/School/Class), data)")[[9]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ FE+(FE|School/Class), data)")[[9]], c("added", "added", "yes"))
  expect_equal(make_vt("lmer(Y ~ FE+(-1+FE||School/Class), data)")[[9]], c("no", "no", "yes"))
})
#> Test passed


test_that("gf_re_corr is correct", {
  expect_equal(make_vt("lmer(Y ~ 1+(1|GF), data)")[[10]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(0+Time|ID), data)")[[10]], c("complex", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time+0|ID), data)")[[10]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time|ID), data)")[[10]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time||ID), data)")[[10]], c("no", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)-1| ID), data)")[[10]], c("complex", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ FE+(FE|School/Class), data)")[[10]], c("yes", "yes", "yes"))
  expect_equal(make_vt("lmer(Y ~ 1+(1+FE||GF1)+(wonder-1|GF2)+(1|GF2)+(1|GF3), data)")[[10]], c("no", "complex", "yes"))
  expect_equal(make_vt("lmer(Y ~ FE+(1+FE||School/Class), data)")[[10]], c("no", "no", "yes"))
})
#> Test passed

expect_error(make_vt("lmer(Y ~ 1+Time+(1+Time+I(Time^2)+Age+Sex|ID), data)"),
             "This function only works for 1 to 4 random effects per grouping factor.")

test_that("n_re is correct", {
  expect_equal(make_vt("lmer(Y ~ 1+(1|GF), data)")[[8]], c(1, 0, 0))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(1|ID)+(0+Time|ID), data)")[[8]], c(2, 0, 0))
  expect_equal(make_vt("lmer(Y ~ 1+Time+(Time+I(Time^2)|ID), data)")[[8]], c(3, 0, 0))
  expect_equal(make_vt("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)-1| ID), data)")[[8]], c(3, 0, 0))
  expect_equal(make_vt("lmer(Y ~ FE+(FE|School/Class), data)")[[8]], c(2, 2, 0))
  expect_equal(make_vt("lmer(Y ~ 1+(1+FE||GF1)+(wonder-1|GF2)+(1|GF2)+(1|GF3), data)")[[8]], c(2, 2, 1))
  expect_equal(make_vt("lmer(Y ~ FE+(1+FE*Age|School/Class), data)")[[8]], c(4, 4, 0))
  expect_equal(make_vt("lmer(Y ~ FE*Sex+(FE*Sex|School/Class), data)",
                       cat_vars = "Sex", cat_vars_nlevels = 2)[[8]], c(4, 4, 0))
})
#> Test passed

expect_error(make_vt("lmer(Y ~ 1+time*age*sex*weight+(1|ID), data)"),
             "This function only works for 3 interactions specified using '*'.")

test_that("interactions in vt_out are correct", {
  expect_equal(make_vt("lmer(Y~1+time+sex+time:sex+(1|GF), data)")[[1]][,6],
               c("1", "time", "sex", "time:sex", "1"))
  expect_equal(make_vt("lmer(Y~1+time+sex+time:sex+(1|GF), data)",
                       cat_vars = "sex",
                       cat_vars_nlevels = 2)[[1]][,6],
               c("1", "time", "sexB", "time:sexB", "1"))
  expect_equal(make_vt("lmer(Y~1+time+sex+time:sex+(1|GF), data)",
                       cat_vars = "sex",
                       cat_vars_nlevels = 3)[[1]][,6],
               c("1", "time", "sexB", "sexC", "time:sexB", "time:sexC", "1"))
  expect_equal(make_vt("lmer(Y~1+time+time:sex+(1|GF), data)")[[1]][,6],
               c("1", "time", "time:sex", "1"))
  expect_equal(make_vt("lmer(Y~+sex+time*sex+(1|GF), data)")[[1]][,6],
               c("1", "sex", "time", "time*sex", "1"))
  expect_equal(make_vt("lmer(Y~1+time*sex*age+(1|GF), data)")[[1]][,6],
               c("1", "time", "sex", "age", "time*sex", "time*age",
                 "sex*age", "time*sex*age", "1"))
  expect_equal(
    make_vt("lmer(Y~1+time+sex+age+time:sex+time:age+age:sex+time:sex:age+(1|GF))")[[1]][,6],
               c("1", "time", "sex", "age", "time:sex",
                 "time:age", "age:sex", "time:sex:age", "1"))
  expect_equal(
    make_vt("lmer(Y~time+sex+age+wgt+time:sex+time:age+time:wgt+age:sex+age:wgt+sex:wgt+time:sex:age+time:sex:wgt+time:wgt:age+wgt:sex:age+time:sex:age:wgt+(1|GF))")[[1]][,6],
    c("1", "time", "sex", "age", "wgt", "time:sex",
      "time:age", "time:wgt", "age:sex", "age:wgt", "sex:wgt",
      "time:sex:age", "time:sex:wgt", "time:wgt:age", "wgt:sex:age",
      "time:sex:age:wgt", "1"))
})
#> Test passed

expect_error(make_vt("lmer(Y~time+sex+age+wgt+time:sex+time:age+time:wgt+age:sex+age:wgt+sex:wgt+time:sex:age+time:sex:wgt+time:wgt:age+wgt:sex:age+time:sex:age:wgt+(1|GF))",
                     cat_vars = c("time", "sex", "age", "wgt"),
                     cat_vars_nlevels = c(3, 2, 3, 2)),
             "Can't have more than a three-way interaction with 'cat_vars'.")
expect_error(make_vt("lmer(Y~weight+age+group+(1+sex|GF), data)",
                     cat_vars = c("sex"),
                     cat_vars_nlevels = c(4, 2)),
             "'cat_vars' must be the same length as 'cat_vars_nlevels'.")
expect_error(make_vt("lmer(Y~weight+age+group+(1+sex|GF), data)",
                     cat_vars = c("group", "sex"),
                     cat_vars_nlevels = c(2)),
             "'cat_vars' must be the same length as 'cat_vars_nlevels'.")
expect_error(make_vt("lmer(Y~weight+age+group+(1+sex|GF), data)",
                     cat_vars = c("group", "sex")),
             "To use 'cat_vars', please specify 'cat_vars_nlevels'.")
expect_error(make_vt("lmer(Y~weight+age+group+(1+sex|GF), data)",
                     cat_vars = c("group", "sex"),
                     cat_vars_nlevels = c(2, 12)),
             "Check 'cat_vars_nlevels'.")


test_that("categorical variables in vt_out are correct", {
  expect_equal(make_vt("lmer(Y~sex+(1|GF), data)",
                       cat_vars = "sex",
                       cat_vars_nlevels = 2)[[1]][,6],
               c("1", "sexB", "1"))
  expect_equal(make_vt("lmer(Y~sex+(1+sex|GF), data)",
                       cat_vars = "sex",
                       cat_vars_nlevels = 3)[[1]][,6],
               c("1", "sexB", "sexC", "1", "sexB", "sexC"))
  expect_equal(make_vt("lmer(Y~weight+age+group+(1+sex|GF), data)",
                       cat_vars = c("group", "sex"),
                       cat_vars_nlevels = c(4, 2))[[1]][,6],
               c("1", "weight", "age", "groupB", "groupC", "groupD",
                 "1", "sexB"))
  expect_equal(make_vt("lmer(Y~weight+age*group+(sex-1|GF1)+(1|GF2), data)",
                       cat_vars = c("group", "sex"),
                       cat_vars_nlevels = c(3, 2))[[1]][,6],
               c("1", "weight", "age", "groupB", "groupC", "age*groupB",
                 "age*groupC", "sexA", "sexB", "1"))
  expect_equal(make_vt("lmer(Y~weight+age*group+(sex-1|GF1)+(1|GF2), data)",
                       cat_vars = c("group", "sex", "age"),
                       cat_vars_nlevels = c(3, 2, 2))[[1]][,6],
               c("1", "weight", "ageB", "groupB", "groupC", "ageB*groupB",
                 "ageB*groupC", "sexA", "sexB", "1"))
  expect_equal(make_vt("lmer(Y~weight+age*group*sex+(1|GF1)+(1|GF2), data)",
                       cat_vars = c("group", "sex", "age"),
                       cat_vars_nlevels = c(3, 2, 2))[[1]][,6],
               c("1", "weight", "ageB", "groupB", "groupC", "sexB", "ageB*groupB",
                 "ageB*groupC", "ageB*sexB", "groupB*sexB", "groupC*sexB",
                 "ageB*groupB*sexB", "ageB*groupC*sexB", "1", "1"))
  expect_equal(make_vt("lmer(Y~weight+age*group*sex+(1|GF1)+(1|GF2), data)",
                       cat_vars = c("group", "sex", "age"),
                       cat_vars_nlevels = c(3, 2, 3))[[1]][,6],
               c("1", "weight", "ageB", "ageC", "groupB", "groupC", "sexB",
                 "ageB*groupB","ageB*groupC", "ageC*groupB","ageC*groupC",
                 "ageB*sexB", "ageC*sexB", "groupB*sexB", "groupC*sexB",
                 "ageB*groupB*sexB", "ageB*groupC*sexB",
                 "ageC*groupB*sexB", "ageC*groupC*sexB", "1", "1"))
})
#> Test passed

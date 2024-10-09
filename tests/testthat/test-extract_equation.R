
expect_error(extract_equation("lmer(Y ~ 1+time*age*sex*weight+(1|ID), data)"),
             "This function only works for 3 interactions specified using '*'.")

# test_that("fe is correct", {
#   expect_equal(extract_equation("lmer(Y ~ 1+(1|GF), data)")[[1]],
#                "\\beta_{0}")
#   expect_equal(extract_equation("lmer(Y ~ Time +(1|GF))")[[1]],
#                "\\beta_{0} + \\beta_{1}(\\operatorname{Time})")
#   expect_equal(extract_equation("lmer(Y ~ Time + I(Time^2) +(1|GF))")[[1]],
#                "\\beta_{0} + \\beta_{1}(\\operatorname{Time}) + \\beta_{2}(\\operatorname{Time^2})")
#   expect_equal(extract_equation("lmer(Y ~ Time*Sex*Age +(1|GF))")[[1]],
#                "\\beta_{0} + \\beta_{1}(\\operatorname{Time}) + \\beta_{2}(\\operatorname{Time^2})")
# })
#> Test passed

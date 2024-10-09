test_that("term is correct for lmer models", {
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+(1|GF), data)")$Term,
               c("1", "(1|GF)", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+Time+(1+Time|ID), data)")$Term,
               c("1", "Time", "(1+...|ID)", "(...+Time|ID)", "", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time|ID))")$Term,
               c("1", "Time", "(1+...|ID)", "(...+Time|ID)", "", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+Time+(1+Time||ID), data)")$Term,
               c("1", "Time", "(1+...||ID)", "(...+Time||ID)", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time||ID), data)")$Term,
               c("1", "Time", "(1+...||ID)", "(...+Time||ID)", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+Time+(1|ID)+(0+Time|ID), data)")$Term,
               c("1", "Time", "(1|ID)", "(0+Time|ID)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+Time+(1|ID)+(Time-1|ID), data)")$Term,
               c("1", "Time", "(1|ID)", "(Time-1|ID)", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time-1|ID)+(1|ID))")$Term,
               c("1", "Time", "(1|ID)", "(Time-1|ID)", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time-1|ID)+(age|ID))")$Term,
               c("1", "Time", "(1+...|ID)", "(...+age|ID)",
                 "(Time-1|ID)", "", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time-1|ID)+(1+age|ID))")$Term,
               c("1", "Time", "(1+...|ID)", "(...+age|ID)",
                 "(Time-1|ID)", "", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+Time+I(Time^2)+(1+Time+I(Time^2)|ID))")$Term,
               c("1", "Time", "I(Time^2)", "(1+...+...|ID)", "(...+Time+...|ID)",
                 "(...+...+I(Time^2)|ID)", "", "", "", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+(Time+I(Time^2)||ID),data)")$Term,
               c("1", "Time", "(1+...+...||ID)", "(...+Time+...||ID)",
                 "(...+...+I(Time^2)||ID)", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+Time+I(Time^2)+(1|ID)+(Time+I(Time^2)-1|ID), data)")$Term,
               c("1", "Time", "I(Time^2)", "(1|ID)", "(Time+...-1|ID)",
                 "(...+I(Time^2)-1|ID)", "", ""))
  expect_equal(extract_variables("lmer(Y ~ Time+I(Time-10)+(1|ID)+(Time+I(Time^2)+0|ID), data)")$Term,
               c("1", "Time", "I(Time-10)", "(1|ID)", "(Time+...+0|ID)",
                 "(...+I(Time^2)+0|ID)", "", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+(1|GF1)+(1|GF2), data)")$Term,
               c("1", "(1|GF1)", "(1|GF2)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+(1|G1:G2)+(1|GF2), data)")$Term,
               c("1", "(1|G1G2)", "(1|GF2)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+(1|GF1)+(1|GF2)+(1|GF3), data)")$Term,
               c("1", "(1|GF1)", "(1|GF2)", "(1|GF3)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+(1+FE||GF1)+(wonder-1|GF2)+(1|GF2)+(1|GF3))")$Term,
               c("1", "(1+...||GF1)", "(...+FE||GF1)",
                 "(1|GF2)", "(wonder-1|GF2)", "(1|GF3)", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+(1|School/Class), data)")$Term,
               c("1", "(1|School)", "(1|School:Class)", ""))
  expect_equal(extract_variables("lmer(Y ~ FE+(FE|School/Class), data)")$Term,
               c("1", "FE", "(1+...|School)", "(...+FE|School)", "",
                 "(1+...|School:Class)", "(...+FE|School:Class)", "", ""))
  expect_equal(extract_variables("lmer(Y ~ FE+(FE||School/Class), data)")$Term,
               c("1", "FE", "(1+...||School)", "(...+FE||School)",
                 "(1+...||School:Class)", "(...+FE||School:Class)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+(1|District/School/Class), data)")$Term,
               c("1", "(1|District)", "(1|District:School)",
                 "(1|District:School:Class)", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lmer(Y ~ 1+(1|GF1)+(1|GF2/GF3), data)")$Term,
               c("1", "(1|GF1)", "(1|GF2)", "(1|GF2:GF3)", ""))
  expect_equal(extract_variables("lmer(Y ~ 1+(1|GF1/GF2)+(1|GF1/GF3))")$Term,
               c("1", "(1|GF1)", "(1|GF1:GF3)", "(1|GF1:GF2)", ""))
})
#> Test passed



test_that("term is correct for lme models", {
  #----------------------------------------------------------------
  expect_equal(extract_variables("lme(Y ~ 1, random=~1, data)")$Term,
               c("1", "~1|AddGF", ""))
  expect_equal(extract_variables("lme(Y ~ 1, random=~1|GF)")$Term,
               c("1", "~1|GF", ""))
  expect_equal(extract_variables("lme(Y ~ 1, random=list(GF=~1))")$Term,
               c("1", "GF=~1", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lme(Y ~ 1 + Time, random=~1+Time|ID)")$Term,
               c("1", "Time", "~1+...|ID", "~...+Time|ID", "", ""))
  expect_equal(extract_variables("lme(Y ~ Time, random=~Time|ID, data)")$Term,
               c("1", "Time", "~1+...|ID", "~...+Time|ID", "", ""))
  expect_equal(extract_variables("lme(Y~Time,random=list(ID=~1+Time+I(Time^2)))")$Term,
               c("1", "Time", "ID=~1+...+...", "ID=~...+Time+...",
                 "ID=~...+...+I(Time^2)", "", "", "", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lme(Y ~ 1 + Time, random=list(ID=pdDiag(~1+Time)), data)")$Term,
               c("1", "Time", "ID=pdDiag(~1+...)", "ID=pdDiag(~...+Time)", ""))
  expect_equal(extract_variables("lme(Y~Time+I(Time^2),random=list(ID=pdDiag(~1+Time+I(Time^2))))")$Term,
               c("1", "Time", "I(Time^2)",
                 "ID=pdDiag(~1+...+...)", "ID=pdDiag(~...+Time+...)",
                 "ID=pdDiag(~...+...+I(Time^2))", ""))
  expect_equal(extract_variables("lme(Y~Time + I(Time^2),random=list(ID=pdBlocked(list(~1,~Time+I(Time^2)-1))))")$Term,
               c("1", "Time", "I(Time^2)",
                 "ID=pdBlocked(list(~1,~...+...-1))",
                 "ID=pdBlocked(list(~...,~Time+...-1))",
                 "ID=pdBlocked(list(~...,~...+I(Time^2)-1))", "", ""))
  #----------------------------------------------------------------
  expect_equal(extract_variables("lme(Y ~ 1, random=~1|District/School/Class, data)")$Term,
               c("1", "~1|District/School/Class", "~1|District/School/Class",
                 "~1|District/School/Class", ""))
  expect_equal(extract_variables("lme(Y ~ FE, random=~1+FE|School/Class)")$Term,
               c("1", "FE", "~1+...|School/Class", "~...+FE|School/Class", "",
                 "~1+...|School/Class", "~...+FE|School/Class", "", ""))
  expect_equal(extract_variables("lme(Y ~ 1, random=list(School=~1,Class=~1), data)")$Term,
               c("1", "School=~1", "Class=~1", ""))
  expect_equal(extract_variables("lme(Y ~ FE, random=list(School=pdDiag(~1+FE),Class=~1), data)")$Term,
               c("1", "FE", "School=pdDiag(~1+...)",
                 "School=pdDiag(~...+FE)", "Class=~1", ""))
  #----------------------------------------------------------------
})
#> Test passed






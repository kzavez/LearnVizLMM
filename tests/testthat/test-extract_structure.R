
expect_error(extract_structure(gf_description = "nested",
                               gf_names = c("School", "Class")),
             "Can't find input 'n_gf' or 'model'.")
expect_error(extract_structure(n_gf = 5,
                               gf_description = "nested",
                               gf_names = c("School", "Class")),
             "'n_gf' must be 1, 2, or 3.")
expect_error(extract_structure(n_gf = 2,
                               gf_description = "nested"),
             "Can't find input 'gf_names' or 'model'.")
expect_error(extract_structure(n_gf = 2,
                               gf_description = "nested",
                               gf_names = c("District", "School", "Class")),
             "'gf_names' must have length equal to 'n_gf'.")
expect_error(extract_structure(n_gf = 2,
                               gf_names = c("School", "Class")),
             "Can't find input 'gf_description' or 'model'.")
expect_error(extract_structure(n_gf = 3,
                               gf_description = "cross",
                               gf_names = c("District", "School", "Class")),
             "Check 'gf_description'.")
expect_error(extract_structure(n_gf = 3,
                               gf_description = "crossed",
                               gf_names = c("District", "School", "Class"),
                               gf_nlevels = c(4, 10)),
             "'gf_nlevels' must have length equal to 'n_gf'.")
expect_error(extract_structure(n_gf = 3,
                               gf_description = "crossed",
                               gf_names = c("District", "School", "Class"),
                               gf_nlevels = c(4, 2, 10)),
             "'gf_nlevels' must be greater than or equal to 3 if numeric.")

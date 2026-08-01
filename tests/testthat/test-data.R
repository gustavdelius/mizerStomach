test_that("barnes_data has its documented structure and invariants", {
    expect_s3_class(barnes_data, "data.frame")
    expect_equal(dim(barnes_data), c(23164, 5))
    expect_named(
        barnes_data,
        c("species", "w_pred", "w_prey", "n_prey", "log_ppmr")
    )
    expect_equal(length(unique(barnes_data$species)), 12)
    expect_true(all(barnes_data$n_prey == 1))
    expect_equal(barnes_data$log_ppmr, log(barnes_data$w_pred / barnes_data$w_prey))
})

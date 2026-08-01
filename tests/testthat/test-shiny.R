capture_fit_gadget <- function(...) {
    testthat::with_mocked_bindings(
        fit_shiny(...),
        runGadget = function(ui, server, viewer) {
            list(ui = ui, server = server, viewer = viewer)
        },
        .package = "shiny"
    )
}

test_that("fit_shiny validates data and constructs an external-browser gadget", {
    expect_error(fit_shiny("not data"), "must be a data frame")

    gadget <- capture_fit_gadget(barnes_data, predators = "Albacore")

    expect_s3_class(gadget$ui, "shiny.tag.list")
    expect_true(is.function(gadget$server))
    expect_true(is.function(gadget$viewer))
})

test_that("fit_shiny creates documented defaults for selected predators", {
    predators <- c("Albacore", "Atlantic cod")
    gadget <- capture_fit_gadget(barnes_data, predators = predators)

    shiny::testServer(gadget$server, {
        session$setInputs(
            sp = predators[1], dist = "trunc_exp", plot_type = "histogram"
        )
        result <- build_result()

        expect_equal(result$species, predators)
        expect_equal(result$distribution, rep("trunc_exp", 2))
        expect_equal(result$power, rep(0, 2))
        expect_equal(result$min_w_pred, rep(0, 2))
        expect_equal(result$alpha, rep(0, 2))
        expect_equal(result$ll, rep(3, 2))
        expect_equal(result$ul, rep(5, 2))
        expect_equal(result$lr, rep(15, 2))
        expect_equal(result$ur, rep(5, 2))
        expect_true(any(grepl(predators[1], as.character(output$sp_sel))))
        expect_true(any(grepl("power-law exponent", as.character(output$sp_params))))

        session$setInputs(sp = predators[2])
        expect_equal(sel_dist_rv()[[predators[2]]], "trunc_exp")

        generator <- session$.__enclos_env__$private$file_generators$get(
            session$ns("download_params")
        )
        path <- tempfile(fileext = ".rds")
        generator$content(path)
        downloaded <- readRDS(path)
        expect_equal(generator$filename(), "fits.rds")
        expect_equal(downloaded$species, predators)
    })
})

test_that("fit_shiny defaults to every species in the observations", {
    gadget <- capture_fit_gadget(barnes_data)

    shiny::testServer(gadget$server, {
        expect_equal(build_result()$species, unique(barnes_data$species))
    })
})

test_that("fit_shiny uses input fits and ignores predators when both are supplied", {
    initial <- data.frame(
        species = "Albacore", distribution = "normal",
        mean = 5, sd = 1, power = 0.5, min_w_pred = 20
    )
    gadget <- capture_fit_gadget(
        barnes_data, fits = initial, predators = "Atlantic cod"
    )

    shiny::testServer(gadget$server, {
        session$setInputs(sp = "Albacore", dist = "normal", plot_type = "kernel")
        result <- build_result()

        expect_equal(result$species, "Albacore")
        expect_equal(result$distribution, "normal")
        expect_equal(result$mean, 5)
        expect_equal(result$sd, 1)
        expect_equal(result$power, 0.5)
        expect_equal(result$min_w_pred, 20)
        expect_equal(names(sel_dist_rv()), "Albacore")
        expect_true(any(grepl("mean", as.character(output$sp_params))))
    })
})

test_that("fit_shiny fits and caches a newly selected family", {
    species <- "Albacore"
    gadget <- capture_fit_gadget(barnes_data, predators = species)
    expected <- fit_log_ppmr(barnes_data, species, distribution = "normal")

    shiny::testServer(gadget$server, {
        session$setInputs(sp = species, dist = "trunc_exp", plot_type = "histogram")
        session$setInputs(dist = "normal")

        result <- build_result()
        expect_equal(result$distribution, "normal")
        expect_equal(result$mean, expected$mean)
        expect_equal(result$sd, expected$sd)
        expect_true(any(grepl("mean", as.character(output$sp_params))))
    })
})

test_that("fit_shiny supplies each family default when a species has no data", {
    initial <- data.frame(
        species = "No stomach data", distribution = "normal",
        mean = 6, sd = 2
    )
    gadget <- capture_fit_gadget(barnes_data, fits = initial)

    shiny::testServer(gadget$server, {
        session$setInputs(
            sp = "No stomach data", dist = "normal", plot_type = "histogram"
        )
        session$setInputs(dist = "gauss_mix")

        result <- build_result()
        expect_equal(result$distribution, "gauss_mix")
        expect_equal(result$p[[1]], c(0.5, 0.5))
        expect_equal(result$mean[[1]], c(3, 7))
        expect_equal(result$sd[[1]], c(1, 1))

        session$setInputs(dist = "trunc_exp")
        result <- build_result()
        expect_equal(result$distribution, "trunc_exp")
        expect_equal(result$alpha, 0)
        expect_equal(result[c("ll", "ul", "lr", "ur")],
                     data.frame(ll = 3, ul = 5, lr = 15, ur = 5),
                     ignore_attr = TRUE)
        expect_match(as.character(output$distPlot$alt), "")
    })
})

test_that("fit_shiny asynchronously fits and refits a truncated family", {
    species <- "Albacore"
    initial <- data.frame(
        species = species, distribution = "normal", mean = 5, sd = 1
    )
    gadget <- capture_fit_gadget(barnes_data, fits = initial)
    pending <- NULL
    calls <- list()
    fitted <- data.frame(
        species = species, distribution = "trunc_exp", power = 0,
        min_w_pred = 0, alpha = 0.2, ll = 2, ul = 4, lr = 12, ur = 6
    )
    mock_fit <- function(ppmr_data, species, distribution, fits = NULL) {
        calls[length(calls) + 1] <<- list(fits)
        fitted
    }

    testthat::with_mocked_bindings(
        testthat::with_mocked_bindings(
            shiny::testServer(gadget$server, {
                session$setInputs(
                    sp = species, dist = "normal", plot_type = "histogram"
                )
                session$setInputs(dist = "trunc_exp")
                expect_true(fitting_rv())
                expect_true(any(grepl("fit-spinner", as.character(
                    output$fitting_indicator
                ))))
                pending()
                expect_false(fitting_rv())
                expect_equal(build_result()$alpha, fitted$alpha)
                expect_null(calls[[1]])

                session$setInputs(fit_btn = 1)
                expect_true(fitting_rv())
                pending()
                expect_false(fitting_rv())
                expect_equal(calls[[2]]$alpha, fitted$alpha)
            }),
            fit_log_ppmr = mock_fit,
            .package = "mizerStomach"
        ),
        later = function(func, delay) pending <<- func,
        .package = "later"
    )
})

test_that("fit_shiny Fit repeats a non-interactive Gaussian-mixture fit", {
    species <- "Albacore"
    mixtures <- data.frame(species = species, distribution = "gauss_mix")
    mixtures$p <- I(list(c(0.4, 0.6)))
    mixtures$mean <- I(list(c(3, 7)))
    mixtures$sd <- I(list(c(1, 2)))
    gadget <- capture_fit_gadget(barnes_data, fits = mixtures)
    expected <- fit_log_ppmr(barnes_data, species, distribution = "gauss_mix")

    shiny::testServer(gadget$server, {
        session$setInputs(
            sp = species, dist = "gauss_mix", plot_type = "histogram"
        )
        session$setInputs(fit_btn = 1)
        result <- build_result()

        expect_equal(result$p[[1]], expected$p[[1]])
        expect_equal(result$mean[[1]], expected$mean[[1]])
        expect_equal(result$sd[[1]], expected$sd[[1]])
    })
})

test_that("fit_shiny edits, undoes, resets, and returns selected fits", {
    initial <- data.frame(
        species = "Albacore", distribution = "normal",
        mean = 5, sd = 1, power = 0, min_w_pred = 0
    )
    gadget <- capture_fit_gadget(barnes_data, fits = initial)
    returned <- NULL

    testthat::with_mocked_bindings(
        shiny::testServer(gadget$server, {
            session$setInputs(
                sp = "Albacore", dist = "normal", plot_type = "histogram"
            )
            # First slider event initializes the observer's species/family key.
            session$setInputs(mean = 5, sd = 1)
            session$setInputs(mean = 6)
            expect_equal(build_result()$mean, 6)

            session$setInputs(undo_btn = 1)
            expect_equal(build_result()$mean, 5)

            session$setInputs(mean = 6.5)
            expect_equal(build_result()$mean, 6.5)
            session$setInputs(reset_btn = 1)
            expect_equal(build_result()$mean, 5)

            session$setInputs(done = 1)
        }),
        stopApp = function(returnValue = invisible()) {
            returned <<- returnValue
        },
        .package = "shiny"
    )

    expect_equal(returned$species, "Albacore")
    expect_equal(returned$distribution, "normal")
    expect_equal(returned$mean, 5)
})

test_that("fit_shiny slider editing supports all three families", {
    mixtures <- data.frame(species = "Albacore", distribution = "gauss_mix")
    mixtures$p <- I(list(c(0.4, 0.6)))
    mixtures$mean <- I(list(c(3, 7)))
    mixtures$sd <- I(list(c(1, 2)))
    gadget <- capture_fit_gadget(barnes_data, fits = mixtures)

    shiny::testServer(gadget$server, {
        session$setInputs(
            sp = "Albacore", dist = "gauss_mix", plot_type = "histogram"
        )
        session$setInputs(p1 = 0.4, mean1 = 3, mean2 = 7, sd1 = 1, sd2 = 2)
        session$setInputs(p1 = 0.3)
        result <- build_result()

        expect_equal(result$p[[1]], c(0.3, 0.7))
        expect_equal(result$mean[[1]], c(3, 7))
        expect_equal(result$sd[[1]], c(1, 2))
        expect_true(any(grepl("left component", as.character(output$sp_params))))
    })

    truncated <- data.frame(
        species = "Albacore", distribution = "trunc_exp",
        alpha = 0.2, ll = 2, ul = 4, lr = 12, ur = 6
    )
    gadget <- capture_fit_gadget(barnes_data, fits = truncated)

    shiny::testServer(gadget$server, {
        session$setInputs(
            sp = "Albacore", dist = "trunc_exp", plot_type = "histogram"
        )
        session$setInputs(alpha = 0.2, ll = 2, ul = 4, lr = 12, ur = 6)
        session$setInputs(alpha = 0.4)
        result <- build_result()

        expect_equal(result$alpha, 0.4)
        expect_equal(
            unname(unlist(result[c("ll", "ul", "lr", "ur")])),
            unname(unlist(truncated[c("ll", "ul", "lr", "ur")]))
        )
    })
})

#' Interactively tune PPMR distribution fits
#'
#' A Shiny gadget for visually tuning distribution parameters for each predator
#' species. Supports "normal", "trunc_exp", and "gauss_mix" (two components)
#' distributions, selectable via a radio button in the sidebar. Returns the
#' updated fits data frame when the "Return" button is clicked.
#'
#' @param ppmr_data A data frame with log ppmr observations. See
#'   [validate_ppmr_data()] for details.
#' @param fits A fit data frame (as returned by [fit_log_ppmr()]). If not
#'   provided, a default fit is constructed from the `predators` argument.
#' @param predators A character vector of species names to include. Only used
#'   when `fits` is not supplied. Defaults to all unique species in
#'   `ppmr_data`.
#' @return The updated fit data frame, invisibly, when the gadget is closed via
#'   the "Return" button.
#' @export
fit_shiny <- function(ppmr_data,
                      fits = NULL,
                      predators = NULL) {
    ppmr_data <- validate_ppmr_data(ppmr_data)

    if (is.null(fits)) {
        if (is.null(predators)) {
            predators <- unique(ppmr_data$species)
        }
        fits <- data.frame(
            species      = predators,
            alpha        = 0,
            ll           = 3,
            ul           = 5,
            lr           = 15,
            ur           = 5,
            distribution = "trunc_exp",
            min_w_pred   = 0,
            power        = 0,
            stringsAsFactors = FALSE
        )
        rownames(fits) <- fits$species
    } else {
        fits <- validate_fit(fits)
    }

    # Helper: create a default single-row fit data frame for a species/dist
    make_default_fit <- function(sp, dist, power = 0, min_w_pred = 0) {
        base <- data.frame(
            species = sp, distribution = dist,
            min_w_pred = min_w_pred, power = power,
            stringsAsFactors = FALSE
        )
        if (dist == "trunc_exp") {
            base$alpha <- 0; base$ll <- 3; base$ul <- 5
            base$lr <- 15;   base$ur <- 5
        } else if (dist == "normal") {
            base$mean <- 5; base$sd <- 1
        } else {  # gauss_mix
            base$p    <- list(c(0.5, 0.5))
            base$mean <- list(c(3, 7))
            base$sd   <- list(c(1, 1))
        }
        rownames(base) <- sp
        base
    }

    # Nested cache: cache[[sp]][[dist]] = single-row fit data frame.
    # Initialised from the input fits; other distributions start empty (NULL)
    # and are created on first visit (defaults) or after fitting.
    fits_cache <- list()
    for (i in seq_len(nrow(fits))) {
        sp   <- fits$species[i]
        dist <- fits$distribution[i]
        if (is.null(fits_cache[[sp]])) fits_cache[[sp]] <- list()
        fits_cache[[sp]][[dist]] <- fits[i, , drop = FALSE]
    }

    # Selected distribution per species (initialised from input fits)
    sel_dist_init <- setNames(fits$distribution, fits$species)

    ui <- shiny::fluidPage(
        shiny::tags$head(shiny::tags$style(shiny::HTML("
            .fit-spinner {
                display: inline-block;
                width: 24px;
                height: 24px;
                border: 3px solid #ddd;
                border-top-color: #555;
                border-radius: 50%;
                animation: fit-spin 0.8s linear infinite;
            }
            @keyframes fit-spin {
                to { transform: rotate(360deg); }
            }
        "))),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::actionButton("reset_btn", "Reset", icon = shiny::icon("fast-backward")),
                shiny::downloadButton("download_params", "Download"),
                shiny::actionButton(
                    "done", "Return", icon = shiny::icon("check"),
                    onclick = "setTimeout(function(){window.close();},500);"
                ),
                shiny::actionButton("fit_btn", "Fit", icon = shiny::icon("play")),
                shiny::actionButton("undo_btn", "Undo", icon = shiny::icon("undo")),
                shiny::uiOutput("sp_sel"),
                shiny::radioButtons(
                    "dist", "Distribution:",
                    choices = c(
                        "Normal"              = "normal",
                        "Truncated power law" = "trunc_exp",
                        "Gaussian mixture"    = "gauss_mix"
                    ),
                    selected = fits$distribution[1]
                ),
                shiny::uiOutput("sp_params")
            ),
            shiny::mainPanel(
                shiny::plotOutput("distPlot"),
                shiny::uiOutput("fitting_indicator"),
                shiny::radioButtons(
                    "plot_type", NULL,
                    choices  = c("Kernel" = "kernel", "Histogram" = "histogram"),
                    selected = "histogram",
                    inline   = TRUE
                )
            )
        )
    )

    server <- function(input, output, session) {
        flags <- new.env()
        flags$key_old <- NULL

        cache_rv      <- shiny::reactiveVal(fits_cache)
        sel_dist_rv   <- shiny::reactiveVal(sel_dist_init)
        undo_stack_rv <- shiny::reactiveVal(list())
        fitting_rv    <- shiny::reactiveVal(FALSE)

        push_undo <- function() {
            stack <- shiny::isolate(undo_stack_rv())
            stack[[length(stack) + 1]] <- shiny::isolate(cache_rv())
            undo_stack_rv(stack)
        }

        update_sliders_from_fit <- function(fit, dist) {
            if (dist == "trunc_exp") {
                shiny::updateSliderInput(session, "alpha", value = fit$alpha)
                shiny::updateSliderInput(session, "ll",    value = fit$ll)
                shiny::updateSliderInput(session, "ul",    value = fit$ul)
                shiny::updateSliderInput(session, "lr",    value = fit$lr)
                shiny::updateSliderInput(session, "ur",    value = fit$ur)
            } else if (dist == "normal") {
                shiny::updateSliderInput(session, "mean", value = fit$mean)
                shiny::updateSliderInput(session, "sd",   value = fit$sd)
            } else {
                shiny::updateSliderInput(session, "p1",    value = fit$p[[1]][1])
                shiny::updateSliderInput(session, "mean1", value = fit$mean[[1]][1])
                shiny::updateSliderInput(session, "sd1",   value = fit$sd[[1]][1])
                shiny::updateSliderInput(session, "mean2", value = fit$mean[[1]][2])
                shiny::updateSliderInput(session, "sd2",   value = fit$sd[[1]][2])
            }
        }

        # Helper: return the current single-row fit (isolating reactive reads)
        current_fit_isolated <- function() {
            sp   <- shiny::isolate(input$sp)
            dist <- shiny::isolate(input$dist)
            cache_rv()[[sp]][[dist]]
        }

        # Helper: build the final result data frame (current dist per species)
        build_result <- function() {
            cache <- cache_rv()
            sd    <- sel_dist_rv()
            dplyr::bind_rows(lapply(names(sd), function(sp) cache[[sp]][[sd[[sp]]]]))
        }

        output$fitting_indicator <- shiny::renderUI({
            if (fitting_rv()) {
                shiny::tags$div(
                    style = "text-align: center; padding: 8px 0;",
                    shiny::tags$div(class = "fit-spinner")
                )
            }
        })

        output$sp_sel <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput("sp", "Species to tune:", as.character(fits$species))
            )
        })

        # When species changes, sync the distribution radio to that species' dist
        shiny::observeEvent(input$sp, {
            dist <- sel_dist_rv()[[input$sp]]
            shiny::updateRadioButtons(session, "dist", selected = dist)
        }, ignoreInit = TRUE)

        # When distribution changes:
        #   - guard against programmatic radio updates (no-op when already matches)
        #   - look up or create a cached fit for the new distribution
        #   - update sel_dist_rv for this species
        shiny::observeEvent(input$dist, {
            shiny::req(input$sp)
            sp       <- input$sp
            new_dist <- input$dist
            old_dist <- sel_dist_rv()[[sp]]

            if (old_dist == new_dist) return()

            # Update selected distribution for this species
            sd       <- sel_dist_rv()
            sd[[sp]] <- new_dist
            sel_dist_rv(sd)

            # Populate cache for new_dist if not yet visited
            cache <- cache_rv()
            if (is.null(cache[[sp]][[new_dist]])) {
                old_fit <- cache[[sp]][[old_dist]]
                if (sp %in% ppmr_data$species) {
                    run_fit <- function() {
                        tryCatch({
                            new_fit <- fit_log_ppmr(ppmr_data, species = sp,
                                                    distribution = new_dist)
                            upd <- shiny::isolate(cache_rv())
                            upd[[sp]][[new_dist]] <- new_fit
                            cache_rv(upd)
                            update_sliders_from_fit(new_fit, new_dist)
                        }, error = function(e) {
                            shiny::showNotification(
                                paste("Auto-fit failed, using defaults:", conditionMessage(e)),
                                type = "warning"
                            )
                            upd <- shiny::isolate(cache_rv())
                            upd[[sp]][[new_dist]] <- make_default_fit(
                                sp, new_dist,
                                power      = old_fit$power,
                                min_w_pred = old_fit$min_w_pred
                            )
                            cache_rv(upd)
                        })
                    }
                    if (new_dist == "trunc_exp") {
                        fitting_rv(TRUE)
                        later::later(function() { run_fit(); fitting_rv(FALSE) }, delay = 0)
                    } else {
                        run_fit()
                    }
                } else {
                    cache[[sp]][[new_dist]] <- make_default_fit(
                        sp, new_dist,
                        power      = old_fit$power,
                        min_w_pred = old_fit$min_w_pred
                    )
                    cache_rv(cache)
                }
            }
        })

        output$sp_params <- shiny::renderUI({
            shiny::req(input$sp, input$dist)
            f    <- shiny::isolate(cache_rv())[[input$sp]][[input$dist]]
            dist <- input$dist
            if (is.null(f)) f <- make_default_fit(input$sp, dist)
            sp_data <- ppmr_data[ppmr_data$species == input$sp, ]
            if (nrow(sp_data) > 0) {
                data_min <- floor(min(sp_data$log_ppmr) * 10) / 10
                data_max <- ceiling(max(sp_data$log_ppmr) * 10) / 10
            } else {
                data_min <- 0
                data_max <- 18
            }
            if (dist == "trunc_exp") {
                shiny::tagList(
                    shiny::sliderInput("alpha", "power-law exponent", min = -1,
                                       max = 2, value = f$alpha, step = 0.05),
                    shiny::sliderInput("ll", "location of left sigmoid",
                                       min = data_min, max = data_max,
                                       value = min(max(f$ll, data_min), data_max),
                                       step = 0.1),
                    shiny::sliderInput("ul", "shape of left sigmoid",
                                       min = 0.1, max = 10, value = f$ul,
                                       step = 0.1),
                    shiny::sliderInput("lr", "location of right sigmoid",
                                       min = data_min, max = data_max,
                                       value = min(max(f$lr, data_min), data_max),
                                       step = 0.1),
                    shiny::sliderInput("ur", "shape of right sigmoid",
                                       min = 0.1, max = 10, value = f$ur, step = 0.1)
                )
            } else if (dist == "normal") {
                shiny::tagList(
                    shiny::sliderInput("mean", "mean", min = data_min, max = data_max,
                                       value = min(max(f$mean, data_min), data_max),
                                       step = 0.1),
                    shiny::sliderInput("sd",   "sd",   min = 0.1,
                                       max = data_max - data_min,
                                       value = min(f$sd, data_max - data_min),
                                       step = 0.1)
                )
            } else {  # gauss_mix
                shiny::tagList(
                    shiny::sliderInput("p1",    "p (left component)", min = 0, max = 1,
                                       value = f$p[[1]][1], step = 0.01),
                    shiny::sliderInput("mean1", "mean left",  min = data_min, max = data_max,
                                       value = min(max(f$mean[[1]][1], data_min), data_max),
                                       step = 0.1),
                    shiny::sliderInput("sd1",   "sd left",    min = 0.1,
                                       max = data_max - data_min,
                                       value = min(f$sd[[1]][1], data_max - data_min),
                                       step = 0.1),
                    shiny::sliderInput("mean2", "mean right", min = data_min, max = data_max,
                                       value = min(max(f$mean[[1]][2], data_min), data_max),
                                       step = 0.1),
                    shiny::sliderInput("sd2",   "sd right",   min = 0.1,
                                       max = data_max - data_min,
                                       value = min(f$sd[[1]][2], data_max - data_min),
                                       step = 0.1)
                )
            }
        })

        output$download_params <- shiny::downloadHandler(
            filename = "fits.rds",
            content  = function(file) saveRDS(build_result(), file = file)
        )

        shiny::observeEvent(input$done, {
            shiny::stopApp(build_result())
        })

        output$distPlot <- shiny::renderPlot({
            shiny::req(input$sp)
            sp <- input$sp
            if (!sp %in% ppmr_data$species) {
                plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
                text(0, 0, paste0("No stomach data available for\n", sp),
                     cex = 1.4, col = "grey40")
                return()
            }
            dist <- sel_dist_rv()[[sp]]
            fit  <- cache_rv()[[sp]][[dist]]
            plot_log_ppmr_fit(ppmr_data, fit, type = input$plot_type)
        })

        # Observe slider changes and write back to cache_rv[[sp]][[dist]].
        # All slider inputs are read unconditionally at the top so that Shiny
        # registers reactive dependencies on them even when we return early.
        # sp/dist are isolated so only slider changes (not species/dist switches)
        # trigger this observer.  The key check skips the first fire after a
        # species or distribution switch (while sliders are still initialising).
        shiny::observe({
            alpha <- input$alpha; ll    <- input$ll;   ul   <- input$ul
            lr    <- input$lr;    ur    <- input$ur
            mean_v <- input$mean; sd_v  <- input$sd
            p1    <- input$p1;    mean1 <- input$mean1; mean2 <- input$mean2
            sd1   <- input$sd1;   sd2   <- input$sd2

            sp   <- shiny::isolate(input$sp)
            dist <- shiny::isolate(input$dist)
            shiny::req(sp, dist)

            key <- paste(sp, dist, sep = "\n")
            if (!identical(key, flags$key_old)) {
                flags$key_old <- key
                return()
            }
            push_undo()
            cache <- shiny::isolate(cache_rv())
            if (dist == "trunc_exp") {
                shiny::req(alpha, ll, ul, lr, ur)
                cache[[sp]][[dist]]$alpha <- alpha
                cache[[sp]][[dist]]$ll    <- ll
                cache[[sp]][[dist]]$ul    <- ul
                cache[[sp]][[dist]]$lr    <- lr
                cache[[sp]][[dist]]$ur    <- ur
            } else if (dist == "normal") {
                shiny::req(mean_v, sd_v)
                cache[[sp]][[dist]]$mean <- mean_v
                cache[[sp]][[dist]]$sd   <- sd_v
            } else {  # gauss_mix
                shiny::req(p1, mean1, mean2, sd1, sd2)
                cache[[sp]][[dist]]$p    <- list(c(p1, 1 - p1))
                cache[[sp]][[dist]]$mean <- list(c(mean1, mean2))
                cache[[sp]][[dist]]$sd   <- list(c(sd1, sd2))
            }
            cache_rv(cache)
        })

        shiny::observeEvent(input$fit_btn, {
            shiny::req(input$sp, input$dist)
            sp   <- input$sp
            dist <- input$dist
            if (!sp %in% ppmr_data$species) return()
            push_undo()
            if (dist == "trunc_exp") {
                init_fit <- shiny::isolate(cache_rv())[[sp]][["trunc_exp"]]
                fitting_rv(TRUE)
                later::later(function() {
                    tryCatch({
                        new_fit <- fit_log_ppmr(ppmr_data,
                                                distribution = "trunc_exp",
                                                fits = init_fit)
                        upd <- shiny::isolate(cache_rv())
                        upd[[sp]][["trunc_exp"]] <- new_fit
                        cache_rv(upd)
                        update_sliders_from_fit(new_fit, "trunc_exp")
                    }, error = function(e) {
                        shiny::showNotification(
                            paste("Fit failed:", conditionMessage(e)),
                            type = "error")
                    })
                    fitting_rv(FALSE)
                }, delay = 0)
            } else {
                cache <- cache_rv()
                tryCatch({
                    new_fit <- fit_log_ppmr(ppmr_data, species = sp,
                                            distribution = dist)
                    cache[[sp]][[dist]] <- new_fit
                    cache_rv(cache)
                    update_sliders_from_fit(new_fit, dist)
                }, error = function(e) {
                    shiny::showNotification(paste("Fit failed:", conditionMessage(e)),
                                            type = "error")
                })
            }
        })

        shiny::observeEvent(input$reset_btn, {
            push_undo()
            cache_rv(fits_cache)
            sel_dist_rv(sel_dist_init)
            sp   <- shiny::isolate(input$sp)
            dist <- sel_dist_init[[sp]]
            shiny::updateRadioButtons(session, "dist", selected = dist)
            fit  <- fits_cache[[sp]][[dist]]
            if (!is.null(fit)) update_sliders_from_fit(fit, dist)
        })

        shiny::observeEvent(input$undo_btn, {
            stack <- undo_stack_rv()
            shiny::req(length(stack) > 0)
            prev_cache <- stack[[length(stack)]]
            stack[[length(stack)]] <- NULL
            undo_stack_rv(stack)
            cache_rv(prev_cache)
            sp   <- shiny::isolate(input$sp)
            dist <- shiny::isolate(input$dist)
            fit  <- prev_cache[[sp]][[dist]]
            if (!is.null(fit)) update_sliders_from_fit(fit, dist)
        })
    }

    shiny::runGadget(ui, server, viewer = browserViewer())
}

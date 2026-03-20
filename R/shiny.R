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

    # Store fits as a named list of single-row data frames, one per species.
    # This avoids column-mismatch issues when species use different distributions.
    fits_list <- setNames(
        lapply(seq_len(nrow(fits)), function(i) fits[i, , drop = FALSE]),
        fits$species
    )

    ui <- shiny::fluidPage(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::downloadButton("download_params", "Download"),
                shiny::actionButton(
                    "done", "Return", icon = shiny::icon("check"),
                    onclick = "setTimeout(function(){window.close();},500);"
                ),
                shiny::actionButton("fit_btn", "Fit", icon = shiny::icon("play")),
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
                shiny::radioButtons(
                    "plot_type", NULL,
                    choices  = c("Kernel" = "kernel", "Histogram" = "histogram"),
                    selected = "kernel",
                    inline   = TRUE
                ),
                shiny::plotOutput("distPlot")
            )
        )
    )

    server <- function(input, output, session) {
        flags <- new.env()
        flags$key_old <- NULL

        fits_rv <- shiny::reactiveVal(fits_list)

        output$sp_sel <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput("sp", "Species to tune:", as.character(fits$species))
            )
        })

        # When species changes, sync the distribution radio to that species' dist
        shiny::observeEvent(input$sp, {
            f <- fits_rv()[[input$sp]]
            shiny::updateRadioButtons(session, "dist", selected = f$distribution)
        }, ignoreInit = TRUE)

        # When distribution changes, update fits_rv with defaults for the new
        # distribution. The fl[[sp]]$distribution != dist guard makes this a
        # no-op for programmatic radio updates (species switch) because in that
        # case the species' stored distribution already matches the new value.
        shiny::observeEvent(input$dist, {
            shiny::req(input$sp)
            sp   <- input$sp
            dist <- input$dist
            fl   <- fits_rv()
            if (fl[[sp]]$distribution != dist) {
                old      <- fl[[sp]]
                fl[[sp]] <- make_default_fit(sp, dist,
                                             power      = old$power,
                                             min_w_pred = old$min_w_pred)
                fits_rv(fl)
            }
        })

        output$sp_params <- shiny::renderUI({
            shiny::req(input$sp, input$dist)
            f    <- shiny::isolate(fits_rv())[[input$sp]]
            dist <- input$dist
            if (dist == "trunc_exp") {
                shiny::tagList(
                    shiny::sliderInput("alpha", "power-law exponent", min = -2,
                                       max = 1, value = f$alpha, step = 0.05),
                    shiny::sliderInput("ll", "location of left sigmoid",
                                       min = 0, max = 7,  value = f$ll, step = 0.1),
                    shiny::sliderInput("ul", "shape of left sigmoid",
                                       min = 0.1, max = 10, value = f$ul,
                                       step = 0.1),
                    shiny::sliderInput("lr", "location of right sigmoid",
                                       min = 4, max = 18, value = f$lr, step = 0.1),
                    shiny::sliderInput("ur", "shape of right sigmoid",
                                       min = 0.1, max = 10, value = f$ur, step = 0.1)
                )
            } else if (dist == "normal") {
                shiny::tagList(
                    shiny::sliderInput("mean", "mean", min = 0,   max = 15, value = f$mean, step = 0.1),
                    shiny::sliderInput("sd",   "sd",   min = 0.1, max = 8,  value = f$sd,   step = 0.1)
                )
            } else {  # gauss_mix
                shiny::tagList(
                    shiny::sliderInput("p1",    "p (left component)", min = 0,   max = 1,  value = f$p[[1]][1],    step = 0.01),
                    shiny::sliderInput("mean1", "mean left",          min = 0,   max = 15, value = f$mean[[1]][1], step = 0.1),
                    shiny::sliderInput("sd1",   "sd left",            min = 0.1, max = 8,  value = f$sd[[1]][1],   step = 0.1),
                    shiny::sliderInput("mean2", "mean right",         min = 0,   max = 15, value = f$mean[[1]][2], step = 0.1),
                    shiny::sliderInput("sd2",   "sd right",           min = 0.1, max = 8,  value = f$sd[[1]][2],   step = 0.1)
                )
            }
        })

        output$download_params <- shiny::downloadHandler(
            filename = "fits.rds",
            content  = function(file) saveRDS(dplyr::bind_rows(fits_rv()), file = file)
        )

        shiny::observeEvent(input$done, {
            shiny::stopApp(dplyr::bind_rows(fits_rv()))
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
            fit <- fits_rv()[[sp]]
            plot_log_ppmr_fit(ppmr_data, fit, type = input$plot_type)
        })

        # Observe slider changes and write back to fits_rv.
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
            fl <- shiny::isolate(fits_rv())
            if (dist == "trunc_exp") {
                shiny::req(alpha, ll, ul, lr, ur)
                fl[[sp]]$alpha <- alpha
                fl[[sp]]$ll    <- ll
                fl[[sp]]$ul    <- ul
                fl[[sp]]$lr    <- lr
                fl[[sp]]$ur    <- ur
            } else if (dist == "normal") {
                shiny::req(mean_v, sd_v)
                fl[[sp]]$mean <- mean_v
                fl[[sp]]$sd   <- sd_v
            } else {  # gauss_mix
                shiny::req(p1, mean1, mean2, sd1, sd2)
                fl[[sp]]$p    <- list(c(p1, 1 - p1))
                fl[[sp]]$mean <- list(c(mean1, mean2))
                fl[[sp]]$sd   <- list(c(sd1, sd2))
            }
            fits_rv(fl)
        })

        shiny::observeEvent(input$fit_btn, {
            shiny::req(input$sp, input$dist)
            sp   <- input$sp
            dist <- input$dist
            if (!sp %in% ppmr_data$species) return()
            fl <- fits_rv()
            tryCatch({
                new_fit <- if (dist == "trunc_exp") {
                    fit_log_ppmr(ppmr_data, distribution = "trunc_exp",
                                 fits = fl[[sp]])
                } else {
                    fit_log_ppmr(ppmr_data, species = sp, distribution = dist)
                }
                fl[[sp]] <- new_fit
                fits_rv(fl)
                if (dist == "trunc_exp") {
                    shiny::updateSliderInput(session, "alpha", value = new_fit$alpha)
                    shiny::updateSliderInput(session, "ll",    value = new_fit$ll)
                    shiny::updateSliderInput(session, "ul",    value = new_fit$ul)
                    shiny::updateSliderInput(session, "lr",    value = new_fit$lr)
                    shiny::updateSliderInput(session, "ur",    value = new_fit$ur)
                } else if (dist == "normal") {
                    shiny::updateSliderInput(session, "mean", value = new_fit$mean)
                    shiny::updateSliderInput(session, "sd",   value = new_fit$sd)
                } else {  # gauss_mix
                    shiny::updateSliderInput(session, "p1",    value = new_fit$p[[1]][1])
                    shiny::updateSliderInput(session, "mean1", value = new_fit$mean[[1]][1])
                    shiny::updateSliderInput(session, "sd1",   value = new_fit$sd[[1]][1])
                    shiny::updateSliderInput(session, "mean2", value = new_fit$mean[[1]][2])
                    shiny::updateSliderInput(session, "sd2",   value = new_fit$sd[[1]][2])
                }
            }, error = function(e) {
                shiny::showNotification(paste("Fit failed:", conditionMessage(e)),
                                        type = "error")
            })
        })
    }

    shiny::runGadget(ui, server, viewer = browserViewer())
}

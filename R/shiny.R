#' Interactively tune PPMR distribution fits
#'
#' A Shiny gadget for visually tuning truncated exponential distribution
#' parameters for each predator species. Returns the updated fits data frame
#' when the "Return" button is clicked.
#'
#' @param ppmr_data A data frame with log ppmr observations. See
#'   [validate_ppmr_data()] for details.
#' @param fits A fit data frame (as returned by [fit_log_ppmr()]) with
#'   `distribution = "trunc_exp"`. If not provided, a default fit is
#'   constructed from the `predators` argument.
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
            power        = 0
        )
        rownames(fits) <- fits$species
    } else {
        fits <- validate_fit(fits)
    }

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
                shiny::uiOutput("sp_params")
            ),
            shiny::mainPanel(
                shiny::plotOutput("distPlot")
            )
        )
    )

    server <- function(input, output, session) {
        flags <- new.env()

        fits_rv <- shiny::reactiveVal(fits)

        output$sp_sel <- shiny::renderUI({
            shiny::tagList(
                shiny::selectInput("sp", "Species to tune:", as.character(fits$species))
            )
        })

        output$sp_params <- shiny::renderUI({
            shiny::req(input$sp)
            f <- shiny::isolate(fits_rv())[input$sp, ]
            shiny::tagList(
                shiny::sliderInput("alpha", "alpha", min = -2,  max = 1,  value = f$alpha, step = 0.05),
                shiny::sliderInput("ll",    "ll",    min = 0,   max = 7,  value = f$ll,    step = 0.1),
                shiny::sliderInput("ul",    "ul",    min = 0.1, max = 10, value = f$ul,    step = 0.1),
                shiny::sliderInput("lr",    "lr",    min = 4,   max = 18, value = f$lr,    step = 0.1),
                shiny::sliderInput("ur",    "ur",    min = 0.1, max = 10, value = f$ur,    step = 0.1)
            )
        })

        output$download_params <- shiny::downloadHandler(
            filename = "fits.rds",
            content  = function(file) saveRDS(fits_rv(), file = file)
        )

        shiny::observeEvent(input$done, {
            shiny::stopApp(fits_rv())
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
            fit <- fits_rv()[sp, , drop = FALSE]
            plot_log_ppmr_fit(ppmr_data, fit)
        })

        shiny::observe({
            shiny::req(input$alpha, input$ll, input$ul, input$lr, input$ur)
            f  <- shiny::isolate(fits_rv())
            sp <- shiny::isolate(input$sp)
            if (!identical(sp, flags$sp_old)) {
                flags$sp_old <- sp
                return()
            }
            f[sp, "alpha"] <- input$alpha
            f[sp, "ll"]    <- input$ll
            f[sp, "ul"]    <- input$ul
            f[sp, "lr"]    <- input$lr
            f[sp, "ur"]    <- input$ur
            fits_rv(f)
        })

        shiny::observeEvent(input$fit_btn, {
            shiny::req(input$sp)
            sp <- input$sp
            f  <- fits_rv()
            new_fit <- fit_log_ppmr(ppmr_data, distribution = "trunc_exp",
                                    fits = f[sp, , drop = FALSE])
            f[sp, colnames(new_fit)] <- new_fit
            fits_rv(f)
            shiny::updateSliderInput(session, "alpha", value = new_fit$alpha)
            shiny::updateSliderInput(session, "ll",    value = new_fit$ll)
            shiny::updateSliderInput(session, "ul",    value = new_fit$ul)
            shiny::updateSliderInput(session, "lr",    value = new_fit$lr)
            shiny::updateSliderInput(session, "ur",    value = new_fit$ur)
        })
    }

    shiny::runGadget(ui, server, viewer = browserViewer())
}

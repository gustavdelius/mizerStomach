library(shiny)
library(ggplot2)
library(mizerStomach)
library(dplyr)

load("../../stomach_data/data/stomach_dataset.Rdata")
ppmr_data <- stom_df |>
    transmute(species = pred_species,
              w_pred = pred_weight_g,
              w_prey = prey_ind_weight_g,
              n_prey = nprey_perpred,
              log_ppmr = log(ppmr)) |>
    filter(w_pred > 0, w_prey > 0, n_prey > 0) |>
    na.omit() |>
    mizerStomach::validate_ppmr_data()

predators <- c(
    Herring = "Clupea harengus",
    Cod = "Gadus morhua",
    Haddock = "Melanogrammus aeglefinus",
    Whiting = "Merlangius merlangus",
    "Blue whiting" = "Micromesistius poutassou",
    Hake = "Merluccius merluccius",
    Monkfish = "Lophius piscatorius",
    "Horse mackerel" = "Trachurus trachurus",
    Mackerel = "Scomber scombrus",
    Plaice = "Pleuronectes platessa",
    Megrim = "Lepidorhombus whiffiagonis",
    Sole = "Solea solea"
)

fits <- data.frame(
    species = predators,
    alpha = 0,
    ll = 3,
    ul = 5,
    lr = 15,
    ur = 5,
    distribution = "trunc_exp",
    min_w_pred = 0
)
rownames(fits) <- fits$species

# Define UI
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            downloadButton("download_params", "Download"),
            actionButton("done", "Return", icon = icon("check"),
                         onclick = "setTimeout(function(){window.close();},500);"),
            uiOutput("sp_sel"),
            uiOutput("sp_params")
        ),
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Flags to skip certain observers ----
    flags <- new.env()

    rownames(fits) <- fits$species
    fits <- reactiveVal(fits)

    output$sp_sel <- renderUI({
        fits <- isolate(fits())
        species <- as.character(fits$species)
        tagList(
            selectInput("sp", "Species to tune:", species)
        )
    })

    output$sp_params <- renderUI({
        # The parameter sliders get updated whenever the species selector
        # changes
        req(input$sp)
        f <- isolate(fits())
        f <- f[input$sp, ]
        tagList(
            sliderInput("alpha", "alpha", min = -2, max = 1, value = f$alpha, step = 0.05),
            sliderInput("ll", "ll", min = 0, max = 7, value = f$ll, step = 0.1),
            sliderInput("ul", "ul", min = 0.1, max = 10, value = f$ul, step = 0.1),
            sliderInput("lr", "lr", min = 4, max = 18, value = f$lr, step = 0.1),
            sliderInput("ur", "ur", min = 0.1, max = 10, value = f$ur, step = 0.1)
        )
    })

    ## Prepare for download of fits object ####
    output$download_params <- downloadHandler(
        filename = "fits.rds",
        content = function(file) {
            saveRDS(fits(), file = file)
        })

    ## Return ####
    # When the user hits the "Return" button we want to
    # return with the latest fits object
    observeEvent(input$done, {
        stopApp(fits())
    })

    ## Plot ----
    output$distPlot <- renderPlot({
        req(input$sp)
        f <- fits()
        fit <- as.list(f[input$sp, ])

        plot_log_ppmr_fit(ppmr_data, fit)
    })

    ## Update parameters ----
    observe({
        req(input$alpha, input$ll, input$ul, input$lr, input$ur)
        # Update the fits object whenever the user changes the parameters
        f <- isolate(fits())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old)) {
            flags$sp_old <- sp
            return()
        }
        print("trigger")
        f[sp, "alpha"] <- input$alpha
        f[sp, "ll"] <- input$ll
        f[sp, "ul"] <- input$ul
        f[sp, "lr"] <- input$lr
        f[sp, "ur"] <- input$ur
        fits(f)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

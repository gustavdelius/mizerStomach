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
    Mackerel = "Scomber scombrus",
    Plaice = "Pleuronectes platessa",
    Megrim = "Lepidorhombus whiffiagonis",
    Sole = "Solea solea"
)

# Define UI
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("species", "Species", predators),
            sliderInput("alpha", "alpha", min = -2, max = 1, value = 0, step = 0.05),
            sliderInput("ll", "ll", min = 0, max = 7, value = 2, step = 0.1),
            sliderInput("ul", "ul", min = 0.1, max = 10, value = 5, step = 0.1),
            sliderInput("lr", "lr", min = 4, max = 18, value = 15, step = 0.1),
            sliderInput("ur", "ur", min = 0.1, max = 10, value = 5, step = 0.1)
        ),
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    output$distPlot <- renderPlot({
        fit <- list(alpha = input$alpha,
                    ll = input$ll, ul = input$ul,
                    lr = input$lr, ur = input$ur,
                    species = input$species,
                    distribution = "trunc_exp",
                    power = 1,
                    min_w_pred = 0) |>
            validate_fit()

        plot_log_ppmr_fit(ppmr_data, fit)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

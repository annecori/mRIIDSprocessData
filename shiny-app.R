source("../scratch.R")
library(shiny)
ui <- fluidPage(

    titlePanel("Projected Incidences at various locations"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("p.stay",
                        "Probability of staying at i",
                        min = 0,
                        max = 1,
                        value = 0.99)),

        mainPanel(
            plotOutput("distPlot")
        )
    )
)

                                        # Define server logic

server <- function(input, output) {

    output$distPlot <- renderPlot({
        p.stay <- input$p.stay
        p.movement  <- probability_movement(relative.risk, p.stay)
        daily.projections <- plyr::alply(r.j.t, 1, function(r.t){
                                                    r.t   <- as.matrix(r.t)
                                                    out   <- project(incid, r.t, SI_Distr,
                                                                     p.movement, n.dates.sim)
                                                    colnames(incid) <- colnames(incidence.count)
                                                    incidence.proj  <- rbind(incidence.count, out)
                                                    incidence.proj %<>% cbind(Date = dates.all)
                                                    return(incidence.proj[(nrow(incidence.count) + 1):t.max, ])})




        weekly.projections <- lapply(daily.projections, daily.to.weekly) %>% dplyr::bind_rows(.)
        plots.list         <- lapply(colnames(by.location.incidence), function(location){
                                                   available  <- weekly.available[, c("Date", "Category", location)]
                                                   projection <- weekly.projections[, c("Date", location)]
                                                   plot.weekly(available, projection)})

        cowplot::plot_grid(plots.list[[1]], plots.list[[2]], plots.list[[3]],
                           plots.list[[4]], plots.list[[5]], plots.list[[6]])

    })
}

                                        # Run the application
shinyApp(ui = ui, server = server)


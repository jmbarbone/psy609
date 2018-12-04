#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Post-study probability (PPV) %"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("alpha",
                        "Alpha: Likelihood of accepting a null positive",
                        min = 0,
                        max = 1,
                        step = .05,
                        value = .05),
            sliderInput("beta",
                        "Beta, Power (1 - Beta): Likelihood of correctly rejecting a null hypothesis",
                        min = 0,
                        max = 1,
                        step = .05,
                        value = .20),
            sliderInput("bias",
                        "mu (bias): The proportion of research 'found' due to bias (log form)",
                        min = -3,
                        max = 3,
                        step = .2,
                        value = 0),
            sliderInput("studies",
                        "n studies: Number of studies of equal power",
                        min = 0,
                        max = 50,
                        step = 5,
                        value = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ppv_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$ppv_plot <- renderPlot({
        studies <- ifelse(input$studies == 0, 1, input$studies)
        bias <- exp(input$bias)
        bias_1 <- ifelse(bias == 1, 0, bias)
        ppv_d_b <- ifelse(bias == 1, 0, bias * input$beta * df$R)
        df <- data.frame(R = seq(0, 1, .001))
        df$ppv_n <- (1 - input$beta^studies) * df$R + ppv_d_b
        df$ppv_d <- df$R + (1 - (1 - input$alpha)^studies) - input$beta^studies * df$R + bias - bias * input$alpha + bias * input$beta * df$R
        df$ppv <- df$ppv_n / df$ppv_d
        
        # generate bins based on input$bins from ui.R
        p <- ggplot(df, aes(x = R, y = ppv)) +
            #geom_smooth(method = glm, method.args = list(family = "quasibinomial")) +
            geom_line() +
            scale_y_continuous(limits = c(0, 1)) +
            scale_x_continuous(limits = c(0, 1))
        # ggplotly(p)
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

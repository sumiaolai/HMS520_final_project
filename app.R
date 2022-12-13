# ## Purpose: HMS 520 Final Project about Web app
# Authors: Su-Miao Lai and Dibbya Biswa
# Contact: sml10@uw.edu
# History: 
#         Date created: 8 December 2022
#         Date rewritten:
#         Last modified:

# set working directory
setwd("/Users/smlai/Desktop/HMS520_final_project/")
rm(list=ls())


# ------------------GBD Child Mortality Data Viewer-------------------------

# load library
library("shiny")
library("data.table")
library("ggplot2")
library("dplyr")
library("maps")
library("tidyverse")
library("readr")

# load data ---------------------------------------------------------------
gbd5q0 <- fread("data/childMortAndCovars.csv")

# process data ------------------------------------------------------------
gbd5q0_country <- gbd5q0 %>% select(country, year, neoMR, postneoMR, age1_5MR, under5MR)
colnames(gbd5q0_country) <- c("country"="country", "year"="year", "neoMR"="Neonatal Mortality Rate", "postneoMR"="Postneonatal Mortality Rate", "age1_5MR"="Age 1-5 Mortality Rate", "under5MR"="Under 5 Mortality Rate")

variable_names <- list(
  time_series = c("`Neonatal Mortality Rate`", "`Postneonatal Mortality Rate`", "`Age 1-5 Mortality Rate`", "`Under 5 Mortality Rate`")

)

# define UI ---------------------------------------------------------------
ui <- fluidPage(
  navbarPage(
    "GBD Child Mortality Viewer",
    tabPanel(
      "Time series",
      sidebarPanel(
        selectInput(
          inputId = "country",
          label = "Country",
          choices = gbd5q0_country$country,
          selected = "United States",
          multiple = TRUE
        ), 
        selectInput(
          inputId = "variable_time_series",
          label = "Child Mortality Rate",
          choices = variable_names$time_series,
          selected = variable_names$time_series[1]
        ),
      ),
      mainPanel(plotOutput("plot_time_series"))
    )
  )
)
# define server -----------------------------------------------------------
server <- function(input, output) {
  get_time_series_info <- reactive({
    data <- gbd5q0_country[country %in% input$country]
    if (startsWith(input$variable_time_series, "new")) {
      data <- data[year != min(year),]
    }
    list(
      data = data,
      country = paste(input$country, collapse = " & "),
      variable = input$variable_time_series
    )
  })

  
  output$plot_time_series <- renderPlot({
    info <- get_time_series_info()
    fig <- ggplot(info$data) +
      geom_line(aes_string(x = "year", y = info$variable, color = "country")) +
      ggtitle(info$country) +
      xlab("Year") + ylab("Mortality Rate per 100,000")
    fig
  })
  
}

shinyApp(ui = ui, server = server)


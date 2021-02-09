
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rio)
library(here)
library(plotly)
library(dplyr)
library(reactable)
library(ggthemes)



data_report <- import(here("data", "data_report.csv"))

create_plot <- function(var) {
  ttl <- case_when(
    var == "Count_elig" ~ "Eligibility",
    var == "Count_eval" ~ "Evaluated",
    var == "Count_ref" ~ "Referrals",
    var == "Count_qual" ~ "Qualify",
    var == "Count_exit" ~ "Exit",
    TRUE ~ "Screen"
    )
  
  data_report %>%
    ggplot(aes(Year, !!rlang::sym(var))) +
    geom_line(aes(group = Qualify), color= "grey80") +
    geom_point(aes(color = Qualify)) + 
    facet_wrap(~Area, scales = "free_y") + 
    theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1.0,
                                     hjust = 1.0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = glue::glue("Number of Children in EI & ECSE: {ttl}")) +
    scale_colour_colorblind()
}

create_tbl <- function(var) {
  data_report %>%
    select(Year, Area, Eligibility, var) %>%
    pivot_wider(names_from = Eligibility,
                values_from = all_of(var)) %>%
    mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
    mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2))

}


ui <- dashboardPage(
  dashboardHeader(title = "EC Cares Dashboard"),
  
  
  dashboardSidebar(
    selectInput("var", "Select variable:",
                choices = c(
                  "Eligibility" = "Count_elig",
                  "Evaluated" = "Count_eval",
                  "Referrals" = "Count_ref",
                  "Qualify" = "Count_qual",
                  "Exit" = "Count_exit",
                  "Screen" = "Count_screen")
                )
    ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot")), 
      uiOutput("box_tbl")
    ),
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    ggplotly(
      create_plot(input$var)
    )
  })
  
  output$box_tbl <- renderUI({
    ttl <- case_when(
      input$var == "Count_elig" ~ "Eligibility",
      input$var == "Count_eval" ~ "Evaluated",
      input$var == "Count_ref" ~ "Referrals",
      input$var == "Count_qual" ~ "Qualify",
      input$var == "Count_exit" ~ "Exit",
      TRUE ~ "Screen")
    
    box(title = ttl,
        reactable(
          create_tbl(input$var), highlight = TRUE, 
          theme = reactableTheme(
            highlightColor = "#56B4E9",
            headerStyle = list(
              "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
              "&[aria-sort='ascending'], 
              &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
              borderColor = "#555")
            
            )
          ))
    })
  
  output$tbl <- renderReactable({
   reactable(
     create_tbl(input$var)
   )
  })
    
}

shinyApp(ui, server)
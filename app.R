
# Required packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rio)
library(plotly)
library(dplyr)
library(reactable)
library(ggthemes)
library(colorblindr)
library(janitor)
library(here)


#upload datasets here#

data_report <- import(here("data", "data_report.csv"))
data_report_pyramid <- import(here("data", "data_report_pyramid.csv"))
child_placement_qtr <- import(here("data", "child_placement_qtr.csv"))

#A. Add functions here#

#. Global theme for all reactable tables
options(reactable.theme = reactableTheme(
  backgroundColor = "#D6EAF8",
  highlightColor = "#00a5c1"))
  

#2. Monthly Report Data tab
##2.1 Function to change name of variables and build a plot on monthly report data tab

create_plot <- function(var) {
  ttl <- case_when(
    var == "Count_elig" ~ "Eligibility",
    var == "Count_eval" ~ "Evaluated",
    var == "Count_ref" ~ "Referrals",
    var == "Count_qual" ~ "Qualify",
    var == "Count_exit" ~ "Exit",
    TRUE ~ "Screen"
    )
  
  ## function to build a plot after variables name change
  
  data_report %>%
    ggplot(aes(Year, !!rlang::sym(var))) +
    geom_line(aes(group = Qualify), color= "grey80") +
    geom_point(aes(color = Qualify)) + 
    facet_wrap(~Area, scales = "free_y") +
    theme_economist() +
    #theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1.0,
                                     hjust = 1.0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = glue::glue("Number of Children in EI & ECSE: {ttl}")) +
    scale_color_OkabeIto() 

  }

##2.2 function to create a table correspondong to select input variable on monthly report data tab.
create_tbl <- function(var) {
  data_report %>%
    select(Year, Area, Eligibility, var) %>%
    pivot_wider(names_from = Eligibility,
                values_from = all_of(var)) %>%
    mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
    mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2))

}


#B. UI: contains dashboardHeader, dashboardsidebar, dashboardBody

## customise dashboardHeader to add EC cares name and logo and link to their website.

dbHeader <- dashboardHeader(title = HTML("Early Childhood Cares Dashboard"),
                            disable = FALSE,
                            titleWidth =350
                            )

dbHeader$children[[2]]$children[[2]] <- dbHeader$children[[2]]$children[[1]]
dbHeader$children[[2]]$children[[1]] <- tags$a(href='https://earlychildhoodcares.uoregon.edu/about/about-us/',
                                           tags$img(src='eccares_logo.png', height='50',
                                                    width='100', align = 'left'),
                                           target = '_blank')

# "UI" starts here

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", 
                              icon = icon("home"),
                              badgeLabel = "2015-2021", 
                              badgeColor = "yellow"),
                     menuItem("Monthly Report Data", tabName = "monthlyreport",
                              icon = icon("calendar-alt"), 
                              startExpanded = FALSE,
                              menuSubItem(selectInput("var", "Select variable:",
                                                      choices = c("Eligibility" = "Count_elig",
                                                                  "Evaluated" = "Count_eval",
                                                                  "Referrals" = "Count_ref",
                                                                  "Qualify" = "Count_qual",
                                                                  "Exit" = "Count_exit",
                                                                  "Screen" = "Count_screen")))
                              ),
                     menuItem("Child-level Data", tabName = "child", icon = icon("baby")),
                     menuItem("About", tabName = "tab_about", icon = icon("info"))
                   )
  ),

  dashboardBody(
    tags$head(
      # changed color of dashboard header, logo is highlighted when hovered to indicate it's an URL, 
      #and highlight active sidebar menu.
      tags$script("document.title = 'Early Childhood Cares Dashboard'"),
      tags$style(HTML(".fa-dashboard { font-size: 20px; }")),
      tags$style(HTML('
      /* logo */.skin-blue .main-header .logo {
      background-color: #006272;
      }
      /* logo when hovered */
      .skin-blue .main-header .logo:hover {
      background-color: #006272;
      }
      /* navbar (rest of the header)
      */.skin-blue .main-header .navbar {
      background-color: #006272;
      }
      /* active selected tab in the sidebarmenu
      */.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
      background-color: #006272;
      }
      ')),
      tags$style( HTML("hr {border-top: 1px solid #000000;}"))
      ),
    tabItems(
      # Dashboard home
      tabItem(
        tabName = "dashboard",
        #contents for the dashboard home
        h2(paste0("EC Cares 5-Years Overview")),
        p("2015-2021"),
        fluidRow(
          uiOutput("overviewtab"))
        ),
      tabItem(
        tabName = "monthlyreport",
        #content for the monthly report tab
        fluidRow(
          box(plotlyOutput("plot")),
          uiOutput("box_tbl"))
        )
      )
    )
)
  # "UI" ends here and "server" starts #
  server <- function(input, output) {
    
    
    #1. Maindashboard
    output$overviewtab <- renderUI({
      overview_tbl <- data_report_pyramid %>%
        pivot_wider(names_from = "variable",
                    values_from = "number")
      box(reactable(overview_tbl,
                groupBy = "Year",
                height = 350,
                width = 1200,
                striped = TRUE,
                defaultPageSize = 12,
                minRows = 7,
                searchable = TRUE,
                highlight = TRUE))
      })
    
    # 2.1monthly report variables plot
    output$plot <- renderPlotly({
      ggplotly(
        create_plot(input$var),
        height = 400)
      })
    
  #2.2monthly repot correspondong table
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
            create_tbl(input$var),
            groupBy = "Year",
            highlight = TRUE, height=350))
    })
  }
  shinyApp(ui, server)

# Required packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rio)
library(ggplot2)
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
child_ref_outcome <- import(here("data", "child_ref_outcome.csv"))
child_ref_elig <- import(here("data", "child_ref_elig.csv"))
child_ref_rescreen <- import(here("data", "child_ref_rescreen.csv"))
child_eligibility_cond <- import(here("data", "child_eligibility_cond.csv"))
child_dnq_return <- import(here("data", "child_dnq_return.csv"))
child_data_foster <- import(here("data", "child_data_foster.csv"))
child_site_center <- import(here("data", "child_site_center.csv"))

#A. Add functions here#

#. Global theme for all reactable tables
options(reactable.theme = reactableTheme(
  backgroundColor = "#D6EAF8",
  highlightColor = "#00a5c1",
  headerStyle = list(
    "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
    "&[aria-sort='ascending'],
    &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
    borderColor = "#555")))
  

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
                            titleWidth =350)
                            
dbHeader$children[[2]]$children[[2]] <- dbHeader$children[[2]]$children[[1]]
dbHeader$children[[2]]$children[[1]] <- tags$a(href='https://earlychildhoodcares.uoregon.edu/about/about-us/',
                                           tags$img(src='eccares_logo.png', height='50',
                                                    width='100', align = 'left'),
                                           target = '_blank')

# "UI" starts here

ui <- dashboardPage(
  dbHeader,
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("home"),
               badgeLabel = "2015-2021",
               badgeColor = "yellow"),
      
      menuItem("Monthly Report Data", tabName = "monthly_report", 
               icon = icon("calendar-alt"), startExpanded = FALSE),
      
      selectInput("var", "Select variable:",
                  choices = c("Eligibility" = "Count_elig",
                              "Evaluated" = "Count_eval",
                              "Referrals" = "Count_ref",
                              "Qualify" = "Count_qual",
                              "Exit" = "Count_exit",
                              "Screen" = "Count_screen"),
                  selected = "Eligibility"),
      
      menuItem("Child-level Data", tabName = "child", 
               icon = icon("baby"), startExpanded = FALSE,
               menuSubItem("Referral Sources", tabName = "Referral_Sources"),
               menuSubItem("Rescreened", tabName = "Rescreened"),
               menuSubItem("Eligibility Condition", tabName = "Eligibility_Condition"),
               menuSubItem("Placement", tabName = "Placement"),
               menuSubItem("Transition", tabName = "Transition"),
               menuSubItem("DNQ Return", tabName = "DNQ_Return"),
               menuSubItem("Medicaid_Foster", tabName = "Medicaid_Foster"),
               menuSubItem("Site Center", tabName = "Site_Center")
               ),
      
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
        h3(paste0("EC Cares 5-Years Overview")),
        p("2015-2021"),
        fluidRow(
          uiOutput("overview"))
        ),
      
      tabItem(
        tabName = "monthly_report",
        #content for the monthly report tab
        fluidRow(
         # selectInput("var", "Select variable:",
         #             choices = c("Eligibility" = "Count_elig",
         #                         "Evaluated" = "Count_eval",
         #                         "Referrals" = "Count_ref",
         #                         "Qualify" = "Count_qual",
         #                         "Exit" = "Count_exit",
         #                         "Screen" = "Count_screen"),
         #             selected = "Eligibility"),
          box(plotlyOutput("plot")),
          box(uiOutput("box_tbl")))
        ),
      
      tabItem(
        tabName = "Referral_Sources",
        fluidRow(
          box(plotlyOutput("pri_ref")),
          box(plotlyOutput("sec_ref"))
        )
        ),
      
      tabItem(
        tabName = "Rescreened",
        fluidRow(
          box(width = 8, plotlyOutput("rescreened_plot")),
          box(width = 4, uiOutput("rescreened_tbl"))
        )
        ),
      
      tabItem(
        tabName = "Eligibility_Condition",
        fluidRow(
          box(plotlyOutput("elig_cond_plot")),
          box(uiOutput("elig_cond_tbl"))
          )
        ),
      
      tabItem(
        tabName = "Placement",
        fluidRow(
          box(width = 8, plotlyOutput("placement_plot"))),
        fluidRow(uiOutput("placement_tbl"))
        ),
      
      tabItem(
        tabName = "Transition",
        fluidRow(
          box(plotlyOutput("transition_plot")),
          box(uiOutput("transition_tbl")))
        ),
      
      tabItem(
        tabName = "DNQ_Return",
        fluidRow(
          box(width = 8, plotlyOutput("dnq_return_plot")),
          box(width = 4, uiOutput("dnq_return_tbl")))
        ),
      
      tabItem(
        tabName = "Medicaid_Foster",
        fluidRow(
          box(plotlyOutput("medi_fost_plot")),
          box(uiOutput("medi_fost_tbl")))
        ),
      
      tabItem(
        tabName = "Site_Center",
        fluidRow(
          box(width = 7, plotlyOutput("site_plot")),
          box(width = 5, uiOutput("site_tbl")))
        )
      )
    )
  )

  
  # "UI" ends here and "server" starts #
  server <- function(input, output) {
    
    
    #1. tabName = "dashboard"
    output$overview <- renderUI({
      overview_tbl <- data_report_pyramid %>%
        pivot_wider(names_from = "variable",
                    values_from = "number")
      box(width = 12, reactable(overview_tbl,
                                groupBy = "Year",
                                height = 350,
                                width = 1200,
                                striped = TRUE,
                                minRows = 7,
                                highlight = TRUE))
      })
    
    # 2.tabName = "monthlyreport"
    output$plot <- renderPlotly({
      
      ggplotly(create_plot(input$var))
    
      })
    
  #2. monthly report correspondong table
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
            highlight = TRUE,
            width = 550,
            height = 325))
    })
    
    #3 Parent tab : Child-level data
    
    #3.1 tabName = "Referral Sources"
    output$pri_ref <- renderPlotly({
      
      child_ref_pri <- child_ref_outcome %>%
        filter(ref_source == "primary") %>%
        mutate(ref_source_ode = case_when(
          ref_source_ode %in% "CAPTA" ~ "CAPTA",
          ref_source_ode %in% "Other DHS" ~ "CAPTA",
          ref_source_ode %in% "Physician/Clinic" ~ "Phys/Clinic/Hosp",
          ref_source_ode %in% "Hospital" ~ "Phys/Clinic/Hosp",
          TRUE ~ "Other"))
      
      #distinct(child_ref_pri) +TOTAL number of unique records in child_Ref_pri = 4142
      
      child_ref_pri <- child_ref_pri %>%
        group_by(ref_age_class, ref_source_ode) %>%
        count(outcome) %>%
        mutate(Outcome = recode(outcome,
                                "ia" = "Inactive",
                                "p" = "Placement",
                                "e" = "Evaluation",
                                "etm" = "TM-Elig",
                                "r" = "Referral",
                                "s" = "Screening"))
      plot_pri_outcome <-ggplot(child_ref_pri, aes(reorder(ref_source_ode, n),
                                                   n, fill = Outcome)) +
        geom_col(aes(group = Outcome), width = 0.3) +
        scale_fill_manual(values = c("#4c4c4c", "#ff9900", "#68a4bd",
                                     "#86BB8D","#B22222", "#CC79A7")) +
        theme_economist() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Primary Referral Sources and Outcomes") +
        coord_flip() +
        facet_wrap(~ref_age_class)
      
      ggplotly(plot_pri_outcome)
      
    })
    
    output$sec_ref <- renderPlotly({
      
      child_ref_sec <- child_ref_outcome %>%
        filter(ref_source == "secondary") %>%
        mutate(ref_source_ode = case_when(
          ref_source_ode %in% "CAPTA" ~ "CAPTA",
          ref_source_ode %in% "Other DHS" ~ "CAPTA",
          ref_source_ode %in% "Physician/Clinic" ~ "Phys/Clinic/Hosp",
          ref_source_ode %in% "Hospital" ~ "Phys/Clinic/Hosp",
          TRUE ~ "Other")) %>%
        group_by(ref_age_class) %>%
        count(ref_source_ode)
      
      plot_secondary <-ggplot(child_ref_sec, (aes(reorder(ref_age_class, n), n,
                                                  fill = ref_source_ode))) +
        geom_col(width = 0.3) +
        scale_fill_manual(values=c("#CC79A7", "#68a4bd", "#86BB8D")) +
        theme_economist() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Secondary Referral Source",
             x = "Age Class") +
        coord_flip() +
        annotate("text", 
                 x = "EI to ECSE", 
                 y = 200,
                 label = "Note: No outcome recorded\n for the secondary\nreferral sources", 
                 size= 3,
                 color = "red",
                 hjust = 1)
      
      ggplotly(plot_secondary)
      
    })
    
    
    #3.2 tabName = "Rescreened"
    
    output$rescreened_plot <- renderPlotly({
      
      plot_rescreened <-ggplot(child_ref_rescreen,
                               aes(x = ref_age_class, y = number, 
                                   fill = outcome)) +
        geom_col(width = 0.5) +
        scale_fill_manual(values=c("#CC79A7", "#68a4bd")) +
        theme_economist() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Number of children rescreened and\noutcome achieved") +
        coord_flip() +
        annotate("text", 
                 x = "EI to KG", 
                 y = 40,
                 label = "Total number of\nchildren rescreened\nat least twice = 82", 
                 size= 3,
                 color = "red",
                 hjust = 1)
      ggplotly(plot_rescreened)
      
    })
    
    output$rescreened_tbl <- renderUI({
      
      box(width = 4, reactable(child_ref_rescreen,
                               highlight = TRUE,
                               width = 300,
                               height = 350))
      })
    
    #3.3 tabName = "Eligibility Condition"
    
    output$elig_cond_plot <- renderPlotly({
      
      plot_elig <- ggplot(child_eligibility_cond, aes(year, n,
                                                      fill = elig_code)) +
        scale_fill_manual(values=c("#4c4c4c", "#86BB8D", 
                                   "#68a4bd", "#ff9900",
                                   "#ff0000")) +
        geom_col(width = 0.5) +
        facet_wrap(~elig_program, scales = "free_y") +
        theme_economist_white() +
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1.0,
                                         hjust = 1.0),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = "Primary Eligibility Condition for EI & ECSE")
      
      ggplotly(plot_elig) 
      
    })
    
    output$elig_cond_tbl <- renderUI({
      
      elig_cond <- child_eligibility_cond %>%
        pivot_wider(names_from = elig_code,
                    values_from = n)
      
      box(reactable(elig_cond,
                    highlight = TRUE,
                    width = 550,
                    height = 350,
                    groupBy = "year"))
      
    })
    
    #3.4 tabName = "Placement"
    
    output$placement_plot <- renderPlotly({
      
      plot_placement <- ggplot(child_placement_qtr,
                               aes(reorder(qtr, n), n, fill = Outcome)) +
        scale_fill_manual(values=c("#4c4c4c", "#86BB8D", "#B22222", "#ff9900",
                                   "#68a4bd", "#CC79A7", "#008000", "#0072B2" )) +
        geom_col(width = 0.5) +
        facet_wrap(~year, scales = "free_y") +
        theme_economist_white() +
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1.0,
                                         hjust = 1.0,
                                         size = 10),
              axis.text.y = element_text(size = 10),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 10),
              strip.text = element_text(size = 10)) +
        labs(title = "Quaterly Placement Outcome: 2016-2021")
      
      ggplotly(plot_placement)
      
    })
    
    output$placement_tbl <- renderUI({
      
      placement_create_tbl <- child_placement_qtr %>%
        select(year, qtr, Outcome, n) %>%
        pivot_wider(names_from = Outcome,
                    values_from = n) %>%
        group_by(year) %>%
        summarize_all(mean) %>%
        select(-qtr, -typ_peer_or_tution) %>%
        rename("evaluation(mean)" = evaluation,
               "inactive(mean)" = inactive,
               "notification(mean)" = notification,
               "placement(mean)" = placement,
               "referral(mean)" = referral,
               "screening(mean)" = screening,
               "elig_team_meeting(mean)" = elig_team_meet)
      
      box(width = 12, reactable(placement_create_tbl,
                                highlight = TRUE))
    })
    
    #3.5 tabName = "Transition"
    
    output$transition_plot <- renderPlotly({
      plot_tran_elig <- ggplot(child_ref_elig, aes(elig_code, n,
                                                   fill = outcome)) +
        scale_fill_manual(values=c("#4c4c4c", "#86BB8D",
                                   "#68a4bd", "#ff9900","#ff0000")) +
        geom_col(aes(group = outcome), width = 0.5) +
        facet_wrap(~ref_age_class, scales = "free_y") +
        theme_economist_white() +
        theme(axis.text.x = element_text(angle = 45,
                                         vjust = 1.0,
                                         hjust = 1.0,
                                         size = 10),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(size = 8),
              plot.title = element_text(size = 10)) +
        labs(title = "Number of Children Qualifying Based on Conditions during Transition")
      ggplotly(plot_tran_elig) 
      
    })
  
    
    output$transition_tbl <- renderUI({
      
      box(reactable(child_ref_elig, highlight = TRUE, 
                    width = 550, height = 350, groupBy = "ref_age_class",
                    minRows = 2))
      
    })
    
    #3.6 tabName = "DNQ Return"
    
    output$dnq_return_plot <- renderPlotly({
      
      dnq_return <- ggplot(child_dnq_return, aes(reorder(return_category, n), n,
                                                 fill = return_category)) +
        geom_col(fill = "#5F9EA0", width = 0.5) +
        geom_text(aes(label = n), vjust = -0.2, size = 3) +
        theme_economist() + 
        theme(axis.text.x = element_text(angle = 60,
                                         vjust = 1.0,
                                         hjust = 1.0, size = 10),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        annotate("text",
                 x = 4,
                 y = 10,
                 label = "Note: Number of unique\nDNQ children who returned\nat least once=76",
                 size= 3,
                 color = "red",
                 hjust = 1) +
        labs(title = "Number of DNQ children who returned")
      ggplotly(dnq_return)
      
    })
    
    output$dnq_return_tbl <- renderUI({
      child_dnq_return <- child_dnq_return %>%
        select(Inactive, Placement, n)
      
      box(reactable(child_dnq_return,
                    highlight = TRUE,
                    height = 350,
                    width = 350))
    })
    
    #3.7 tabName = "Medicaid/Foster"
    
    output$medi_fost_plot <- renderPlotly({
      
      plot_foster <- ggplot(child_data_foster,
                            aes(x = medi_fost, y = percent)) +
        geom_col(aes(fill = category), width = 0.1) +
        scale_fill_manual(values=c("#CC79A7", "#68a4bd")) +
        theme_economist() +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 12)) +
        labs(title = "Number of children in foster care and with medicaid",
             y = "Percent")
      
      ggplotly(plot_foster)
      
    
    })
    
    output$medi_fost_tbl <- renderUI({
      
      box(reactable(child_data_foster,
                    highlight = TRUE,
                    width = 500))
    })
    
    #3.8 tabName = "Site Center"
    
    output$site_plot <- renderPlotly({
      
      site_plot <- ggplot(child_site_center, 
                          aes(reorder(Placement, Number), 
                              Number, fill = Region)) +
        geom_col(width = 0.3) +
        theme_economist() +
        scale_fill_manual(values=c("#4c4c4c", "#86BB8D", "#B22222", "#ff9900",
                                   "#68a4bd", "#CC79A7", "#0074D9")) +
        theme(axis.text.x = element_text(angle = 60,
                                         vjust = 1.0,
                                         hjust = 1.0, size = 10),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 12, hjust = 0.5),
              legend.title = element_text( size=10), 
              legend.text=element_text(size=10)) +
        labs(title = "Number of Children in each Placement by Region")
      ggplotly(site_plot)
      
    })
    
    output$site_tbl <- renderUI({
      
      child_site_center <- child_site_center[, c("Region", "Placement", "Number")]
      
      box(reactable(child_site_center,
                    groupBy = "Region",
                    highlight = TRUE,
                    width = 460,
                    height = 350))
      
    })
    
  }
  
  shinyApp(ui, server)
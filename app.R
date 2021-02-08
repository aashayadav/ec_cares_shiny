
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rio)
library(here)
library(plotly)
library(dplyr)
library(kableExtra)

data_report <- import(here("data", "data_report.csv"))

view(data_report)

ui <- dashboardPage(
  dashboardHeader(title = "EC Cares Dashboard"),
  
  
  dashboardSidebar(
  selectInput("variable", "Select variable: Report Data",
                choices = c("No. of Children Eligible" = "Eligibile",
                            "No. of Children Evaluated who Qualify " = "Qualify",
                            "No. of Children Screened & Not Referred" = "Screen",
                            "No. of Children Referred" = "Referrals",
                            "No. of Children who Exit" = "Exit",
                            "No. of Evaluated who DNQ" = "Evaluated"),
                selected = "Eligibility")
    ),
  dashboardBody(
    fluidRow(box(plotlyOutput("Eligibility")), box(tableOutput("Eligibility_table"))),
    fluidRow(box(plotlyOutput("Qualify")), box(tableOutput("Qualify_table"))),
    fluidRow(box(plotlyOutput("Screen")), box(tableOutput("Screen_table"))),
    fluidRow(box(plotlyOutput("Referrals")), box(tableOutput("Referrals_table"))),
    fluidRow(box(plotlyOutput("Exit")), box(tableOutput("Exit_table"))),
    fluidRow(box(plotlyOutput("Evaluated")), box(tableOutput("Evaluated_table")))
    )
  )



server <- function(input, output) {
  
  output$Eligibility <- renderPlotly({
    
    d_eligibility<- data_report %>%
      ggplot(aes(Year, Count_elig)) +
      geom_line(aes(group = Eligibility), color= "grey80") +
      geom_point(aes(color = Eligibility)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children Eligible for EI & ECSE") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_eligibility)

  
  })
  
  output$Eligibility_table <- function() {
   
    data_report %>%
      select(Year, Area, Eligibility, Count_elig) %>%
      pivot_wider(names_from = Eligibility,
                  values_from = Count_elig) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children Eligible for EI and ECSE") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px")
      
      
  }
    
  
  output$Referrals <- renderPlotly({
    
    d_referrals<- data_report %>%
      ggplot(aes(Year, Count_ref)) +
      geom_line(aes(group = Referrals), color= "grey80") +
      geom_point(aes(color = Referrals)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children Referred to EI & ECSE") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_referrals)
  
  })
  
  output$Referrals_table <- function() {
    
    data_report %>%
      select(Year, Area, Referrals, Count_ref) %>%
      pivot_wider(names_from = Referrals,
                  values_from = Count_ref) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children Referred to EI & ECSE") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px") 
     
    
  }
  
  
  output$Qualify <- renderPlotly({
    
    d_qualify <- data_report %>%
      ggplot(aes(Year, Count_qual)) +
      geom_line(aes(group = Qualify), color= "grey80") +
      geom_point(aes(color = Qualify)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children Evaluated who Qualify for EI & ECSE") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_qualify)
    
  })
  
  output$Qualify_table <- function() {
    
    data_report %>%
      select(Year, Area, Qualify, Count_qual) %>%
      pivot_wider(names_from = Qualify,
                  values_from = Count_qual) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children Evaluated who Qualify for EI & ECSE") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px") 
      
  }
 
  
  output$Exit <- renderPlotly({
    
    d_exit <- data_report %>%
      ggplot(aes(Year, Count_exit)) +
      geom_line(aes(group = Exit), color= "grey80") +
      geom_point(aes(color = Exit)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children who Exit from EI & ECSE") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_exit)
    
  })
  
  output$Exit_table <- function() {
    
    data_report %>%
      select(Year, Area, Exit, Count_exit) %>%
      pivot_wider(names_from = Exit,
                  values_from = Count_exit) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children who Exit from EI & ECSE") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px") 
     
  } 
  
  
  output$Screen <- renderPlotly({
    
    d_screen <- data_report %>%
      ggplot(aes(Year, Count_screen)) +
      geom_line(aes(group = Screen), color= "grey80") +
      geom_point(aes(color = Screen)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children Screened and Not Referred for Evaluation") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_screen)
    
  })
  
  output$Screen_table <- function() {
    data_report %>%
      select(Year, Area, Screen, Count_screen) %>%
      pivot_wider(names_from = Screen,
                  values_from = Count_screen) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2 )) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children Screened and Not Referred for Evaluation") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px") 
     
  }
  
  
  output$Evaluated <- renderPlotly({
    
    d_eval <- data_report %>%
      ggplot(aes(Year, Count_eval)) +
      geom_line(aes(group = Evaluated), color= "grey80") +
      geom_point(aes(color = Evaluated)) + 
      facet_wrap(~Area) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 1.0,
                                       hjust = 1.0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      labs(title = "Number of Children Evaluated who Did Not Qualify") +
      scale_y_continuous(expand = c(0, 0))
    ggplotly(d_eval)
    
    })
  
  output$Evaluated_table <- function() {
    data_report %>%
      select(Year, Area, Evaluated, Count_eval) %>%
      pivot_wider(names_from = Evaluated,
                  values_from = Count_eval) %>%
      mutate(EI_percent = round((EI/TOTAL)*100, digits = 2)) %>%
      mutate(ECSE_percent = round((ECSE/TOTAL)*100, digits = 2)) %>%
      kable(caption = "Number of Children Evaluated who Did Not Qualify") %>%
      kable_styling(bootstrap_options = "striped", "hover",
                    full_width = F, position = "float_right") %>%
      scroll_box(height = "400px", width = "600px")
    }
  
  }

shinyApp(ui, server)
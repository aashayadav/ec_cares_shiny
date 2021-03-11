
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
library(forcats)
library(htmlwidgets)



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
  

# To remove modebar in ggplotly

custom_modebar <- list("zoom2d","pan2d","select2d","lasso2d",
                       "zoomIn2d","zoomOut2d","autoScale2d",
                       "toggleSpikelines",
                       "resetScale2d")
  
 

# For placement outcome tab: tabName: placement

placement_create_tbl <- child_placement_qtr %>%
  select(year, qtr, Outcome, n) %>%
  pivot_wider(names_from = Outcome,
              values_from = n) %>%
  group_by(year) %>%
  summarize_all(mean) %>%
  select(-qtr, -typ_peer_or_tution) #%>%
 # rename("evaluation(mean)" = evaluation,
 #        "inactive(mean)" = inactive,
 #        "notification(mean)" = notification,
 #        "placement(mean)" = placement,
 #        "referral(mean)" = referral,
 #        "screening(mean)" = screening,
 #       "elig_team_meeting(mean)" = elig_team_meet)

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
    geom_point(aes(color = Qualify), size = 3) + 
    facet_wrap(~Area, scales = "free_y") +
    theme_economist() +
    #theme_minimal(base_size = 15) +
    theme(axis.text.x = element_text(angle = 45,
                                     vjust = 1.0,
                                     hjust = 1.0),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 14),
          strip.text = element_text(face="bold", size = 11)) +
    scale_y_continuous(expand = c(0, 0)) +
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



#  Child level data

# For eligiblity_condition 

elig_cond <- child_eligibility_cond %>%
  pivot_wider(names_from = elig_code,
              values_from = n)

plot_elig <- ggplot(child_eligibility_cond, 
                    aes(year, n,
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
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Primary Eligibility Condition for EI & ECSE")


# tabname: referral
## primary referral plot function

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
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  labs(title = "Primary Referral Source and Outcome") +
  coord_flip() +
  facet_wrap(~ref_age_class)


## secondary referral


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
          legend.text = element_text(size = 10),
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

  # rescreened
  
  plot_rescreened <-ggplot(child_ref_rescreen,
                           aes(x = fct_rev(fct_reorder(ref_age_class, number)),
                               y = number,
                               fill = outcome)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values=c("#CC79A7", "#68a4bd")) +
    theme_economist() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Number of children rescreened and\noutcome achieved") +
    #coord_flip() +
    annotate("text", 
             x = "EI to KG", 
             y = 40,
             label = "Total number of\nchildren rescreened\nat least twice = 82", 
             size= 3,
             color = "red",
             hjust = 1)
  
  # tabName: Trainsition
  
  plot_tran_elig <- ggplot(child_ref_elig, 
                           aes(x = fct_rev(fct_reorder(elig_code, n)),
                               y = n,
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
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Number of Children Qualifying Based on Conditions during Transition")
  
  
  
# DNQ return
  
  dnq_return <- ggplot(child_dnq_return,
                       aes(reorder(return_category, n), n,
                           fill = return_category)) +
    geom_col(fill = "#5F9EA0", width = 0.8) +
    geom_text(aes(label = n), size = 4) +
    theme_economist() + 
    theme(axis.text.x = element_text(size = 10),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(vjust = -0.5)) +
    annotate("text",
             x = 4,
             y = 10,
             label = "Note: Number of unique\nDNQ children who returned\nat least once=76",
             size= 3,
             color = "red",
             hjust = 1) +
    labs(title = "Number of DNQ children who returned") +
    coord_flip()
  
  # TabName: Placement
  
  ## placement quater plot
  
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
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Quaterly Placement Outcome: 2016-2021")
  
  
  
  
  ## placement mean plot
  d <- placement_create_tbl %>%
    pivot_longer(-year,
                 names_to = "outcome",
                 values_to = "mean_value")
  d$outcome <- as.factor(d$outcome)
  
  g <- ggplot(d, aes(x = year, y = mean_value,
                     group = outcome, color = outcome, shape = outcome)) + 
    geom_point(size = 2) + geom_line(alpha = 0.5) +
    scale_color_manual(values=c("#4c4c4c", "#86BB8D", "#B22222", "#ff9900",
                                "#68a4bd", "#CC79A7", "#008000")) +
    scale_shape_manual(values = c(15, 16, 17, 3, 4, 18, 11)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Placement outcome(Mean) by Year", 
         y = "mean", x = "") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    guides(group = FALSE, shape = FALSE, size = FALSE)
  
  
  
  # TablName: Medi_fost
  
  plot_foster <- ggplot(child_data_foster,
                        aes(x = medi_fost, y = percent)) +
    geom_col(aes(fill = category), width = 0.4) +
    scale_fill_manual(values=c("#CC79A7", "#68a4bd")) +
    theme_economist() +
    theme(axis.title.x = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 12)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Number of children in foster care and with medicaid",
         y = "Percent")

# For Site_plot

## plot function to be used in server for plot and downloadhandler
site_plot <- ggplot(child_site_center, 
                    aes(reorder(Placement, Number), 
                        Number, fill = Region)) +
  geom_col(width = 0.5) +
  theme_economist() +
  scale_fill_manual(values=c("#4c4c4c", "#86BB8D", "#B22222", "#ff9900",
                             "#68a4bd", "#CC79A7", "#0074D9")) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.title = element_blank(), 
        legend.text=element_text(size=10)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Number of Children in each Placement by Region") +
  coord_flip()



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
               menuSubItem("Medicaid Foster", tabName = "Medicaid_Foster"),
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
          box(width = 12, plotlyOutput("plot",
                                      height = 550,
                                      width = "100%"),
              downloadButton(outputId = "plot_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;"))),
        fluidRow(
          box(width = 12, uiOutput("box_tbl"),
              downloadButton(outputId = "box_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Referral_Sources",
        fluidRow(
          box(width = 6, plotlyOutput("pri_ref",
                                      width = "100%",
                                      height = 500),
              downloadButton(outputId = "pri_ref_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          box(width = 6, plotlyOutput("sec_ref",
                                      width = "100%",
                                      height = 500),
              downloadButton(outputId = "sec_ref_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Rescreened",
        fluidRow(
          box(width = 7, plotlyOutput("rescreened_plot",
                                      width = "100%",
                                      height = 575),
              downloadButton(outputId = "rescreened_plot_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          
          box(width = 5, uiOutput("rescreened_tbl"),
              downloadButton(outputId = "rescreened_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Eligibility_Condition",
        fluidRow(
          box(width = 12, plotlyOutput("elig_cond_plot",
                                       width = "100%",
                                       height = 500),
              downloadButton(outputId = "elig_cond_plot_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;"))
          ),
        
        fluidRow(
          box(width = 12, uiOutput("elig_cond_tbl"),
              downloadButton(outputId = "elig_cond_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Placement",
        fluidRow(
          box(width = 6, plotlyOutput("placement_plot"),
              downloadButton(outputId = "placement_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          box(width = 6, plotlyOutput("placement_mean_plot"),
              downloadButton(outputId = "placement_mean_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;"))),
        fluidRow(
          box(width = 12, uiOutput("placement_tbl"),
              downloadButton(outputId = "placement_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Transition",
        fluidRow(
          box(width = 7, plotlyOutput("transition_plot",
                                      width = "100%",
                                      height = 550),
              downloadButton(outputId = "transition_plot_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          
          box(width = 5, uiOutput("transition_tbl"),
              downloadButton(outputId = "transition_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "DNQ_Return",
        fluidRow(
          box(width = 8, plotlyOutput("dnq_return_plot",
                           width = "100%", 
                           height = "550px"),
              downloadButton(outputId = "dnq_return_plot_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          
          box(width = 4, uiOutput("dnq_return_tbl",
                                  width = "100%",
                                  height = "800px"),
              downloadButton(outputId = "dnq_return_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Medicaid_Foster",
        fluidRow(
          box(plotlyOutput("medi_fost_plot"),
              downloadButton(outputId = "medi_fost_plot_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          
          box(uiOutput("medi_fost_tbl"),
              downloadButton(outputId = "medi_fost_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "Site_Center",
        fluidRow(
          box(width = 7, plotlyOutput("site_plot",
                                      width = "100%",
                                      height = "600"),
              downloadButton(outputId = "site_plot_dl",
                             label = "Download Plot",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")),
          
          box(width = 5, uiOutput("site_tbl"),
              downloadButton(outputId = "site_tbl_dl",
                             label = "Download Data",
                             style="color: #fff; background-color: #00a5c1; border-color: Black;")))
        ),
      
      tabItem(
        tabName = "tab_about",
        fluidRow(
          box(title= h2("About this Project"),
              width = "6 col-lg-4",
              tags$p(
                class = "text-center",
                tags$img(class = "img-responsive",
                         src = "eccares_logo.png", 
                         style = "max-width: 150px;")
                ),
              
              tags$p("Early Childhood Cares (EC Cares) provides early",
                     "intervention and early childhood",
                     "special education services to infants,",
                     "toddlers, and prechool chidren in Lane",
                     "County in Oregon."),
              tags$p("This project was developed as part",
                     "of the Data Sciene Capstone project.",
                     "The purpose was to track trends and",
                     "address the need for data use for",
                     "accountability, improvements, and ",
                     "operations at the EI program level."),
              
              tags$p(h2("Data Source"),
                     "The data for the dashboard was provided",
                     "by EC Cares. Data came from monthly reports",
                     "from 2015-2021, and from children currently",
                     "enrolled in EC Cares.")
              ),
          
          box(title = h2("Contact"),
              width = "6 col-lg-4",
              tags$p("Your suggestions, feedback",
              "complaints or compliments are",
              "highly valued and will guide",
              "us to improve the dashboard.",
              "Please email them to:",
              HTML(paste0(tags$a(href = "mailto:ayadav@uoregon.edu",
                                 "ayadav@uoregon.edu"), ".")),
              tags$p(h2("About the Developer")),
              tags$p("This dashboard is developed by Asha Yadav.",
                     "Asha is currently pursuing Ph.D.in Special Education",
                     "at the University of Oregon. Connect with her at:",
                     HTML(paste0(tags$a(href = "mailto:ayadav@uoregon.edu",
                                        "ayadav@uoregon.edu"), ".")),
              tags$p(h3("Interests:")),
              tags$ul(
                tags$li("Children and Family lives"),
                tags$li("Data Science in R"))
                     ))
              ),
          
          box(title = h2("About this Dashboard"),
              width = "6 col-lg-4",
              tags$p(
                class = "text-center",
                tags$a(
                  href = "https://rstudio.com",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "RStudio.png",
                           style = "max-width: 150px;"
                           )
                  ),
                tags$a(
                  href = "https://shiny.rstudio.com/",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "shiny.png",
                           style = "max-width: 150px; margin-left: 2em;"
                           )
                  ),
                tags$a(
                  href = "https://www.tidyverse.org/",
                  target = "_blank",
                  tags$img(class = "image-responsive",
                           src = "tidyverse.png",
                           style = "max-width: 150px; margin-left: 2em;"
                           )
                  )
                ),
              tags$p(
                "This dashboard was built in",
                tags$a(href = "https://r-project.org", target = "_blank", "R"),
                "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
                tags$strong("shiny,"),
                tags$strong("shinydashboard,"),
                tags$strong("ggplot2,"),
                tags$strong("plotly,"),
                "the", tags$strong("tidyverse,"),
                "and many more packages."
                
              )
              )
          )
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
      
      ggplotly(create_plot(input$var)) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      })
    
    output$plot_dl <- downloadHandler(
      filename = function() {
        paste("plot", ".png", sep = "")
      },
      content = function(file) {
        
        png(file)
        print(create_plot(input$var))
        dev.off()
      }
    )
    
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
            highlight = TRUE,
            width = "1200",
            height = 400,
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(4, 10, 14),
            defaultPageSize = 5))
      
      })
    
    output$box_tbl_dl <- downloadHandler(
      filename = function(){
        paste("box_tbl", "csv", sep = ".")
      },
      content = function(file){
        write.csv(create_tbl(input$var), file)
      }
    )
    
    
   
    #3 Parent tab : Child-level data
    
    
    #3.1 tabName = "Referral Sources"
    
    output$pri_ref <- renderPlotly({
      ggplotly(plot_pri_outcome) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
        
      })
    
    
    output$pri_ref_dl <- downloadHandler(
      filename = function() {
        paste("pri_ref_plot", ".png", sep = "")
      },
      content = function(file) {
        # export plotly html widget as a temp file to download.
      png(file)
        print(plot_pri_outcome)
        dev.off()
      }
    )
    
    
    
    output$sec_ref <- renderPlotly({
  
      ggplotly(plot_secondary) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      })
    
    
    
    output$sec_ref_dl <- downloadHandler(
      filename = function() {
        paste("sec_ref_plot", ".png", sep = "")
      },
      content = function(file) {
        
        png(file)
        print(plot_secondary)
        dev.off()
      }
    )
    
    
    #3.2 tabName = "Rescreened"
    
    output$rescreened_plot <- renderPlotly({
      
     
      ggplotly(plot_rescreened) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove =
                 custom_modebar)
      
      
    })
    
    output$rescreened_plot_dl <- downloadHandler(
      filename = function() {
        paste("rescreened_plot", ".png", sep = "")
      },
      content = function(file) {
        # export plotly html widget as a temp file to download.
        png(file)
        print(plot_rescreened)
        dev.off()
      }
    )
    
    
    
    output$rescreened_tbl <- renderUI({
      
      box(title = "Rescreened",
          reactable(child_ref_rescreen,
                    highlight = TRUE,
                    width = 450,
                    height = 500))
      })
    
    output$rescreened_tbl_dl <- downloadHandler(
      filename = function(){
        paste("rescreened_tbl", "csv", sep = ".")
      },
      content = function(file){
        write.csv(child_ref_rescreen, file)
      }
      )
    
    
    # Specifying the dataset used in the reactive table to download
    
    
    #3.3 tabName = "Eligibility Condition"
    
    output$elig_cond_plot <- renderPlotly({
      
      ggplotly(plot_elig) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    output$elig_cond_plot_dl <- downloadHandler(
      filename = function() {
        paste("elig_plot", ".png", sep = "")
      },
      content = function(file) {
        png(file)
        print(plot_elig)
        dev.off()
      }
    )
    
    output$elig_cond_tbl <- renderUI({
      
      box(title = "Primary Eligibility Condition",
          reactable(elig_cond,
                    highlight = TRUE,
                    width = 1200,
                    showPageSizeOptions = TRUE, 
                    pageSizeOptions = c(5, 9, 13), 
                    defaultPageSize = 5))

      })
    
    output$elig_cond_tbl_dl <- downloadHandler(
      filename = function(){
        paste("elig_cond_tbl", "csv", sep = ".")
      },
      content = function(file){
        write.csv(elig_cond, file)
      }
    )
   
   
  
    #3.4 tabName = "Placement"
    
    output$placement_plot <- renderPlotly({
      
      ggplotly(plot_placement) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    output$placement_dl <- downloadHandler(
      filename = function() {
        paste("placement_plot", ".png", sep = "")
      },
      content = function(file) {
        png(file)
        print(plot_placement)
        dev.off()
      }
    )
    
    
    
    output$placement_mean_plot <- renderPlotly({
      
      ggplotly(g) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    output$placement_mean_dl <- downloadHandler(
      filename = function() {
        paste("placement_mean_plot", ".png", sep = "")
      },
      content = function(file) {
        png(file)
        print(g)
        dev.off()
      }
    )
    
    output$placement_tbl <- renderUI({
      
      box(title = "Placement Outcome(Mean)", 
          reactable(placement_create_tbl,
                    highlight = TRUE,
                    width = 1200))
      })
    
    output$placement_tbl_dl <- downloadHandler(
      filename = function(){
        paste("placement_tbl", "csv", sep = ".")
        },
      content = function(file){
        write.csv(placement_create_tbl, file)
        }
      )
    
    
   
    #3.5 tabName = "Transition"
    
    output$transition_plot <- renderPlotly({
      
      ggplotly(plot_tran_elig) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    
    output$transition_plot_dl <- downloadHandler(
      filename = function() {
        paste("transition_plot", ".png", sep = "")
        },
      
      content = function(file) {
        png(file)
        print(plot_tran_elig)
        dev.off()
      }
    )
    
  
    
    output$transition_tbl <- renderUI({
      
      
      box(title = "Qualifying Conditions:Transition", 
          reactable(child_ref_elig,
                    highlight = TRUE, 
                    showPageSizeOptions = TRUE,
                    pageSizeOptions = c(5, 11),
                    defaultPageSize = 5,
                    width = 450,
                    height = 475))
      })
    
    output$transition_tbl_dl <- downloadHandler(
      filename = function(){
        paste("transition_tbl", "csv", sep = ".")
        },
      content = function(file){
        write.csv(child_ref_elig, file)
      })
    
   
    
    #3.6 tabName = "DNQ Return"
    
    output$dnq_return_plot <- renderPlotly({
      
      ggplotly(dnq_return) %>%
        style(textposition = "right") %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    
    output$dnq_return_plot_dl <- downloadHandler(
      filename = function() {
        paste("dnq_return_plot", ".png", sep = "")
      },
      
      content = function(file) {
        png(file)
        print(dnq_return)
        dev.off()
      }
    )
    
    
    
    output$dnq_return_tbl <- renderUI({
      
      child_dnq_return <- child_dnq_return %>%
        select(Inactive, Placement, n)
      
      box(title = "Returning children: DNQ",
      reactable(child_dnq_return,
                highlight = TRUE,
                height = 500,
                width = 350))
      })
    
    output$dnq_return_tbl_dl <- downloadHandler(
      filename = function(){
        paste("dnq_return_tbl", "csv", sep = ".")
      },
      content = function(file){
        write.csv(child_dnq_return, file)
      }
      )
    
    
    #3.7 tabName = "Medicaid/Foster"
    
    output$medi_fost_plot <- renderPlotly({
      
      
      ggplotly(plot_foster) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      })
    
    output$medi_fost_plot_dl <- downloadHandler(
      filename = function() {
        paste("medi_fost_plot", ".png", sep = "")
      },
      
      content = function(file) {
        png(file)
        print(plot_foster)
        dev.off()
      }
    )
    
    
    output$medi_fost_tbl <- renderUI({
      
      box(title = "Medicaid & Foster care",
          reactable(child_data_foster,
                    highlight = TRUE,
                    width = 500,
                    height = 325))
    })
    
    output$medi_fost_tbl_dl <- downloadHandler(
      filename = function(){
        paste("medi_fost_tbl", "csv", sep = ".")
      },
      content = function(file){
        write.csv(child_data_foster, file)
      }
    )
    #3.8 tabName = "Site Center"
    
    output$site_plot <- renderPlotly({
      
     ggplotly(site_plot) %>%
        config(displaylogo = F,
               modeBarButtonsToRemove = 
                 custom_modebar)
      
      
    })
    
    output$site_plot_dl <- downloadHandler(
      filename = function() {
        paste("site_plot", ".png", sep = "")
      },
      
      content = function(file) {
        png(file)
        print(site_plot)
        dev.off()
      }
    )
    
    
    output$site_tbl <- renderUI({
      
      child_site_center <- child_site_center[, c("Region", "Placement", "Number")]
      
      box(title = "Placement by Region", 
          reactable(child_site_center,
                    groupBy = "Region",
                    highlight = TRUE,
                    width = 460,
                    height = 550))
      })
    
    output$site_tbl_dl <- downloadHandler(
      
      filename = function(){
        paste("site_tbl", "csv", sep = ".")
        },
      content = function(file){
        write.csv(child_site_center, file)
      }
    )
  }
  
  
  
  shinyApp(ui, server)
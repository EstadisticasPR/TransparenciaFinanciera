library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

## UI App Header
header <- dashboardHeader(title = tags$a(href='https://estadisticas.pr',
                                         tags$img(src='iepr_esp.png', 
                                                  height="100%",
                                                  width="50%")))

## UI App Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Agencias", tabName = "agencias"), 
    menuItem("Cuentas", tabName = "cuentas"),
    menuItem("Personas", tabName = "personas"),
    menuItem("Explorador de Datos", tabName = "explorer"),
    uiOutput("year_slider")
  )
)

## Spanish Description about the project
spanish_about_short = "El Instituto de Estadísticas de Puerto Rico es 
una entidad gubernamental autónoma creada mediante la Ley Núm. 209-2003 
para coordinar el servicio de producción de estadísticas del Gobierno 
del Estado Libre Asociado de Puerto Rico y asegurar que los sistemas de 
recopilación de datos y estadísticas -en los que se basan las políticas 
públicas- estén completos, sean confiables y de acceso rápido y universal.

Existe una gran necesidad y reclamo para mejorar la accesibilidad y 
la calidad de la información financiera de los gastos del Gobierno.

A través del portal de Transparencia Financiera, el Instituto de 
Estadísticas persigue lograr la coordinación así como la participación de 
todas las entidades gubernamentales para que provean la información de sus 
gastos. Esto con el fin de impulsar una cultura de apertura en la 
administración pública en Puerto Rico.

Por este medio, el Instituto de Estadísticas invita a todas las entidades 
gubernamentales a sumarse a este esfuerzo, el cual no requiere recursos ni 
conocimiento técnico, ni tampoco compromete recursos tecnológicos 
existentes de las entidades.

Al momento, las siguientes entidades públicas han respondido a este llamado:

Instituto de Cultura Puertorriqueña: desde año fiscal 2014-15

Instituto de Estadísticas de Puerto Rico: desde año fiscal 2007-08"

## English Description about the proyect
english_about_short = "The Puerto Rico Institute of Statistics (PRIS) is an 
autonomous public entity created by Puerto Rico Act No. 209-2003, as amended, 
with the mission of coordinating the statistical production of the Government
of Puerto Rico and to ensure that the data collection systems produce 
comprehensive and reliable statistics that are timely and universally 
accessible.

There is a great need and demand for improvements to the accessibility and 
quality of the financial information of the Government of Puerto Rico.

Through the Financial Transparency site, PRIS seeks to achieve the coordination
and participation of all public entities of the Government of Puerto Rico, in 
order to promote a culture of openness in the public administration of Puerto Rico.

PRIS invites all public entities of the Government of Puerto Rico to join this effort, 
which does not cost anything, nor require specialized skills, 
nor commit existing technological resources of the entities.

The following public entities have joined this effort thus far:

Institute of Puerto Rican Culture: from FY 2014-15 onward

Puerto Rico Institute of Statistics from FY 2007-08 onward."

## UI App Body
body <- dashboardBody(
  useShinyalert(),
  useShinyjs(),
  tabItems(
    ## Home UI Content
    tabItem(tabName = "home", 
            h2("Transparencia Financiera"),
            
            ## Summaries
            fluidRow(
              column(1),
              infoBoxOutput("total_spending"),
              column(2),
              infoBoxOutput("num_transactions")
            ),
            
            # Full Comparison Graph
            fluidRow(
              column(1),
              column(1,
                     actionButton("full_graph_bttn",
                                  "Mostrar más años"))
            ),
            
            fluidRow(
              p(),
              column(2),
              column(8,
                     hidden(
                       plotOutput('full_graph_plot')
                     )
              ),
              p()
            ),

            
            ## Text Descriptions
            fluidRow(
              column(1),
              column(4, 
                     h3("Sobre El Proyecto"),
                     p(spanish_about_short)),
              column(2),
              column(4,
                     h3("About The Project"),
                     p(english_about_short)),
              column(1)
            )
    ),
    tabItem(tabName = "agencias",
            h2("Datos por Agencia"),
            fluidRow(
              column(4),
              column(4, valueBoxOutput("top_agency", width = NULL)),
              column(4)
            ),
            fluidRow(
              column(2),
              tabBox(
                tabPanel("Tab 1", plotlyOutput("agency_year_plot")),
                tabPanel("Tab 2", plotlyOutput("agency_month_plot")),
                tabPanel("Tab 3", plotlyOutput("agency_qtr_plot")),
                tabPanel("Tab 4", plotlyOutput("agency_agency_plot")),
                width = 8),
              column(2)
            ),
            fluidRow(
              column(2),
              column(8,  DT::dataTableOutput("agency_table")),
              column(2)
            )
    ),
    tabItem(tabName = "cuentas", 
            h3("Datos por tipo de Cuenta"),
            fluidRow(
              column(4),
              column(4, valueBoxOutput("top_type", width = NULL)),
              column(4)
            ),
            fluidRow(
              column(2),
              tabBox(
                tabPanel("Tab 1", plotlyOutput("account_year_plot")),
                tabPanel("Tab 2", plotlyOutput("account_month_plot")),
                tabPanel("Tab 3", plotlyOutput("account_qtr_plot")),
                tabPanel("Tab 4", plotlyOutput("account_account_plot")),
                width = 8),
              column(2)
            ),
            fluidRow(
              column(2),
              column(8, DT::dataTableOutput("account_table")),
              column(2)
            )
    ),
    tabItem(tabName = "personas",
            h3("Datos por Persona"), 
            fluidRow(
              column(4),
              column(4, valueBoxOutput("top_person", width = NULL)),
              column(4)
            ),
            fluidRow(
              column(2),
              tabBox(
                tabPanel("Tab 1", plotlyOutput("person_year_plot")),
                tabPanel("Tab 2", plotlyOutput("person_month_plot")),
                tabPanel("Tab 3", plotlyOutput("person_qtr_plot")),
                tabPanel("Tab 4", plotlyOutput("person_person_plot")),
                width = 8),
              column(2)
            ),
            fluidRow(
              column(2),
              column(8, DT::dataTableOutput("person_table"))
            )
    ),
    tabItem(tabName = "explorer",
            h3("Data Explorer"))
  )
)

ui <- dashboardPage(skin="purple", header, sidebar, body)

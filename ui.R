## Spanish Description about the project

spanish_about_short = "Transparencia Financiera es parte de nuestro compromiso con mejorar la 
transparencia del Gobierno de Puerto Rico. Al momento, provee información detallada de las 
transacciones del Instituto de Cultura Puertorriqueña y del Instituto de Estadísticas de Puerto 
Rico en distintos años. Esto con la intención de servir de ejemplo y motivar a todas las 
entidades públicas a hacer lo propio."

## English Description about the proyect
english_about_short = "This site is part of our commitment to improve the transparency of the 
Government of Puerto Rico. Currently, it provides detailed information on the transactions of 
the Institute of Puerto Rican Culture and of the Puerto Rico Institute of Statistics in different 
years. It is our intention to serve as an example and to motivate all public entities of the 
Government of Puerto Rico to do the same."

library(shiny)
library(DT)
library(rbokeh)
library(lubridate)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(theme = shinytheme("sandstone"), 
             "Sistema de Transparencia Financiera de Puerto Rico",
             tabPanel("Página Principal", icon = icon("home"),
                      fluidPage(
                        fluidRow(
                          h1(class="text-center", "Sistema de Transparencia Financiera de Puerto Rico"),
                            column(2),
                            column(8, 
                                   tabsetPanel(
                                   tabPanel("Gasto Anual por Agencia", rbokehOutput("agency_year_plot")),
                                   tabPanel("Gasto Trimestral por Agencia", rbokehOutput("agency_qtr_plot")),
                                   tabPanel("Gasto Mensual por Agencia", rbokehOutput("agency_month_plot"))
                                 )
                               ),
                        column(2)
                        ),
                        fluidRow(
                          column(1),
                          column(4,
                                 h3(class="text-center", "Sobre el Proyecto"),
                                 p(class="text-justify", spanish_about_short)),
                          column(2),
                          column(4, 
                                 h3(class="text-center", "About the Project"),
                                 p(class="text-justify", english_about_short)),
                          column(1)
                        ),
                        hr(),
                        h3( class="text-center", "Agencias Participantes"),
                        fluidRow(
                          column(2),
                          column(8, DT::dataTableOutput("agency_table")),
                          column(2)
                        )
                      )
              ),
             tabPanel("Tipo de Gastos", icon = icon("dollar"),
                      h3(class="text-center","Datos por Tipo de Gasto"),
                      fluidRow(
                        column(4),
                        column(4),
                        column(4)
                      ),
                      fluidRow(
                        column(2),
                        column(8,
                               tabsetPanel(
                                 tabPanel("Gasto Anual por Tipo de Gasto", rbokehOutput("account_year_plot")),
                                 tabPanel("Gasto Trimestral por Tipo de Gasto", rbokehOutput("account_qtr_plot")),
                                 tabPanel("Gasto Mensual por Tipo de Gasto", rbokehOutput("account_month_plot")))),
                        column(2)
                        ),
                      hr(),
                      fluidRow(
                        column(2),
                        column(8, DT::dataTableOutput("account_table")),
                        column(2)
                      )
                      ),
             tabPanel("Personas", icon = icon("users"), 
                      h3(class="text-center","Datos por Persona"),
                      fluidRow(
                        column(2),
                        column(8, tabsetPanel(
                          tabPanel("Gasto Anual por Persona", rbokehOutput("person_year_plot")),
                          tabPanel("Gasto Trimestral por Persona", rbokehOutput("person_qtr_plot")),
                          tabPanel("Gasto Mensual por Persona", rbokehOutput("person_month_plot"))
                          )),
                        column(2)
                      ),
                      hr(),
                      fluidRow(
                        column(2),
                        column(8, DT::dataTableOutput("person_table"))
                      )),
             tabPanel("Explorador de Datos", icon = icon("signal"),
                      fluidPage(
                        titlePanel("Explorador de Datos"),
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       helpText("Seleccione las variables que interesa explorar:"),
                                       selectInput("indvar", 
                                                   label = "",
                                                   choices = list("Mes", "Año", "Tipo de Gasto", "Persona", "Agencia"),
                                                   selected = "Año"),
                                       selectInput("depvar",
                                                   label = "",
                                                   choices = list("Tipo de Gasto", "Persona", "Agencia"),
                                                   selected = "Tipo de Gasto")),
                          mainPanel(
                            rbokehOutput("data_explorer_plot"))
                      ))),
             tabPanel("Descarga de Datos", icon = icon("download"), 
                      h3("Downloader"),
                      h5(class = 'text-left', 'En esta página puede seleccionar los datos que desea descargar 
                        de el Sistema de Transparencia Financiera de Puerto Rico, luego presiona el botón de 
                        descarga y descargara un archivo en formato CSV.'),
                      p(class = 'text-center', downloadButton('dwn_bttn', "Descargar Datos Seleccionados")),
                      fluidRow(
                        column(1),
                        column(10, DT::dataTableOutput("table_download")),
                        column(1)
                      )),
             collapsible = T, fluid = T
  )
)

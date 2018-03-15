## app.R ##
library(shiny)
library(tidyverse)
library(shinyjs)
library(shinydashboard)
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
    menuItem("Top Spending", tabName = "top", icon = icon("money")), 
    menuItem("Visualizaciones", tabName = "viz", icon = icon("signal"))
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
            
            ## Full Comparison Graph
            fluidRow(
              column(1),
              column(1, 
                     actionButton("full_graph_bttn", 
                                  "Mostrar más años"))),
            
            fluidRow(
              p(),
              column(1),
              column(10, 
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
    
    ## Top UI Content
    tabItem(tabName = "top",
            h2("Top Spending"),
            fluidRow(
                     infoBoxOutput("top_agency"),
                     infoBoxOutput("top_type"),
                     infoBoxOutput("top_person")
            ),
            fluidRow(
              column(4, DT::dataTableOutput("agency_table")),
              column(4, DT::dataTableOutput("account_table")),
              column(4, DT::dataTableOutput("person_table"))
            )
    ),
    
    ## Viz UI Content
    tabItem(tabName = "viz",
            h2("Datos por Agencia"), 
            fluidRow(
              column(6, plotlyOutput("agency_year_plot"), p()),
              column(6, plotlyOutput("agency_month_plot"), p())
            ), 
            fluidRow(
              column(6, plotlyOutput("agency_qtr_plot"), p()),
              column(6, plotlyOutput("agency_agency_plot"), p())
            ), 
            p(), 
            p(),
            h3("Datos por tipo de Cuenta"),
            fluidRow(
              column(6, plotlyOutput("account_year_plot")),
              column(6, plotlyOutput("account_month_plot"))
            ),
            p(),
            p(),
            fluidRow(
              column(6, plotlyOutput("account_qtr_plot")),
              column(6, plotlyOutput("account_account_plot"))
            ), 
            p(),
            p(),
            fluidRow(
              column(6, plotlyOutput("person_year_plot")),
              column(6, plotlyOutput("person_month_plot"))
            ),
            p(),
            p(),
            fluidRow(
              column(6, plotlyOutput("person_qtr_plot")),
              column(6, plotlyOutput("person_person_type"))
            ))
  )
)

ui <- dashboardPage(skin="green", header, sidebar, body)


server <- function(input, output, session) {
  
  data <- read.csv("transparencia.csv", na.strings = c("", "NA"))
  
  ######################## HOME TAB ##################################
  
  ## InfoBox total gasto anual
  total_spent = sum(data$amount[data$fiscal_year == 2018], na.rm = TRUE)
  s_total_spent = paste0("$", round(total_spent/1e6, 3), " Millones")
  
  output$total_spending <- renderInfoBox({
    infoBox("Gastos Totales en el 2018", s_total_spent, 
            icon = icon("money"), color = "purple")
    })
  
  ##InfoBox total de transacciones
  trans_num = length(data$amount[data$fiscal_year == 2018])
  trans_num = paste0(trans_num, " Transacciones")
  
  output$num_transactions <- renderInfoBox({
    infoBox("Numero de Transacciones", trans_num, 
            icon = icon("users"), color = "purple")
  })

  ## Full Graph Comparison
  observeEvent(input$full_graph_bttn, {toggle("full_graph_plot")})
  
  output$full_graph_plot <- renderPlot({
    ggplot(data = subset(data, !is.na(fiscal_year | amount))) + 
             geom_bar(mapping=aes(x = fiscal_year,
                                  y = amount), 
                                  stat = "identity") +
    labs(x = "Año Fiscal",
         y = "Cantidad Total Anual",
         title = "Gasto Anual Total",
         subtitle = "IEPR & ICP") +
      theme_minimal()
  })
  
  ##################### TOP TAB ###########################
  
  #### Top Agency ####
  
  # Top Agency Spending InfoBox
  agency_data <- subset(data, select = c("department","amount"))
  
  agency_data <- aggregate(amount~department, agency_data, sum)
  agency_data$amount = agency_data$amount/1e6
  agency_data <- agency_data[order(-agency_data$amount)]
  
  
  top_ag <- as.character(agency_data$department[1])
  amnt_ag <- round(sum(agency_data$amount[agency_data$department == top_ag], na.rm = TRUE),3)
  
  output$top_agency <- renderInfoBox({
    infoBox("Agencia", paste0(top_ag,": $", amnt_ag, " Millones"))
  })
  
  # Top Agency Spending Data Table
  
  
  output$agency_table <- DT::renderDataTable({
    datatable(agency_data, colnames = c("Agencia", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  #### Type of Spending ####
  
  # Type of Spending InfoBox
  account_data <- subset(data, select = c("account","amount"))
  
  account_data <- aggregate(amount~account, account_data, sum)
  account_data$amount <- account_data$amount/1e6
  account_data <- account_data[order(-account_data$amount),]
  
  top_tp <- as.character(account_data$account[1])
  amnt_tp <- round(sum(account_data$amount[account_data$account == top_tp], na.rm = TRUE),3)
  
  output$top_type <- renderInfoBox({
    infoBox("Tipo de Cuenta", paste0(top_tp, ": $", amnt_tp, " Millones"))
  })

  # Type of Spending Data Table
  output$account_table <- DT::renderDataTable({
    datatable(account_data, colnames = c("Tipo de Cuenta", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  #### Person ####
  
  # Person InfoBox
  person_data <- subset(data, select = c("name", "amount"))
  
  person_data <- aggregate(amount~name, person_data, sum)
  person_data$amount <- person_data$amount/1e6
  person_data <- person_data[order(-person_data$amount),]
  
  top_pers <- as.character(person_data$name[1])
  amnt_pers <- round(sum(person_data$amount[person_data$name == top_pers], na.rm = TRUE), 3)
  
  output$top_person <- renderInfoBox({
    infoBox("Persona", paste0(top_pers, ": $", amnt_pers, " Millones"))
  })
  
  # Person Data Table
  output$person_table <- DT::renderDataTable({
    datatable(person_data, colnames = c("Nombre", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  ################ Viz Tab ##########################
  
  # Agency per Year
  year_amount = aggregate(amount~fiscal_year+department, data, sum)
  year_amount$amount = year_amount$amount/1e6
  
  agency_yearly <- ggplot(data = year_amount) +
    geom_line(mapping = aes(x = fiscal_year, y = amount, colour = department)) +
    labs(x = "Año Fiscal",
         y = "Cantidad en Millones de $",
         title = "Gasto Agencial Anual", 
         colour = "Agencia")
  
  output$agency_year_plot <- renderPlotly({
    ggplotly(agency_yearly)
  })
  
  # Agency per Month
  month_amount = aggregate(amount~fiscal_year_period+department+fiscal_year, data, sum)
  month_amount$amount = month_amount$amount/1e6
  month_amount$dates <- paste0(month_amount$fiscal_year, "/", month_amount$fiscal_year_period, "/1")
  
  month_amount$dates <- as.Date(month_amount$dates)
  
  agency_monthly <- ggplot(data = month_amount) +
    geom_line(mapping = aes(x = month_amount$dates, y = amount, colour = department)) +
    labs(x = "Mes del Año Fiscal", 
         y = "Cantidad en Millones de $",
         title = "Gasto Agencial Mensual",
         colour = "Agencia")
  
  output$agency_month_plot <- renderPlotly({
    ggplotly(agency_monthly)
  })
  
  # Agency per QTR
  month_amount$qtr <- numeric(length(month_amount$fiscal_year_period))
  for (i in 1:length(month_amount$fiscal_year_period)){
    if (month_amount$fiscal_year_period[i] >= 1 && month_amount$fiscal_year_period[i] <= 3){
      month_amount$qtr[i] = paste0(month_amount$fiscal_year[i], "-1")
    } else if (month_amount$fiscal_year_period[i] >= 4 && month_amount$fiscal_year_period[i] <= 6){
      month_amount$qtr[i] = paste0(month_amount$fiscal_year[i], "-2")
    } else if (month_amount$fiscal_year_period[i] >= 7 && month_amount$fiscal_year_period[i] <= 9){
      month_amount$qtr[i] = paste0(month_amount$fiscal_year[i], "-3")
    } else if (month_amount$fiscal_year_period[i] >= 10 && month_amount$fiscal_year_period[i] <= 12){
      month_amount$qtr[i] = paste0(month_amount$fiscal_year[i], "-4")
    }
  }
  
  qtr_amount = aggregate(amount~qtr+department, month_amount, sum)
  
  agency_qtr <- ggplot(data = qtr_amount) +
    geom_line(mapping = aes(x = qtr, y = amount, colour = department, group = 1)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank()) + 
    labs(x = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), " - ", max(data$fiscal_year), ")"), 
         y = "Cantidad en Millones de $",
         title = "Gasto Agencial por Trimestre",
         colour = "Agencia")
  
  output$agency_qtr_plot <- renderPlotly({
    ggplotly(agency_qtr)
  })
  
  # Agency per Agency
  
  # todos los años
  agency_amount = aggregate(amount~department, data, sum)
  agency_amount$amount = agency_amount$amount/1e6
  
  agency_agency <- ggplot(data = agency_amount) + 
    geom_bar(mapping = aes(x = department, y = amount, fill = department), stat = "identity") + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank()) + 
    labs(x = "Agencia",
         y = "Cantidad en Millones de $",
         title = "Gasto Agencial Total",
         fill = "Agencia")
  
  output$agency_agency_plot <- renderPlotly({
    ggplotly(agency_agency)
  })
  
  # Account Year
  acc_year = aggregate(amount~account, data, sum)
  acc_year = acc_year[order(-acc_year$amount),]
  top_acc = acc_year[1:10, 1]
  
  acc_year_amount = aggregate(amount~fiscal_year+account, data, sum)
  acc_year_amount$amount = acc_year_amount$amount/1e6
  
  acc_year_amount$top5 = numeric(length(acc_year_amount$account))
  
  for (i in 1:length(acc_year_amount$account)){
    if (acc_year_amount$account[i] %in% top_acc){
      acc_year_amount$top5[i] = "top5"
    } else {
      acc_year_amount$top5[i] = "notTop5"
    }
  }
  
  acc_year_amount = subset(acc_year_amount, top5 == "top5")
  
  acc_year_plot <- ggplot(data = acc_year_amount) +
    geom_line(mapping = aes(x = fiscal_year, y = amount, colour = account)) +
    labs(x = "Año Fiscal",
         y = "Cantidad en Millones de $",
         title = "Gasto por Tipo de Cuenta Anual", 
         colour = "Tipo de Cuenta")
  
  output$account_year_plot <- renderPlotly({
    ggplotly(acc_year_plot)
  })
  
  # Account Month
  acc_month_amount = aggregate(amount~fiscal_year_period+account+fiscal_year, data, sum)
  acc_month_amount$amount = acc_month_amount$amount/1e6
  acc_month_amount$dates <- paste0(acc_month_amount$fiscal_year, "/", acc_month_amount$fiscal_year_period, "/1")
  
  acc_month_amount$dates <- as.Date(acc_month_amount$dates)
  
  acc_month_amount$top5 = numeric(length(acc_month_amount$account))
  
  for (i in 1:length(acc_month_amount$account)){
    if (acc_month_amount$account[i] %in% top_acc){
      acc_month_amount$top5[i] = "top5"
    } else {
      acc_month_amount$top5[i] = "notTop5"
    }
  }
  
  acc_month_amount = subset(acc_month_amount, top5 == "top5")
  
  acc_month_plot <- ggplot(data = acc_month_amount) +
    geom_line(mapping = aes(x = dates, y = amount, colour = account)) +
    labs(x = "Mes del Año Fiscal", 
         y = "Cantidad en Millones de $",
         title = "Gasto por Tipo de Cuenta Mensual",
         colour = "Tipo de Cuenta")
  
  output$account_month_plot <- renderPlotly({
    ggplotly(acc_month_plot)
  })
  
  # Account QTR
  acc_month_amount$qtr <- numeric(length(acc_month_amount$fiscal_year_period))
  for (i in 1:length(acc_month_amount$fiscal_year_period)){
    if (acc_month_amount$fiscal_year_period[i] >= 1 && acc_month_amount$fiscal_year_period[i] <= 3){
      acc_month_amount$qtr[i] = paste0(acc_month_amount$fiscal_year[i], "-1")
    } else if (acc_month_amount$fiscal_year_period[i] >= 4 && acc_month_amount$fiscal_year_period[i] <= 6){
      acc_month_amount$qtr[i] = paste0(acc_month_amount$fiscal_year[i], "-2")
    } else if (acc_month_amount$fiscal_year_period[i] >= 7 && acc_month_amount$fiscal_year_period[i] <= 9){
      acc_month_amount$qtr[i] = paste0(acc_month_amount$fiscal_year[i], "-3")
    } else if (acc_month_amount$fiscal_year_period[i] >= 10 && acc_month_amount$fiscal_year_period[i] <= 12){
      acc_month_amount$qtr[i] = paste0(acc_month_amount$fiscal_year[i], "-4")
    }
  }
  
  acc_qtr_amount = aggregate(amount~qtr+account, acc_month_amount, sum)
  
  
  acc_qtr_plot <- ggplot(data = acc_qtr_amount) +
    geom_line(mapping = aes(x = qtr, y = amount, colour = account, group = 1)) +
    labs(x = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), " - ", max(data$fiscal_year), ")"), 
         y = "Cantidad en Millones de $",
         title = "Gasto por Tipo de Cuenta por Trimestre",
         colour = "Tipo de Cuenta") + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$account_qtr_plot <- renderPlotly({
    ggplotly(acc_qtr_plot)
  })
  
  # Account Account
  acc_acc_year = acc_year[1:10, ]
  acc_acc_year$amount = acc_acc_year$amount/1e6
  
  acc_acc_plot <- ggplot(data = acc_acc_year) + 
    geom_bar(mapping = aes(x = account, y = amount, fill = account), stat = "identity") + 
    labs(x = "Tipo de Cuenta",
         y = "Cantidad en Millones de $",
         title = "Gasto por Tipo de Cuenta Total",
         fill = "Tipo de Cuenta") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$account_account_plot <- renderPlotly({
    ggplotly(acc_acc_plot)
  })
  
  # Person Anual
  name_year = aggregate(amount~name, data, sum)
  name_year = name_year[order(-name_year$amount),]
  top_nm = name_year[1:10, 1]
  
  name_year_amount = aggregate(amount~fiscal_year+name, data, sum)
  name_year_amount$amount = name_year_amount$amount/1e6
  
  name_year_amount$top5 = numeric(length(name_year_amount$name))
  
  for (i in 1:length(name_year_amount$name)){
    if (name_year_amount$name[i] %in% top_nm){
      name_year_amount$top5[i] = "top5"
    } else {
      name_year_amount$top5[i] = "notTop5"
    }
  }
  
  name_year_amount = subset(name_year_amount, top5 == "top5")
  
  ppl_year_plt <- ggplot(data = name_year_amount) +
    geom_line(mapping = aes(x = fiscal_year, y = amount, colour = name)) +
    labs(x = "Año Fiscal",
         y = "Cantidad en Millones de $",
         title = "Gasto por Persona Anual", 
         colour = "Persona")
  
  output$person_year_plot <- renderPlotly({
    ggplotly(ppl_year_plt)
  })
  
  # Person Month
  
  name_month_amount = aggregate(amount~fiscal_year_period+name+fiscal_year, data, sum)
  name_month_amount$amount = name_month_amount$amount/1e6
  name_month_amount$dates <- paste0(name_month_amount$fiscal_year, "/", name_month_amount$fiscal_year_period, "/1")
  
  name_month_amount$dates <- as.Date(name_month_amount$dates)
  
  name_month_amount$top5 = numeric(length(name_month_amount$name))
  
  for (i in 1:length(name_month_amount$name)){
    if (name_month_amount$name[i] %in% top_nm){
      name_month_amount$top5[i] = "top5"
    } else {
      name_month_amount$top5[i] = "notTop5"
    }
  }
  
  name_month_amount = subset(name_month_amount, top5 == "top5")
  
  ppl_month_plt <- ggplot(data = name_month_amount) +
    geom_line(mapping = aes(x = dates, y = amount, colour = name)) +
    labs(x = "Mes del Año Fiscal", 
         y = "Cantidad en Millones de $",
         title = "Gasto por Persona Mensual",
         colour = "Persona")
  
  output$person_month_plot <- renderPlotly({
    ggplotly(ppl_month_plt)
  })
  
  # Person QTR
  
  name_month_amount$qtr <- numeric(length(name_month_amount$fiscal_year_period))
  for (i in 1:length(name_month_amount$fiscal_year_period)){
    if (name_month_amount$fiscal_year_period[i] >= 1 && name_month_amount$fiscal_year_period[i] <= 3){
      name_month_amount$qtr[i] = paste0(name_month_amount$fiscal_year[i], "-1")
    } else if (name_month_amount$fiscal_year_period[i] >= 4 && name_month_amount$fiscal_year_period[i] <= 6){
      name_month_amount$qtr[i] = paste0(name_month_amount$fiscal_year[i], "-2")
    } else if (name_month_amount$fiscal_year_period[i] >= 7 && name_month_amount$fiscal_year_period[i] <= 9){
      name_month_amount$qtr[i] = paste0(name_month_amount$fiscal_year[i], "-3")
    } else if (name_month_amount$fiscal_year_period[i] >= 10 && name_month_amount$fiscal_year_period[i] <= 12){
      name_month_amount$qtr[i] = paste0(name_month_amount$fiscal_year[i], "-4")
    }
  }
  
  name_qtr_amount = aggregate(amount~qtr+name, name_month_amount, sum)
  
  
  ppl_qtr_plt <- ggplot(data = name_qtr_amount) +
    geom_line(mapping = aes(x = qtr, y = amount, colour = name, group = 1)) +
    labs(x = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), " - ", max(data$fiscal_year), ")"), 
         y = "Cantidad en Millones de $",
         title = "Gasto por Persona por Trimestre",
         colour = "Persona") + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$person_qtr_plot <- renderPlotly({
    ggplotly(ppl_qtr_plt)
  })
  
  # Person Person
  nm_nm_year = name_year[1:10, ]
  nm_nm_year$amount = nm_nm_year$amount/1e6
  
  ppl_ppl_plt <- ggplot(data = nm_nm_year) + 
    geom_bar(mapping = aes(x = name, y = amount, fill = name), stat = "identity") + 
    labs(x = "Persona",
         y = "Cantidad en Millones de $",
         title = "Gasto por Persona Total",
         fill = "Persona") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$person_person_type <- renderPlotly({
    ggplotly(ppl_ppl_plt)
  })
}

shinyApp(ui, server)

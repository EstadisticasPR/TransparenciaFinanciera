
## Libraries ####
library(shiny)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)


server <- function(input, output, session) {
  
  shinyalert("Transparencia Financiera",
             "Transparencia Financiera es parte de nuestro compromiso con mejorar la transparencia del Gobierno de Puerto Rico. Al momento, provee información detallada de las transacciones del Instituto de Cultura Puertorriqueña y del Instituto de Estadísticas de Puerto Rico en distintos años. Esto con la intención de servir de ejemplo y motivar a todas las entidades públicas a hacer lo propio.",
             type = "info", confirmButtonText = "Excelente!", closeOnClickOutside = TRUE, confirmButtonCol = "#9BC337")

  data <- read.csv("transparencia.csv", na.strings = c("", "NA"))
  data$date <- as.Date.character(data$date)
  
  ### Sidebar ####
  
  output$year_slider <- renderUI({
    sliderInput("year", "Año (Year)", min = min(data$fiscal_year), max = max(data$fiscal_year), value = max(data$fiscal_year), sep = "", step = 1)
  })
  
  ######################## HOME TAB ##################################
  
  ## Progress Bar
  output$progress_bar <- renderValueBox({
    pctg = length(unique(data$department[data$fiscal_year == input$year], na.rm = TRUE))
    valueBox(paste0(round(pctg/131, 3), " % ", "(",(pctg), ") "), 
             paste0("Porcentaje de Participación Agencial para el ", input$year), 
             icon = icon("university"), 
             color = "teal")
  })
  
  ## InfoBox total gasto anual

  output$total_spending <- renderInfoBox({
    total_spent = sum(data$amount[data$fiscal_year == input$year], na.rm = TRUE)
    s_total_spent = paste0("$", round(total_spent/1e6, 3), " Millones")
    infoBox(paste("Gastos Totales en el ", input$year), s_total_spent, 
            icon = icon("money"), color = "purple")
  })
  
  ##InfoBox total de transacciones
  
  output$num_transactions <- renderInfoBox({
    trans_num = length(data$amount[data$fiscal_year == input$year])
    trans_num = paste0(trans_num, " Transacciones")
    infoBox(paste0("Numero de Transacciones en el ", input$year), trans_num, 
            icon = icon("users"), color = "purple")
  })
  
  ## Full Graph Comparison
  observeEvent(input$full_graph_bttn, {toggle("full_graph_plot")})
  
  output$full_graph_plot <- renderPlot({
    
  acc_year = aggregate(amount~account, data, sum)
  acc_year = acc_year[order(-acc_year$amount),]
  top_acc = acc_year[1:5, 1]
  
  data %>%
    filter(account %in% top_acc) %>%
    ggplot()+
    geom_bar(aes(x = fiscal_year, y = amount, fill = account), stat = "identity") +
      labs(x = "Año Fiscal",
           y = "Cantidad Total Anual",
           title = "Gasto Anual Total de Agencias Participantes",
           fill = "Tipo de Cuenta") +
      theme_minimal() +
      scale_fill_brewer(palette = "Purples")
    
  })
  
  
 ################### Agency TAB ######################### 
  
  # Top Agency Spending InfoBox
  
  output$top_agency <- renderValueBox({
    agency_data <- subset(data, fiscal_year == input$year, select = c("department","amount"))
    
    agency_data <- aggregate(amount~department, agency_data, sum)
    agency_data$amount = agency_data$amount/1e6
    agency_data <- dplyr::arrange(agency_data, desc(amount))
    
    
    top_ag <- as.character(agency_data$department[1])
    amnt_ag <- round(sum(agency_data$amount[agency_data$department == top_ag], na.rm = TRUE),3)
    
    valueBox(paste0("$", amnt_ag, " Millones"), paste(top_ag, " ", input$year), icon = icon("dollar"), color = "teal")
  })
  
  # Top Agency Spending Data Table
  
  
  output$agency_table <- DT::renderDataTable({
    agency_data <- subset(data, fiscal_year == input$year, select = c("department","amount"))
    
    agency_data <- aggregate(amount~department, agency_data, sum)
    agency_data$amount = agency_data$amount/1e6
    agency_data <- dplyr::arrange(agency_data, desc(amount))
    
    datatable(agency_data, filter = "top", colnames = c("Agencia", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  
   # Agency per Year
  
  
  output$agency_year_plot <- renderPlotly({
    year_amount = aggregate(amount~fiscal_year+department, data, sum)
    year_amount$amount = year_amount$amount/1e6
    
    agency_yearly <- ggplot(data = year_amount) +
      geom_line(mapping = aes(x = fiscal_year, y = amount, colour = department)) +
      labs(x = "Año Fiscal",
           y = "Cantidad en Millones de $",
           title = "Gasto Anual por Agencia", 
           colour = "Agencia")
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
         title = "Gasto Mensual por Agencia",
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
  
  agency_qtr_gr <- ggplot(data = qtr_amount) +
    geom_line(mapping = aes(x = qtr, y = amount, colour = department, group = 1)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank()) + 
    labs(x = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), " - ", max(data$fiscal_year), ")"), 
         y = "Cantidad en Millones de $",
         title = "Gasto Trimestral por Agencia",
         colour = "Agencia")
  
  output$agency_qtr_plot <- renderPlotly({
    ggplotly(agency_qtr_gr)
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
         title = "Gasto Total por Agencia",
         fill = "Agencia")
  
  output$agency_agency_plot <- renderPlotly({
    ggplotly(agency_agency)
  })
  
  ################### Account TAB ######################### 
  
  # Type of Spending InfoBox
  
  output$top_type <- renderValueBox({
    account_data <- subset(data, fiscal_year == input$year, select = c("account","amount"))
    
    account_data <- aggregate(amount~account, account_data, sum)
    account_data$amount <- account_data$amount/1e6
    account_data <- dplyr::arrange(account_data, desc(amount))
    
    top_tp <- as.character(account_data$account[1])
    amnt_tp <- round(sum(account_data$amount[account_data$account == top_tp], na.rm = TRUE),3)
    
    valueBox(paste0("$", amnt_tp, " Millones"), paste(top_tp, " ", input$year), icon = icon("dollar"), color = "teal")
  })
  
  
  # Type of Spending Data Table
  output$account_table <- DT::renderDataTable({
    account_data <- subset(data, fiscal_year == input$year, select = c("account","amount"))
    
    account_data <- aggregate(amount~account, account_data, sum)
    account_data$amount <- account_data$amount/1e6
    account_data <- dplyr::arrange(account_data, desc(amount))
    
    datatable(account_data, filter = "top", colnames = c("Tipo de Cuenta", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
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
         title = "Gasto Anual por Tipo de Cuenta", 
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
         title = "Gasto Mensual por Tipo de Cuenta",
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
         title = "Gasto Trimestral por Tipo de Cuenta",
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
         title = "Gasto Total por Tipo de Cuenta",
         fill = "Tipo de Cuenta") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$account_account_plot <- renderPlotly({
    
    ggplotly(acc_acc_plot)
  })
  
  ################### Person TAB ######################### 
  
  
  # Person InfoBox
  
  output$top_person <- renderValueBox({
    person_data <- subset(data, fiscal_year == input$year, select = c("name", "amount"))
    
    person_data <- aggregate(amount~name, person_data, sum)
    person_data$amount <- person_data$amount/1e6
    person_data <- dplyr::arrange(person_data, desc(amount))
    
    top_pers <- as.character(person_data$name[1])
    amnt_pers <- round(sum(person_data$amount[person_data$name == top_pers], na.rm = TRUE), 3)
    
    valueBox(paste0("$", amnt_pers, " Millones"), paste(top_pers, " ", input$year), icon = icon("dollar"), color = "teal")
  })
  
  # Person Data Table
  output$person_table <- DT::renderDataTable({
    person_data <- subset(data, fiscal_year == input$year, select = c("name", "amount"))
    
    person_data <- aggregate(amount~name, person_data, sum)
    person_data$amount <- person_data$amount/1e6
    person_data <- dplyr::arrange(person_data, desc(amount))
    
    datatable(person_data, filter = 'top', colnames = c("Nombre", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
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
         title = "Gasto Anual por Persona", 
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
         title = "Gasto Mensual por Persona",
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
         title = "Gasto Trimestral por Persona",
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
         title = "Gasto Total por Persona",
         fill = "Persona") +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank())
  
  output$person_person_type <- renderPlotly({
    ggplotly(ppl_ppl_plt)
  })
  
  ###### Data Download Tab ########
  output$table_download <- renderDataTable({
    datatable(data, filter = 'top', colnames = c("Tipo de Cuenta", "Cantidad (USD)", "Fecha", "Agencia", "Año Fiscal", "Mes", "Nombre"), 
              options = list(order = list(3, 'desc')))
  })
  
  output$dwn_bttn <- downloadHandler('transparencia_financiera.csv', content = function(file){
    s = input$table_download_rows_all
    df = data.frame(data[s, ])
    write.csv(df, file)
  })
  
}
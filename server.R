####  Libraries ####
library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(rbokeh)
library(lubridate)
library(shinythemes)


shinyserver <- function(input, output, session) {
  
data <- fread('data/transparencia.csv', na.strings = c('', 'NA'))

####  HOME TAB DATA TABLE  ####
  
  output$agency_table <- DT::renderDataTable({
    agency_data <- data %>%
      select("department", "amount") %>%
      group_by(department) %>%
      summarize(amount = sum(amount/1e6, na.rm = T)) %>%
      arrange(desc(amount)) %>%
      mutate(amount = paste(round(amount, 3), '$'))
    
    datatable(agency_data, filter = "top", colnames = c("Agencia", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  
####  HOME TAB PLOTS   ####

  # Agency per Year
  year_amount <- data %>%
    group_by(fiscal_year, department) %>%
    summarize(amount = sum(amount/1e6)) %>%
    arrange(fiscal_year)

  output$agency_year_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Anual por Agencia") %>%
      ly_lines(fiscal_year, amount, 
               data = year_amount, 
               group = department, 
               color = department) %>%
      ly_points(fiscal_year, amount,
                data = year_amount,
                group = department, 
                color = department, 
                hover = c(department, fiscal_year, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares") %>%
      theme_axis(which = c("x"), minor_tick_line_color = "white")
  })
  
  # Agency per Month
  month_amount <- data %>%
    group_by(fiscal_year_period, department, fiscal_year) %>%
    summarize(amount = sum(amount/1e6)) %>%
    mutate(dates = ymd(paste0(fiscal_year, '/', fiscal_year_period, '/1'))) %>%
    arrange(dates)
  
  output$agency_month_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Mensual por Agencia") %>%
      ly_lines(x = dates, 
               y = amount, 
               data = month_amount, 
               group = department, 
               color = department) %>%
      ly_points(x = dates, 
                y = amount, 
                data = month_amount, 
                group = department, 
                color = department, 
                hover = c(department, dates, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares")
  })
  
  # Agency per QTR
  qtr_amount <- month_amount %>%
    mutate(qtr = paste0(fiscal_year, '-' ,lubridate::quarter(fiscal_year_period))) %>%
    group_by(qtr, department) %>%
    mutate(amount = sum(amount))
  
  output$agency_qtr_plot <- renderRbokeh({
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T, title = "Gasto Trimestral por Agencia") %>%
      ly_lines(x = qtr,
               y = amount, 
               data = qtr_amount,
               group = department, 
               color = department) %>%
      ly_points(x = qtr,
                y = amount, 
                data = qtr_amount, 
                group = department, 
                color = department, 
                hover = c(qtr, department, amount)) %>%
      x_axis(label = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), ' - ', max(data$fiscal_year), ')')) %>%
      y_axis(label = "Millones de Dolares") %>%
      theme_axis(which = c("x"), major_label_text_color = "white", major_tick_line_color = "white")
  })


####  ACCOUNT TAB DATA TABLE  ####
  
  output$account_table <- DT::renderDataTable({
    account_data <- data %>%
      select("account", "amount") %>%
      group_by(account) %>%
      summarize(amount = sum(amount/1e6)) %>%
      arrange(desc(amount)) %>%
      mutate(amount = paste(round(amount, 3), '$'))
    
    datatable(account_data, filter = "top", colnames = c("Tipo de Gasto", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })


####  ACCOUNT TAB PLOTS ####
  # Account Year
  acc_year <- data %>% 
    group_by(account) %>%
    summarize(amount = sum(amount)) %>%
    arrange(desc(amount)) %>%
    top_n(5) %>%
    select('account')
  
  acc_year_amount <- data %>%
    filter(account %in% c(acc_year$account)) %>%
    group_by(fiscal_year, account) %>%
    summarize(amount = sum(amount/1e6)) %>%
    arrange(desc(amount))
  
  output$account_year_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Anual por Tipo de Gasto") %>%
      ly_lines(fiscal_year, amount, 
               data = acc_year_amount, 
               group = account, 
               color = account) %>%
      ly_points(fiscal_year, amount,
                data = acc_year_amount,
                group = account, 
                color = account, 
                hover = c(account, fiscal_year, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares") %>%
    theme_axis(which = c("x"), minor_tick_line_color = "white")

  })
  
  # Account Month
  
  acc_month_amount <- data %>%
    filter(account %in% c(acc_year$account)) %>%
    group_by(fiscal_year_period, account, fiscal_year) %>%
    summarize(amount = sum(amount/1e6)) %>%
    mutate(dates = ymd(paste0(fiscal_year, '/', fiscal_year_period, '/1'))) %>%
    arrange(dates)
  
  output$account_month_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Mensual por Tipo de Gasto") %>%
      ly_lines(x = dates, 
               y = amount, 
               data = acc_month_amount, 
               group = account, 
               color = account) %>%
      ly_points(x = dates, 
                y = amount, 
                data = acc_month_amount, 
                group = account, 
                color = account, 
                hover = c(account, dates, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares")
  })
  
  # Account QTR
  
  acc_qtr_amount <- acc_month_amount %>%
    mutate(qtr = paste0(fiscal_year, '-' ,lubridate::quarter(fiscal_year_period))) %>%
    group_by(qtr, account) %>%
    summarize(amount = sum(amount))

  output$account_qtr_plot <- renderRbokeh({
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T, title = "Gasto Trimestral por Agencia") %>%
      ly_lines(x = qtr,
               y = amount,
               data = acc_qtr_amount,
               group = account,
               color = account) %>%
      ly_points(x = qtr,
                y = amount,
                data = acc_qtr_amount,
                group = account,
                color = account,
                hover = c(qtr, account, amount)) %>%
      x_axis(label = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), ' - ', max(data$fiscal_year), ')')) %>%
      y_axis(label = "Millones de Dolares") %>%
      theme_axis(which = c("x"), major_label_text_color = "white", major_tick_line_color = "white")
   })
  
  
#### PERSON TAB DATA TABLE ####
  # Person Data Table
  output$person_table <- DT::renderDataTable({
    person_data <- data %>%
      select("name", "amount") %>%
      group_by(name) %>%
      summarize(amount = (sum(amount)/1e6)) %>%
      arrange(desc(amount)) %>%
      mutate(amount = paste(round(amount, 3), '$'))
    
    datatable(person_data, filter = 'top', colnames = c("Nombre", "Cantidad (Millones)"), 
              options = list(order = list(2, 'desc')))
  })
  
  
#### PERSON TAB PLOTS ####
  # Person Anual
  name_year = data %>%
    group_by(name) %>%
    summarize(amount = sum(amount)) %>%
    arrange(desc(amount)) %>%
    top_n(5) %>%
    select('name')
    
  name_year_amount <- data %>%
    filter(name %in% c(name_year$name)) %>%
    group_by(fiscal_year, name) %>%
    summarize(amount = sum(amount/1e6)) %>%
    arrange(desc(amount))
  
  output$person_year_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Anual por Persona") %>%
      ly_lines(fiscal_year, amount, 
               data = name_year_amount, 
               group = name, 
               color = name) %>%
      ly_points(fiscal_year, amount,
                data = name_year_amount,
                group = name, 
                color = name, 
                hover = c(name, fiscal_year, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares") %>%
      theme_axis(which = c("x"), minor_tick_line_color = "white")
    })
  
  # Person Month
  
  name_month_amount <- data %>%
    filter(name %in% c(name_year$name)) %>%
    group_by(fiscal_year_period, name, fiscal_year) %>%
    summarize(amount = sum(amount/1e6)) %>%
    mutate(dates = ymd(paste0(fiscal_year, '/', fiscal_year_period, '/1'))) %>%
    arrange(dates)
  
  output$person_month_plot <- renderRbokeh({
    figure(legend_location = "top_left", width = 3300, height = 1100, h_symmetry = T, title = "Gasto Mensual por Persona") %>%
      ly_lines(x = dates, 
               y = amount, 
               data = name_month_amount, 
               group = name, 
               color = name) %>%
      ly_points(x = dates, 
                y = amount, 
                data = name_month_amount, 
                group = name, 
                color = name, 
                hover = c(name, dates, amount)) %>%
      x_axis(label = "Año Fiscal") %>%
      y_axis(label = "Millones de Dolares")
  })
  
  # Person QTR
  
  name_qtr_amount <- name_month_amount %>%
    mutate(qtr = paste0(fiscal_year, '-' ,lubridate::quarter(fiscal_year_period))) %>%
    group_by(qtr, name) %>%
    summarize(amount = sum(amount))
  
  output$person_qtr_plot <- renderRbokeh({
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T, title = "Gasto Trimestral por Persona") %>%
      ly_lines(x = qtr,
               y = amount,
               data = name_qtr_amount,
               group = name,
               color = name) %>%
      ly_points(x = qtr,
                y = amount,
                data = name_qtr_amount,
                group = name,
                color = name,
                hover = c(qtr, name, amount)) %>%
      x_axis(label = paste0("Trimestre del Año Fiscal (", min(data$fiscal_year), ' - ', max(data$fiscal_year), ')')) %>%
      y_axis(label = "Millones de Dolares")  %>%
      theme_axis(which = c("x"), major_label_text_color = "white", major_tick_line_color = "white")
  })
  

#### DATA EXPLORER TAB ####
x <- reactive({
  switch(input$indvar,
         "Mes" = 'fiscal_year_period', 
         "Año" = 'fiscal_year',
         "Tipo de Gasto" = "account", 
         "Persona" = "name", 
         "Agencia" = "department")
})

z <- reactive({
  switch(input$depvar, 
         "Tipo de Gasto" = "account", 
         "Persona" = "name", 
         "Agencia" = "department")
})

output$data_explorer_plot <- renderRbokeh({
  
  if (x() == "fiscal_year_period"){
    third_axis = as.name(z())
    
    plot_data = data %>%
      filter(name %in% c(name_year$name)) %>%
      group_by_('fiscal_year_period', z(), 'fiscal_year') %>%
      summarize(amount = sum(amount/1e6)) %>%
      mutate(dates = ymd(paste0(fiscal_year, '/', fiscal_year_period, '/1'))) %>%
      arrange(dates)
    
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T) %>%
      ly_lines(dates, 
               amount, 
               plot_data, 
               group = z(),
               color = z()) %>%
      ly_points(dates,
                amount,
                plot_data,
                group = z(),
                color = z(), 
                hover = c(dates, z(), amount))
    
  } else if (x() == "fiscal_year"){
    third_axis = as.name(z())
    
    plot_data = data %>%
      filter(name %in% c(name_year$name)) %>%
      group_by_(z(), 'fiscal_year') %>%
      summarize(amount = sum(amount/1e6))
    
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T) %>%
      ly_lines(fiscal_year, 
               amount, 
               plot_data, 
               group = z(),
               color = z()) %>%
      ly_points(fiscal_year,
                amount,
                plot_data,
                group = z(),
                color = z(),
                hover = c(fiscal_year, z(), amount))
  } else if (x() == "account" || x() == "name" || x() == "department"){
    third_axis = as.name(z())
    
    plot_data = data %>%
      filter(name %in% c(name_year$name)) %>%
      group_by_(z(), x()) %>%
      summarize(amount = sum(amount/1e6))
    
    figure(legend_location = 'top_left', width = 3300, height = 1100, h_symmetry = T) %>%
      ly_bar(x(), 
             amount, 
             plot_data, 
             hover = T, 
             color = z(), 
             position = "dodge")
  }
  
})
  

#### DATA DOWNLOAD TAB ####
  output$table_download <- renderDataTable({
    df = data.frame(data)
    datatable(df, filter = 'top', colnames = c("Mes", "Tipo de Gasto", "Nombre", "Año Fiscal", "Fecha", "Cantidad (USD)",  "Agencia"),
              options = list(order = list(4, 'desc')))
  })
  
  output$dwn_bttn <- downloadHandler('transparencia_financiera.csv', content = function(file){
    s = input$table_download_rows_all
    df = data.frame(data[s, ])
    write.csv(df, file)
  })
  



}

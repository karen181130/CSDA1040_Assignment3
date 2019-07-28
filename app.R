
library(shiny)
#setwd("C:/Users/Karen/Desktop/1040R/Assign3/Project3/Project3")

#load RDS objects
#1st plot
furn_s_ts <- readRDS("furn_s_ts.Rds")
ofc_s_ts <- readRDS("ofc_s_ts.Rds")
tech_s_ts <- readRDS("tech_s_ts.Rds")
furn_s_fore <- readRDS("furn_s_fore.Rds")
ofc_s_fore <- readRDS("ofc_s_fore.Rds")
tech_s_fore <- readRDS("tech_s_fore.Rds")

#2nd plot
f_var_cpi_24 <- readRDS("f_var_cpi_24.Rds")
f_var_inf_24 <- readRDS("f_var_inf_24.Rds")
f_var_une_24 <- readRDS("f_var_une_24.Rds")
f_var_fx_24 <- readRDS("f_var_fx_24.Rds")
o_var_cpi_24 <- readRDS("o_var_cpi_24.Rds")
o_var_inf_24 <- readRDS("o_var_inf_24.Rds")
o_var_une_24 <- readRDS("o_var_une_24.Rds")
o_var_fx_24 <- readRDS("o_var_fx_24.Rds")
t_var_cpi_24 <- readRDS("t_var_cpi_24.Rds")
t_var_inf_24 <- readRDS("t_var_inf_24.Rds")
t_var_une_24 <- readRDS("t_var_une_24.Rds")
t_var_fx_24 <- readRDS("t_var_fx_24.Rds")

#3rd plot
f_var2_24 <- readRDS("f_var2_24.Rds")
o_var2_24 <- readRDS("o_var2_24.Rds")
t_var2_24 <- readRDS("t_var2_24.Rds")

ui <- fluidPage(
  
  #App Title
  titlePanel("Superstore Sales Forecast"),
  
  #Sidebar Layout (for inputs and outputs)
  sidebarLayout(
    
    #Sidebar layout for inputs
    sidebarPanel(
      
      #Select product category
      selectInput("prod_cat", "Select a Superstore Product Category", 
                  c("Furniture", "Office Supplies", "Technology")),
      
      #Select macroeconomic factor
      selectInput("factor", "Select a Macroeconomic Factor to see the effect on Sales of selected Product Category",
                  c("Inflation Rate", "Consumer Price Index", "Unemployment Rate", "CADUSD FX Rate")),
      
      #Additional Info
      helpText("Note: Click the Forecast button to get a visualization of the predicted values of the time series analysis."),
      
      #Update action button
      actionButton("update", "Forecast")
      
    ),
    
    #Main Panel for outputs
    mainPanel(
      
      #3 Tabs for Univariate and Multivariate Plots
      tabsetPanel(
        
        tabPanel("Univariate Analysis - Sales",
                 h5("This graph shows the plot of the monthly sales figures from 2014-2017 and the forecasted values for 2018-2019."),
                 plotOutput("uniplot")),
        
        tabPanel("Bivariate Analysis - Sales and Selected Macroeconomic Factor",
                 h5("This graph shows the plot of monthly sales figure and predicted trend of the values after 2017, using sales and the selected macroeconomic factor as variables."),
                 plotOutput("multi1")),
        
        tabPanel("Multivariate Analysis - Sales, Inflation, CPI, Unemployment, CADUSD FX",
                 h5("This graph shows the fancharts of all the variables include in the Multivariate Time Series Analysis."),
                 plotOutput("multi2"))
        
      )
    )
  )
)

server <- function(input, output) {
  
  #Output for tab1: Univariate Forecast
  
  library(stringi)
  
  prodtxt <- eventReactive(input$update, {
    input$prod_cat
  })
  
  plot1title <- eventReactive(input$update, {
    stri_c(prodtxt(), " Sales Forecast")
  })

  t1 <- eventReactive(input$update, {
    switch(prodtxt(),
           "Furniture" = furn_s_ts,
           "Office Supplies" = ofc_s_ts,
           "Technology" = tech_s_ts)
  }, ignoreNULL = FALSE)

  t2 <- eventReactive(input$update, {
    switch(prodtxt(),
           "Furniture" = furn_s_fore$pred,
           "Office Supplies" = ofc_s_fore$pred,
           "Technology" = tech_s_fore$pred)
  }, ignoreNULL = FALSE)
  
  t3 <- eventReactive(input$update, {
    switch(prodtxt(),
           "Furniture" = furn_s_fore$se,
           "Office Supplies" = ofc_s_fore$se,
           "Technology" = tech_s_fore$se)
  }, ignoreNULL = FALSE)
  
  plt1 <- eventReactive(input$update, {
    ts.plot(t1(), t2(), t2()+t3(), t2()-t3(), col=c(1,2,4,4), lty = c(1,1,2,2), main=plot1title())
    legend("topleft",c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2),cex=1)
  })

  output$uniplot <- renderPlot({plt1()})
  
  #Output for tab2: Multivariate (sales+1 macro factor)
  
  t5 <- eventReactive(input$update, {
    switch(prodtxt(),
           "Furniture" = "f_var_",
           "Office Supplies" = "o_var_",
           "Technology" = "t_var_")
  }, ignoreNULL = FALSE)
  
  mfact <- eventReactive(input$update, {
    input$factor
  })
  
  t6 <- eventReactive(input$update, {
    switch(mfact(),
           "Inflation Rate" = "inf_24",
           "Consumer Price Index" = "cpi_24",
           "Unemployment Rate" = "une_24",
           "CADUSD FX Rate" = "fx_24")
  }, ignoreNULL = FALSE)
  
  t7 <- eventReactive(input$update, {
    stri_c(t5(),t6())
  })
  
  t8 <- eventReactive(input$update, {
    switch(t7(),
           "f_var_inf_24" = f_var_inf_24,
           "f_var_cpi_24" = f_var_cpi_24,
           "f_var_une_24" = f_var_une_24,
           "f_var_fx_24" = f_var_fx_24,
           "o_var_inf_24" = o_var_inf_24,
           "o_var_cpi_24" = o_var_cpi_24,
           "o_var_une_24" = o_var_une_24,
           "o_var_fx_24" = o_var_fx_24,
           "t_var_inf_24" = t_var_inf_24,
           "t_var_cpi_24" = t_var_cpi_24,
           "t_var_une_24" = t_var_une_24,
           "t_var_fx_24" = t_var_fx_24)
  })
  
  plt3 <- eventReactive(input$update,{
    plot(t8())
  })
  
  output$multi1 <- renderPlot({plt3()})
  
  #Output for tab2: Multivariate (all factors)
  library(vars)

  t4 <- eventReactive(input$update, {
    switch(prodtxt(),
           "Furniture" = f_var2_24,
           "Office Supplies" = o_var2_24,
           "Technology" = t_var2_24)
  }, ignoreNULL = FALSE)
  
  plt2 <- eventReactive(input$update, {
    fanchart(t4())
  })
  
  output$multi2 <- renderPlot({plt2()})
  
}

shinyApp(ui=ui, server=server)








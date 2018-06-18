library(shinydashboard)
require(plyr)
library(dplyr)
library(shiny)
require(RPostgreSQL)
require(reshape2)
require(tables)
library(xlsx)
library(rsconnect)
rsconnect::setAccountInfo(name='grad2mis',
                          token='3CBB8545E987FE1A93C5C2B8A106ABAD',
                          secret='kMVdXjnsAcN+QZPv9AIIljc6EVM0PsdyXBD3VNWj')
options(knitr.table.format="html")
options(scipen=999) #avoids printing exponential notations such 1e+10

options(warn = -1) #suppresses warnings

source("sitesettings.R")

year <- c(format(Sys.Date(), "%Y"))

dashTitle <- paste0("GRAD2MIS ",year)
ui <- dashboardPage(
  dashboardHeader(title = dashTitle),
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Vesa Saving", tabName = "vsdashboard", icon = icon("dashboard")
    ),
    menuItem("Vesa Loan", tabName = "vldashboard", icon = icon("dashboard"))
  )),
  ## Body content
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "vsdashboard",
            fluidPage(
              titlePanel("VESA Household Saving - Yearly "),
              
              # Select Year 
              selectInput(
                inputId =  "date_from", 
                label = "Select year", 
                choices = 2010:2100,
                selected = 2018
              ),
              verbatimTextOutput("savingyear_filtered_row"),
              downloadButton(outputId = "download_savingyear_filtered",
                             label = "Download Data"),
            fluidRow(DT::dataTableOutput('saving_dty')),hr(),
            titlePanel("VESA Household Saving - Time frame"),
            
            # Select Quarter or Month 
            dateRangeInput("monthslide", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                        end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
            verbatimTextOutput("savingqtr_filtered_row"),
            downloadButton(outputId = "download_savingqtr_filtered",
                           label = "Download Data"),
            fluidRow(DT::dataTableOutput('saving_dtq'))
            )),
    
    
    # Second tab content
    tabItem(tabName = "vldashboard",
            fluidPage(
              titlePanel("VESA Household Loan Yearly"),
              # Select Year 
              selectInput(
                inputId =  "date_from2", 
                label = "Select year", 
                choices = 2010:2100,
                selected = 2018
              ),
              verbatimTextOutput("loanyear_filtered_row"),
              downloadButton(outputId = "download_loanyear_filtered",
                             label = "Download Data"),
              fluidRow(DT::dataTableOutput('loan_dty')),hr(),
              titlePanel("VESA Household Loan - Time frame"),
              
              # Select Quarter or Month 
              dateRangeInput("monthslide2", label = h5("Select Period, i.e monthly, quarterly, etc"), start = NULL, 
                             end = NULL , format = "yyyy-mm-dd", startview = "month", separator = "to", weekstart = 0),
              verbatimTextOutput("loanqtr_filtered_row"),
              downloadButton(outputId = "download_loanqtr_filtered",
                             label = "Download Data"),
              fluidRow(DT::dataTableOutput('loan_dtq'))
              
               ))
  ))
  )


server <- function(input, output, session) {
  
  year3 <- "2018"
  
  drv<-dbDriver("PostgreSQL")
  con<-dbConnect(drv, user= dbUserName, password=dbPassword,host=dbHost, port=dbPort,dbname=dbName)
  
#Yearly DATA
  savingDataYear <- reactive({
  
  #Gets a list of of VESA, PSNP and Saving amount
  sql.saving<-paste0("select tedv.lastupdated::DATE as report_date, reg.name as Region, 
zon.name as Zone, wor.name as Woreda, keb.name as Kebele, 
ves.name as VESA, teav.value as PSNP_number, tedv.value as Saving from _orgunitstructure ous
INNER JOIN organisationunit ves ON ous.idlevel6 = ves.organisationunitid
INNER JOIN organisationunit keb ON ous.idlevel5 = keb.organisationunitid
INNER JOIN organisationunit wor ON ous.idlevel4 = wor.organisationunitid
INNER JOIN organisationunit zon ON ous.idlevel3 = zon.organisationunitid
INNER JOIN organisationunit reg ON ous.idlevel2 = reg.organisationunitid
INNER JOIN trackedentityinstance tei ON ves.organisationunitid = tei.organisationunitid 
INNER JOIN trackedentityattributevalue teav ON tei.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programinstance pi ON pi.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programstageinstance psi ON psi.programinstanceid = pi.programinstanceid 
INNER JOIN  trackedentitydatavalue tedv ON tedv.programstageinstanceid = psi.programstageinstanceid
WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2677 AND tedv.value::decimal > 0 AND tedv.lastupdated::text LIKE '",input$date_from,"%';")
  
  
  savingAmount <-dbGetQuery(con,sql.saving)
  
  #Summarize and aggregate the savings data
  savingAmount <- ddply(savingAmount, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, saving_amount=sum(as.numeric(saving)))
  
  return(savingAmount)})
  
  loanDataYear <- reactive({
  
  #Gets a list of of VESA, PSNP and loan amount
  sql.loan<-paste0("select tedv.lastupdated::DATE as report_date, reg.name as Region, 
zon.name as Zone, wor.name as Woreda, keb.name as Kebele, 
ves.name as VESA, teav.value as PSNP_number, tedv.value as Loan from _orgunitstructure ous
INNER JOIN organisationunit ves ON ous.idlevel6 = ves.organisationunitid
INNER JOIN organisationunit keb ON ous.idlevel5 = keb.organisationunitid
INNER JOIN organisationunit wor ON ous.idlevel4 = wor.organisationunitid
INNER JOIN organisationunit zon ON ous.idlevel3 = zon.organisationunitid
INNER JOIN organisationunit reg ON ous.idlevel2 = reg.organisationunitid
INNER JOIN trackedentityinstance tei ON ves.organisationunitid = tei.organisationunitid 
INNER JOIN trackedentityattributevalue teav ON tei.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programinstance pi ON pi.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programstageinstance psi ON psi.programinstanceid = pi.programinstanceid 
INNER JOIN  trackedentitydatavalue tedv ON tedv.programstageinstanceid = psi.programstageinstanceid
WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2673 AND tedv.value::decimal > 0 AND tedv.lastupdated::text LIKE '",input$date_from2,"%';")
  
  loanAmount <-dbGetQuery(con,sql.loan)

  

  #Summarize and aggregate the loans data
  loanAmount <- ddply(loanAmount, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, loan_amount=sum(as.numeric(loan)))
  })
  
  
  #Quarterly Data
  savingDataQuarter <- reactive({
    
    #Gets a list of of VESA, PSNP and Saving amount
    sql.savingQuarter<-paste0("select tedv.lastupdated::DATE as report_date, reg.name as Region, 
                              zon.name as Zone, wor.name as Woreda, keb.name as Kebele, 
                              ves.name as VESA, teav.value as PSNP_number, tedv.value as Saving from _orgunitstructure ous
                              INNER JOIN organisationunit ves ON ous.idlevel6 = ves.organisationunitid
                              INNER JOIN organisationunit keb ON ous.idlevel5 = keb.organisationunitid
                              INNER JOIN organisationunit wor ON ous.idlevel4 = wor.organisationunitid
                              INNER JOIN organisationunit zon ON ous.idlevel3 = zon.organisationunitid
                              INNER JOIN organisationunit reg ON ous.idlevel2 = reg.organisationunitid
                              INNER JOIN trackedentityinstance tei ON ves.organisationunitid = tei.organisationunitid 
                              INNER JOIN trackedentityattributevalue teav ON tei.trackedentityinstanceid = teav.trackedentityinstanceid 
                              INNER JOIN programinstance pi ON pi.trackedentityinstanceid = teav.trackedentityinstanceid 
                              INNER JOIN programstageinstance psi ON psi.programinstanceid = pi.programinstanceid 
                              INNER JOIN  trackedentitydatavalue tedv ON tedv.programstageinstanceid = psi.programstageinstanceid
                              WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2677 AND tedv.value::decimal > 0 AND tedv.lastupdated::text BETWEEN '",input$monthslide[1],"%' AND '",input$monthslide[2],"%';")
    
    
    savingAmountQuarter <-dbGetQuery(con,sql.savingQuarter)
    
    #Summarize and aggregate the savings data
    savingAmountQuarter <- ddply(savingAmountQuarter, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, saving_amount=sum(as.numeric(saving)))
    
    return(savingAmountQuarter)})
  
  loanDataQuarter <- reactive({
    
    #Gets a list of of VESA, PSNP and loan amount
    sql.loanQuarter<-paste0("select tedv.lastupdated::DATE as report_date, reg.name as Region, 
zon.name as Zone, wor.name as Woreda, keb.name as Kebele, 
ves.name as VESA, teav.value as PSNP_number, tedv.value as Loan from _orgunitstructure ous
INNER JOIN organisationunit ves ON ous.idlevel6 = ves.organisationunitid
INNER JOIN organisationunit keb ON ous.idlevel5 = keb.organisationunitid
INNER JOIN organisationunit wor ON ous.idlevel4 = wor.organisationunitid
INNER JOIN organisationunit zon ON ous.idlevel3 = zon.organisationunitid
INNER JOIN organisationunit reg ON ous.idlevel2 = reg.organisationunitid
INNER JOIN trackedentityinstance tei ON ves.organisationunitid = tei.organisationunitid 
INNER JOIN trackedentityattributevalue teav ON tei.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programinstance pi ON pi.trackedentityinstanceid = teav.trackedentityinstanceid 
INNER JOIN programstageinstance psi ON psi.programinstanceid = pi.programinstanceid 
INNER JOIN  trackedentitydatavalue tedv ON tedv.programstageinstanceid = psi.programstageinstanceid
WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2673 AND tedv.value::decimal > 0 AND tedv.lastupdated::text BETWEEN '",input$monthslide2[1],"%' AND '",input$monthslide2[2],"%';")
    
    loanAmountQuarter <-dbGetQuery(con,sql.loanQuarter)
    
    
    
    #Summarize and aggregate the loans data
    loanAmountQuarter <- ddply(loanAmountQuarter, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, loan_amount=sum(as.numeric(loan)))
  })

  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  # access the value of the widget with input$date, e.g.
  output$value <- renderPrint({ input$date_from })
  
  output$saving_dty = DT::renderDataTable(
    savingDataYear(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$saving_dtq = DT::renderDataTable(
    savingDataQuarter(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$loan_dty = DT::renderDataTable(
    loanDataYear(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$loan_dtq = DT::renderDataTable(
    loanDataQuarter(),
    filter = 'top',
    options = list(scrollX = TRUE)
  )

  output$savingyear_filtered_row <- 
    renderText({
      paste0(length(input$saving_dty_rows_all)," Households saved")})
  
  output$savingqtr_filtered_row <- 
    renderText({
      paste0(length(input$saving_dtq_rows_all)," Households saved")})
  
  
  
  output$download_savingyear_filtered <- 
    downloadHandler(
      filename = "Vesa Saving This Year.xlsx",
      content = function(file){
        write.xlsx(savingDataYear()[input$saving_dty_rows_all, ], file, sheetName = "VESA Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$download_savingqtr_filtered <- 
    downloadHandler(
      filename = "Vesa Saving Quarterly.xlsx",
      content = function(file){
        write.xlsx(savingDataQuarter()[input$saving_dtq_rows_all, ], file, sheetName = "VESA Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$loanyear_filtered_row <- 
    renderText({
      paste0(length(input$loan_dty_rows_all)," Households obtained loans")})
  
  
  output$download_loanyear_filtered <- 
    downloadHandler(
      filename = "Vesa Loan This Year.xlsx",
      content = function(file){
        write.xlsx(loanDataYear()[input$loan_dty_rows_all, ], file, sheetName = "VESA Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$loanqtr_filtered_row <- 
    renderText({
      paste0(length(input$loan_dtq_rows_all)," Households obtained loans")})
  
  output$download_loanqtr_filtered <- 
    downloadHandler(
      filename = "Vesa Loan Quarterly.xlsx",
      content = function(file){
        write.xlsx(loanDataQuarter()[input$loan_dtq_rows_all, ], file, sheetName = "VESA Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
}

shinyApp(ui, server)

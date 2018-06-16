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
thisYear <- toString(year)
drv<-dbDriver("PostgreSQL")
con<-dbConnect(drv, user= dbUserName, password=dbPassword,host=dbHost, port=dbPort,dbname=dbName)

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
WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2677 AND tedv.value::decimal > 0 AND tedv.lastupdated::text LIKE '",year,"%';")

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
WHERE teav.trackedentityattributeid = 3195 and tedv.dataelementid = 2673 AND tedv.value::decimal > 0 AND tedv.lastupdated::text LIKE '",year,"%';")

loanAmount <-dbGetQuery(con,sql.loan)

savingAmount <-dbGetQuery(con,sql.saving)

#Summarize and aggregate the savings data
savingAmount <- ddply(savingAmount, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, saving_amount=sum(as.numeric(saving)))

write.xlsx(savingAmount, "vesa_savings_last_quarter.xlsx", sheetName = "VESA Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)

#Summarize and aggregate the loans data
loanAmount <- ddply(loanAmount, .(report_date, region,zone,woreda,kebele,vesa,psnp_number), summarise, loan_amount=sum(as.numeric(loan)))
write.xlsx(loanAmount, "vesa_loan_year.xlsx", sheetName = "VESA Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)

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
              titlePanel("VESA Household Saving This Year "),
              verbatimTextOutput("filtered_row"),
              downloadButton(outputId = "download_filtered",
                             label = "Download Data"),
            fluidRow(DT::dataTableOutput('items_dt')))),
    
    # Second tab content
    tabItem(tabName = "vldashboard",
            fluidPage(
              titlePanel("VESA Household Loan This Year"),
              verbatimTextOutput("filtered_row2"),
              downloadButton(outputId = "download_filtered2",
                             label = "Download Data"),
              fluidRow(DT::dataTableOutput('items_dt2'))))
  ))
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$items_dt = DT::renderDataTable(
    savingAmount,
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  
  output$items_dt2 = DT::renderDataTable(
    loanAmount,
    filter = 'top',
    options = list(scrollX = TRUE)
  )
  

  output$filtered_row <- 
    renderText({
      paste0(length(input$items_dt_rows_all)," Households saved")})
  
  output$download_filtered <- 
    downloadHandler(
      filename = "Vesa Saving This Year.xlsx",
      content = function(file){
        write.xlsx(savingAmount[input$items_dt_rows_all, ], file, sheetName = "VESA Saving", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
  
  output$filtered_row2 <- 
    renderText({
      paste0(length(input$items_dt2_rows_all)," Households obtained loans")})
  
  
  output$download_filtered2 <- 
    downloadHandler(
      filename = "Vesa Loan This Year.xlsx",
      content = function(file){
        write.xlsx(loanAmount[input$items_dt2_rows_all, ], file, sheetName = "VESA Loans", col.names = TRUE, row.names = TRUE, append = FALSE, showNA = TRUE)
        
      }
    )
}
rsconnect::deployApp('~/Grad2MIS-shinyApp')
shinyApp(ui, server)

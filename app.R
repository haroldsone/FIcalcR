## app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyverse)
library(cowplot)
library(magick)
library(grid)
library(png)
source("CalcFuncs.R")

T947b <- readPNG("www/Thiery947b2.png")

ui <- dashboardPage(
    #Header
    dashboardHeader(title = "FIcalcR"),
    
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Main", tabName = "Main"),
            menuItem("H2O-NaCl", tabName = "HN", badgeLabel = "under construction", badgeColor = "red"),
            menuItem("H2O-CO2-NaCl", tabName = "HCN", badgeLabel = "coming soon", badgeColor = "green"),
            menuItem("CO2-CH4", tabName = "CC", badgeLabel = "under construction", badgeColor = "red"))),
    
    #Main Body
    dashboardBody(
        tabItems(
    
    #Main tab content
            tabItem(tabName = "Main",
                fluidRow(
                    box(
                    img(src = "FIcalcRlogo.png", height = 120, width = 120),
                    br(),
                    strong("FIcalcR is a free and open-source online web app for calculating fluid inclusion data."),
                    br(),
                    p("This app is programmed in RStudio and presented online using the Shiny package"),
                    br(),
                    tags$div(class="header", checked=NA,
                        tags$p("Users are welcome to contribute to the development of FIcalcR. The source code can be forked from "),
                        tags$a(href="https://github.com/haroldsone/FIcalcR", "GitHub"),
                        tags$br(),
                        tags$p("Please, submit a pull request if you have fixed any bugs. Contact the developer directly (within GitHub) if you wish to add significant content")
                    )
))),
                    
    #H2O-NaCl tab content
            tabItem(tabName = "HN",
                fluidRow(
                    img(src = "HNp.png", height = 120, width = 240),
                    br(),
                    title = "Data Table", 
                        br(),
                        strong("Replace the values for Tmice and Th with your data"), 
                        br(), 
                        rHandsontableOutput("table"),
                        br(), 
                        strong("pressing save will export an updated table to the working directory, in the future this file can be downloaded"), 
                        actionButton("save", "Save")),
                fluidRow(
                    box(title = "Isochores", 
                        plotOutput("plot"))),
                fluidRow(
                    box(title = "Salinity vs. Th", 
                        plotOutput("plot2"))),
                fluidRow(
                    box(title = "References",
                        br(),
                        p("Atkinson, A.B. Jr., 2002, A Model for the PTX Properties of H2O-NaCl. Unpublished MSc Thesis, Dept. of Geosciences, Virginia Tech, Blacksburg VA, 133 pp."),
                        br(),
                        p("Bodnar, R.J., 1993, Revised equation and table for determining the freezing point depression of H2O-NaCl solutions, Geochimica et Cosmochimica Acta, vol. 57, p. 683-684"),
                        br(),
                        p("Bodnar, R.J., and Vityk, M.O., 1994, Interpretation of microthermometric data for H2O-NaCl fluid inclusions. In Fluid Inclusions in Minerals; Method and Applications, B. DeVivo & M.L. Frezzotti, eds., Virginia Polytechnic Institute and State University Press, Blacksburg, Virginia, p. 117-130"),
                        br(),
                        p("Knight, C.L., Bodnar,R.J.,1989, Synthetic fluid inclusions: IX Critical PVTX properties of NaCl–H2O solutions, Geochimica et Cosmochimica Acta, vol. 53, p. 3–8"),
                        br(),
                        p("Zhang, Y. and Frantz, J.D., 1987, Determination of the homogenization temperatures and densities of supercritical fluids in the system NaCl-KCl-CaCl2-H2O using synthetic fluid inclusions, Chemical Geology, vol. 64, pg. 335-350")))),
    
    #H2O-CO2-NaCl tab content
            tabItem(tabName = "HCN",
                fluidRow(p("content coming soon"))),

    #CO2-CH4 tab content
            tabItem(tabName = "CC",
                fluidRow(
                        rHandsontableOutput("tableCC"),
                        plotOutput("Thiery947b", click = "plot_click"),
                        verbatimTextOutput("CH4est")))
)))
    
server <- function(input, output) {

# # CO2-CH4 content
  

# CO2-CH4 dataframe
FIAid <- rep(NA_integer_, 10)
FIid <- rep(NA_integer_, 10)
Tmc <- rep(NA_integer_, 10)
VfrcVap <- rep(NA_integer_, 10)
Th <- rep(NA_integer_, 10)
HomToLorV <- rep(NA_integer_, 10)
CO2densAtTh <-  rep(NA_integer_, 10)
pureCO2molarVol <- rep(NA_integer_, 10)
estCH4 <- rep(NA_integer_, 10)
Bulkdens <- rep(NA_integer_, 10)
BulkMV <- rep(NA_integer_, 10)
CCdf <- data.frame(FIAid=FIAid, FIid=FIid, Tmc=Tmc, VfrcVap=VfrcVap, Th=Th, HomToLorV=HomToLorV, CO2densAtTh=CO2densAtTh, pureCO2molarVol=pureCO2molarVol, estCH4=estCH4, Bulkdens=Bulkdens, BulkMV=BulkMV)

#CO2-CH4 data table rendering
output$tableCC <- renderRHandsontable({
    rhandsontable(CCdf)%>%
        hot_col("CO2densAtTh", readOnly = TRUE) %>% 
        hot_col("pureCO2molarVol", readOnly = TRUE) %>% 
        hot_col("estCH4", readOnly = TRUE) %>% 
        hot_col("Bulkdens", readOnly = TRUE) %>% 
        hot_col("BulkMV", readOnly = TRUE)
})    

X_CH4 <- c(0.0:1.0, 0.01)
MV <- c(30:100, 1)
Tdf <- data.frame(X_CH4, MV)

output$Thiery947b <- renderPlot({
    ggplot(Tdf, aes(X_CH4, MV)) +
    geom_blank(ymin = 35) +
        ylim(35, 100) +
        annotation_raster(T947b, xmin = 0, xmax = 1, ymin = 35, ymax = 100) +
        coord_fixed(ratio = 0.01)
})

output$CH4est <- renderText({
    paste0("X_CH4=", input$plot_click$x, "\nMolar Volume=", input$plot_click$y)
})
   
# # H2O-NaCl content
 
#H20-NaCl dataframe
# FIAid <- rep(NA_real_, 10)
# FIid <- rep(NA_real_, 10)
# Tmice <- rep(NA_real_, 10)
# Th <- rep(NA_real_, 10)
# WtPctNaCl <- rep(NA_real_, 10)
# molality <- rep(NA_real_, 10)
# dens <- rep(NA_real_, 10)
# PatTh <-  rep(NA_real_, 10)
# Tcrit <- rep(NA_real_, 10)
# Pcrit <- rep(NA_real_, 10)
# dpdt <- rep(NA_real_, 10)

FIAid <- as.character(c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3))
FIid <- as.integer(c(1, 2, 3, 1, 2, 3, 4, 5, 1, 2))
Tmice <- c(-17.8, -18.0, -17.5, -9.6, -10, -9.5, -9, -10.2, -2, -2.5)
Th <- c(85, 92, 97, 120, 124, 125, 123, 120, 230, 233)
WtPctNaCl <- B93s(Tmice)
molality <- ZF87mol(WtPctNaCl)
dens <- ZF87rho(molality, Th)

if (Th > -21.2 & Th < 300) {
  PatTh = A02e1(Th, WtPctNaCl)
}
else if (Th > 300 & Th < 484) {
  PatTh = A02e2(Th, WtPctNaCl)
}
else {
  PatTh = A02e3(Th, WtPctNaCl)
}

Tcrit <- KB89ct(WtPctNaCl)
Pcrit <- KB89cp(Tcrit)
as <- 18.28 + 1.4413 * WtPctNaCl + 0.0047241 * WtPctNaCl^2 - 0.0024213 * WtPctNaCl^3 + 0.000038064 * WtPctNaCl^4
bs <- 0.019041 - 0.015268 * WtPctNaCl + 0.000566012 * WtPctNaCl^2 - 0.0000042329 * WtPctNaCl^3 - 0.000000030354 * WtPctNaCl^4
cs <- -0.00015988 + 0.000036892 * WtPctNaCl - 0.0000019473 * WtPctNaCl^2 + 0.000000041674 * WtPctNaCl^3 - 0.00000000033008 * WtPctNaCl^4
dpdt <- as + bs * Th + cs * Th^2

df <- data.frame(FIAid=FIAid, FIid=FIid, Tmice=Tmice, Th=Th, WtPctNaCl=WtPctNaCl, molality=molality, dens=dens, PatTh=PatTh, Tcrit=Tcrit, Pcrit=Pcrit, dpdt=dpdt)

#make H20-NaCl data values reactive
datavalues <- reactiveValues(data=df)

#H2O-NaCl data table rendering
output$table <- renderRHandsontable({
        rhandsontable(datavalues$data) %>%
        hot_col("WtPctNaCl", readOnly = TRUE) %>%
        hot_col("molality", readOnly = TRUE) %>%
        hot_col("dens", readOnly = TRUE) %>%
        hot_col("PatTh", readOnly = TRUE) %>%
        hot_col("Tcrit", readOnly = TRUE) %>%
        hot_col("Pcrit", readOnly = TRUE) %>%
        hot_col("dpdt", readOnly = TRUE) %>%
        hot_context_menu(allowColEdit = FALSE) %>%
        hot_validate_numeric(col = 3, min = -21.2, max = 0.1) %>%
        hot_validate_numeric(col = 4, min = 45, max = 600)
    })

output$plot <- renderPlot({
    ggplot(datavalues$data, aes(x=Th, y=PatTh)) + 
        geom_segment(aes(xend=1000, yend=dpdt * (1000 - dpdt) + PatTh), size=0.1, color="black") + 
        xlab("Temperature") + 
        ylab("Pressure") + 
        coord_cartesian(xlim = c(0, 600), ylim = c(0,6000)) +
        theme_cowplot() 
})

output$plot2 <- renderPlot({
    ggplot(datavalues$data, aes(Th, WtPctNaCl, color = FIAid, size = 3)) +
        geom_point() +
        theme_cowplot()
})

## If a change is made to the table, the following code will run to identify which row was changes, and recalculate as necessary
observeEvent(
        input$table$changes$changes,
        {
          
# capture the row which is changed            
            xi=input$table$changes$changes[[1]][[1]] 
            
# use hot_t_r to convert to r objects
            datavalues$data <- hot_to_r(input$table) 
            
##Calculate the salinity using equation 1 from Bodnar (1993)  
            datavalues$data[xi+1,5] = B93s(datavalues$data[xi+1,3]) 
            
##Calculate the molality from the salinity in weight percent NaCl using Zhang and Frantz (1987)
            datavalues$data[xi+1,6] = ZF87mol(datavalues$data[xi+1,5]) 
            
## Calculate the density (rho) using Zhang and Frantz (1987) for molality < 5, otherwise use Bodnar (1983)           
             if(datavalues$data[xi+1,6] < 5) {
               datavalues$data[xi+1,7] = ZF87rho(datavalues$data[xi+1,6], datavalues$data[xi+1,4])
                  }
             else if (datavalues$data[xi+1,6] > 5) {
                  datavalues$data[xi+1,7] = B83rho(datavalues$data[xi+1,4], datavalues$data[xi+1,5])
             }
              else {
                
              }
   
## Calculate the Pressure at Th using Atkinson (2002)        
            if (datavalues$data[xi+1,4] > -21.2 & datavalues$data[xi+1,4] < 300) {
              datavalues$data[xi+1,8] = A02e1(datavalues$data[xi+1,4], datavalues$data[xi+1,5])
            }
            else if (datavalues$data[xi+1,4] > 300 & datavalues$data[xi+1,4] < 484) {
              datavalues$data[xi+1,8] = A02e2(datavalues$data[xi+1,4], datavalues$data[xi+1,5])
            }
            else {
              datavalues$data[xi+1,8] = A02e3(datavalues$data[xi+1,4], datavalues$data[xi+1,5])
            }
            
#calculate the critical Temperature for the FI system using Knight and Bodnar (1989)            
            datavalues$data[xi+1,9] = KB89ct(datavalues$data[xi+1,5])

#calculate the critical Pressure for the FI system
            datavalues$data[xi+1,10] = KB89cp(datavalues$data[xi+1,9])
   
#calculate an isochore using Bodnar and Vityk (1994)
            datavalues$data[xi+1,11] = BV94isoch(datavalues$data[xi+1,5], datavalues$data[xi+1,4])         

}
)

saveData <- function(){
    write.csv(datavalues$data, file = "HNdata.csv", row.names = FALSE)
}

observeEvent(input$save, saveData())

}



shinyApp(ui, server)
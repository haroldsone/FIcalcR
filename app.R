## app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(tidyverse)
library(cowplot)
library(magick)
library(grid)
library(png)
library(plotly)
library(readxl)
library(gg3D)
source("HNFuncs.R")

T947b <- readPNG("www/Thiery947b2.png")

ui <- dashboardPage(
    #Header
    dashboardHeader(title = "FIcalcR"),
    
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Main", tabName = "Main"),
            menuItem("H2O-NaCl", tabName = "HN"),
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
)))),
                    
    #H2O-NaCl tab content
            tabItem(tabName = "HN",
                fluidRow(
                    img(src = "HNp.png", height = 120, width = 240),
                    br(),
                    title = "Data Table", 
                        br(),
                        strong("-Replace the values for Tm and Th with your data"), 
                        br(), 
                        strong("-In column 'phase': Enter the value for last phase to disappear (1 = ice, 2 = halite and 3 = hydrohalite)"),
                        br(),
                        strong("-New rows can be added by right-clicking on the table"),
                        br(),
                        p("-You can also add data by copying from an excel table. New rows will automatically be added, but you will need to prompt calculations by 'modifying' a data cell (just re-enter the same values) for each row below row 10"),
                        rHandsontableOutput("table"),
                fluidRow(
                        downloadButton("downloadHNt", "Download"),
                        strong("To save, add '.csv' to the end of the filename")),
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
                        p("Lecumberri-Sanchez, P., Steele-MacInnis, M., Bodnar, R.J., 2012, A numerical model to estimate trapping conditions of fluid inclusions that homogenize by halite disappearance, Geochimica et Cosmochimica Acta, vol. 92, pg. 14-22"),
                        br(),
                        p("Steel-MacInnis, M., Lecumberri-Sanchez, P., Bodnar, R.J., 2012, HokieFlincs_H2O-NaCl: A microsoft excel spreadsheet for interpreting microthermometric data from fluid inclusions based on the PVTX properties of H2O-NaCl, Computers & Geosciences, vol. 49, p. 334-337"),
                        br(),
                        p("Sterner, S.M., Hall, D.L., Bodnar, R.J., 1988, Synthetic fluid inclusions. V. Solubility relations in the system NaCl-KCl-H2O under vapor-saturated conditions, Geochimica et Cosmochimica Acta, vol. 52, p. 989-1006"))))),
    
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
 
#H20-NaCl dataframe with blank cells
# FIAid <- rep(NA_real_, 10)
# FIid <- rep(NA_real_, 10)
# Tm <- rep(NA_real_, 10)
# Th <- rep(NA_real_, 10)
# WtPctNaCl <- rep(NA_real_, 10)
# molality <- rep(NA_real_, 10)
# dens <- rep(NA_real_, 10)
# PatTh <-  rep(NA_real_, 10)
# Tcrit <- rep(NA_real_, 10)
# Pcrit <- rep(NA_real_, 10)
# dpdt <- rep(NA_real_, 10)

FIAid <- as.character(c(345, 345, 345, 20, 20, 20, 20, 20, 17, 17))
FIid <- as.integer(c(1, 2, 3, 1, 2, 3, 4, 5, 1, 2))
Tm <- as.numeric(c(-17.8, -18.0, -17.5, -9.6, -10, -9.5, 45, 47, -2, -2.5))
phase <- as.character(c(1, 1, 1, 1, 1, 1, 2, 2, 3, 3))
Th <- as.numeric(c(85, 92, 97, 120, 124, 125, 123, 120, 230, 233))
WtPctNaCl <- as.numeric(c(20.82, 20.97, 20.60, 13.51, 13.94, 13.40, 26.73, 26.77, 25.99, 25.92))
dens <- as.numeric(c(1.12, 1.12, 1.11, 1.04, 1.04, 1.03, 1.14, 1.14, 1.02, 1.01))
PatTh <- as.numeric(c(0.48, 0.63, 0.76, 1.81, 2.04, 2.12, 2.00, 1.53, 27.19, 23.35))
Tcrit <- as.numeric(c(581.21, 583.49, 577.88, 497.64, 501.61, 496.63, 706.71, 707.77, 686.12, 684.17))
Pcrit <- as.numeric(c(768.28, 773.90, 760.04, 552.63, 563.35, 549.88, 1092.84, 17.25, 1033.36, 1027.98))
dpdt <- as.numeric(c(27.92, 27.34, 26.92, 23.55, 23.39, 23.12, 24.35, 24.54, 17.98, 17.81))
message <- rep(NA_real_,10)
df <- data.frame(FIAid=FIAid, FIid=FIid, Tm=Tm, phase=phase, Th=Th, WtPctNaCl=WtPctNaCl, dens=dens, PatTh=PatTh, Tcrit=Tcrit, Pcrit=Pcrit, dpdt=dpdt, message=message)

phasetypes = c(1, 2, 3)

#make H20-NaCl data values reactive
datavalues <- reactiveValues(data=df)

#H2O-NaCl data table rendering
output$table <- renderRHandsontable({
        rhandsontable(datavalues$data) %>%
        hot_col("FIAid", type = "dropdown", allowInvalid = TRUE) %>%
        hot_col("phase", type = "dropdown", allowInvalid = FALSE) %>%
        hot_col("WtPctNaCl", readOnly = TRUE) %>%
        hot_col("dens", readOnly = TRUE) %>%
        hot_col("PatTh", readOnly = TRUE) %>%
        hot_col("Tcrit", readOnly = TRUE) %>%
        hot_col("Pcrit", readOnly = TRUE) %>%
        hot_col("dpdt", readOnly = TRUE) %>%
        hot_context_menu(allowColEdit = FALSE) %>%
        hot_validate_numeric(col = 3, min = -21.2, max = 801) %>%
        hot_validate_numeric(col = 4, min = 0, max = 1000) 
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
    ggplot(datavalues$data, aes(x=Th, y=WtPctNaCl, color = FIAid, size = 3)) +
        geom_point() +
        theme_cowplot()
})
 
## If a change is made to the table, the following code will recalculate as necessary
observeEvent(
        input$table$changes$changes,
        {
            
# use hot_t_r to convert to r objects
            datavalues$data <- hot_to_r(input$table)
            
#identify a specific row which changes            
            xi=input$table$changes$changes[[1]][[1]]
            
##Calculate the salinity using equation 1 from Bodnar (1993) for Tm = ice, for Tm = halite or hydrohalite the salinity is calculated using Sterner, Hall and Bodnar (1988), unless Tm > Th and 'halite' is selected, in which case Lecumberri-Sanchez, Steele-MacInnis, Bodnar (2012) is used
            ifelse(
              datavalues$data[xi+1,4] == 1, 
                datavalues$data[xi+1,6] <- B93s(datavalues$data[xi+1,3]),
              ifelse(
                (datavalues$data[xi+1,3] < datavalues$data[5]) & (datavalues$data[xi+1,4] == 2),
                datavalues$data[xi+1,6] <- SHB88Zs(datavalues$data[xi+1,3]), 
              ifelse(
                (datavalues$data[xi+1,3] > datavalues$data[xi+1,5]) & (datavalues$data[xi+1,4] == 2),
                datavalues$data[xi+1,6] <- LSB12s(datavalues$data[xi+1,3],datavalues$data[xi+1,5]), 
              ifelse(
                datavalues$data[xi+1,4] == 3,
                datavalues$data[xi+1,6] <- SHB88hhs(datavalues$data[xi+1,3]),
                  datavalues$data[xi+1,12] <- NA_real_))))
            
## Calculate the density (rho) using Bodnar (1983)           
            ifelse(
              datavalues$data[4] == 2 & datavalues$data[3] > datavalues$data[5],
              datavalues$data[7] <- LSB12rho(datavalues$data[3],datavalues$data[5]),datavalues$data[7] <-  B83rho(datavalues$data[5], datavalues$data[6]))

   
## Calculate the Pressure at Th using Atkinson (2002) or LSB12
  ifelse(
    datavalues$data[xi+1,3] > datavalues$data[xi+1,5],
      datavalues$data[xi+1,8] <- LSB12p(datavalues$data[xi+1,3],datavalues$data[xi+1,5]),
    ifelse(
      datavalues$data[xi+1,3] < datavalues$data[xi+1,5] & datavalues$data[xi+1,5] > -21.2 & datavalues$data[xi+1,5] < 300,
      datavalues$data[xi+1,8] <- A02e1(datavalues$data[xi+1,5],datavalues$data[xi+1,6]),
    ifelse(
      datavalues$data[xi+1,3] < datavalues$data[5] & datavalues$data[xi+1,5] > 300 & datavalues$data[xi+1,5] < 484,
      datavalues$data[xi+1,8] <- A02e2(datavalues$data[xi+1,5], datavalues$data[xi+1,6]),
    ifelse(
      datavalues$data[xi+1,3] < datavalues$data[xi+1,5] & datavalues$data[xi+1,5] > 484,
      datavalues$data[xi+1,8] <- A02e3(datavalues$data[xi+1,5], datavalues$data[xi+1,6]),
      datavalues$data[xi+1,8] <- NA_real_))))
            
#calculate the critical Temperature for the FI system using Knight and Bodnar (1989)
  datavalues$data[xi+1,9] <- if(datavalues$data[xi+1,3] < datavalues$data[xi+1,5]){
      KB89ct(datavalues$data[xi+1,6])
  } else {
      datavalues$data[xi+1,9] <- NA_real_
  }

#calculate the critical Pressure for the FI system
              ifelse(
                (datavalues$data[xi+1,3] < datavalues$data[xi+1,5]) & (datavalues$data[xi+1,4] == "2"),
                  datavalues$data[xi+1,10] <- LSB12isoch(datavalues$data[xi+1,3],datavalues$data[xi+1,5]),
                  datavalues$data[xi+1,10] <-  KB89cp(datavalues$data[xi+1,9]))
   
#calculate an isochore using Bodnar and Vityk (1994)
            datavalues$data[xi+1,11] = BV94isoch(datavalues$data[xi+1,6], datavalues$data[xi+1,5]) 
            
  
      
            
## TODO: Need to create/fix error handling statements
                      ifelse(datavalues$data[xi+1,5] > datavalues$data[xi+1,9], datavalues$data[xi+1,12] <- "Warning: Your Th is > Tcrit", datavalues$data[xi+1,12] <- NA_real_)
            
            # ifelse(
            #   datavalues$data[xi+1,3] > -21.2 & datavalues$data[xi+1,3] < 0 & datavalues$data[xi+1,4] == 1, 
            #       datavalues$data[xi+1,12] <- NA_real_, 
            #       datavalues$data[xi+1,12] <- "Warning: Tm outside range ice")
            # 
            # ifelse(
            #   datavalues$data[xi+1,3] > 0.1 & datavalues$data[xi+1,3] < 801 & datavalues$data[xi+1,4] == 2,
            #       datavalues$data[xi+1,12] <- NA_real_, 
            #       datavalues$data[xi+1,12] <- "Warning: Tm outside range for halite")
}
)

# Downloadable csv of selected dataset ----
output$downloadHNt <- downloadHandler(
  filename = function() {
    paste("FIcalcRdata", Sys.Date(), ".csv", sep = "_")
  },
  content = function(file) {
    write.csv2(datavalues$data, file, row.names = FALSE)
  }
)


}

shinyApp(ui, server)
## app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(readxl)
library(tidyverse)

ui <- dashboardPage(
    #Header
    dashboardHeader(title = "FIcalcR"),
    #Sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("H2O-NaCl", tabName = "HN"),
            menuItem("H2O-CO2-NaCl", tabName = "HCN", badgeLabel = "coming soon", badgeColor = "green")
        )
    ),
    #Main Body
    dashboardBody(
        tabItems(
    #First tab content
            tabItem(tabName = "HN",
                fluidRow(
                    img(src = "HNp.png", height = 120, width = 240, align = "center")),
                fluidRow(
                    box(title = "Data Table", 
                        br(),
                        strong("Replace the values for Tmice and Th with your data"), 
                        br(), 
                        rHandsontableOutput("table"), 
                        br(), 
                        strong("pressing save will export an updated table to the working directory"), 
                        actionButton("calc", "Save")),
                    box(title = "Isochores", plotOutput("plot")))),
                
    #Second tab content
            tabItem(tabName = "HCN",
                 fluidRow(p("content coming soon")
)))))
    
server <- function(input, output) {

FIid <- c(1:5)
Tmice <- c(-1, -1, -5, -6, -10)
Th <- c(85, 92, 140, 160, 250)
WtPctNaCl <- (0.00 + 1.78 * abs(Tmice) - 0.0442 * abs(Tmice)^2 + 0.000557 * abs(Tmice)^3)
t <- 18.01534 * WtPctNaCl / (18.01534 * WtPctNaCl + 58.4428 * (100 - WtPctNaCl))
molality <- 55.55 * t / (1 - t)
g1 <- 1.0014
g2 <- -0.00022323
g3 <- -0.0000013472
g4 <- -0.0000000046597
g5 <- 0.023547
g6 <- 0.0045636
g7 <- 0.00048805
g8 <- -0.00006498
g9 <- -0.000053074
g10 <- 0.000001009
m <- molality
dens <- (g1 + g5*m + g6*m^2 + g7*m^3) + (g2 + g8*m + g9*m^2) * Th + (g3 + g10*m) * Th^2 + g4*Th^3
tr <- Th/100
x1 <- exp(-5.38 + 0.0688*Th - 0.000208*Th^2 + 0.000000296*Th^3)
x2 <- -135.99 + 304.37*tr - 236.18*tr^2 + 78.625*tr^3 - 10.094*tr^4 + 0.4244*tr^5
PatTh <-  ifelse(Th > -21.2 & Th < 180.01, x1, x2)
Tcrit <- 374.1 + 8.8*WtPctNaCl + 0.1771*WtPctNaCl^2 - 0.02113*WtPctNaCl^3 + 0.0007334*WtPctNaCl^4
Pcrit <- 2094 - 20.56*Tcrit + 0.06896*Tcrit^2 - 0.00008903*Tcrit^3 + 0.00000004214*Tcrit^4
as <- 18.28 + 1.4413 * WtPctNaCl + 0.0047241 * WtPctNaCl^2 - 0.0024213 * WtPctNaCl^3 + 0.000038064 * WtPctNaCl^4
bs <- 0.019041 - 0.015268 * WtPctNaCl + 0.000566012 * WtPctNaCl^2 - 0.0000042329 * WtPctNaCl^3 - 0.000000030354 * WtPctNaCl^4
cs <- -0.00015988 + 0.000036892 * WtPctNaCl - 0.0000019473 * WtPctNaCl^2 + 0.000000041674 * WtPctNaCl^3 - 0.00000000033008 * WtPctNaCl^4
dpdt <- as + bs * Th + cs * Th^2

df <- data.frame(FIid=FIid, Tmice=Tmice, Th=Th, WtPctNaCl=WtPctNaCl, molality=molality, dens=dens, PatTh=PatTh, Tcrit=Tcrit, Pcrit=Pcrit, dpdt=dpdt)

datavalues <- reactiveValues(data=df)

output$table <- renderRHandsontable({
        rhandsontable(datavalues$data) %>%
        hot_col("WtPctNaCl", readOnly = TRUE) %>%
        hot_col("molality", readOnly = TRUE) %>%
        hot_col("dens", readOnly = TRUE) %>%
        hot_col("PatTh", readOnly = TRUE) %>%
        hot_col("Tcrit", readOnly = TRUE) %>%
        hot_col("Pcrit", readOnly = TRUE) %>%
        hot_col("dpdt", readOnly = TRUE)
    })

observeEvent(
        input$table$changes$changes,
        {
            
            xi=input$table$changes$changes[[1]][[1]] # capture the row which is changed
            
            datavalues$data <- hot_to_r(input$table) 
            
            datavalues$data[xi+1,4] = 0.00 + 1.78 * abs(datavalues$data[xi+1,2]) - 0.0442 * abs(datavalues$data[xi+1,2])^2 + 0.000557 * abs(datavalues$data[xi+1,2])^3 #calculate the salinity using equation 1 from Bodnar (1993)
            
            t <- 18.01534 * datavalues$data[xi+1,4] / (18.01534 * datavalues$data[xi+1,4] + 58.4428 * (100 - datavalues$data[xi+1,4]))
            datavalues$data[xi+1,5] = 55.55 * t / (1 - t) # calculate molality
            
            datavalues$data[xi+1,6] = ifelse(datavalues$data[xi+1,5] < 5, (g1 + g5*datavalues$data[xi+1,5] + g6*datavalues$data[xi+1,5]^2 + g7*datavalues$data[xi+1,5]^3) + (g2 + g8*datavalues$data[xi+1,5] + g9*datavalues$data[xi+1,5]^2) * datavalues$data[xi+1,3] + (g3 + g10*datavalues$data[xi+1,5]) * datavalues$data[xi+1,3]^2 + g4*datavalues$data[xi+1,3]^3, NA_real_) #calculate the density using molality (m) into equation 22 from Zhang and Frantz (1987), Chemical Geology, vol. 64, pg. 335-350
            
            datavalues$data[xi+1,7] = ifelse(datavalues$data[xi+1,3] > -21.2 & datavalues$data[xi+1,3] < 180.01, exp(-5.38 + 0.0688*datavalues$data[xi+1,3] - 0.000208*datavalues$data[xi+1,3]^2 + 0.000000296*datavalues$data[xi+1,3]^3), -135.99 + 304.37*(datavalues$data[xi+1,3]/100) - 236.18*(datavalues$data[xi+1,3]/100)^2 + 78.625*(datavalues$data[xi+1,3]/100)^3 - 10.094*(datavalues$data[xi+1,3]/100)^4 + 0.4244*(datavalues$data[xi+1,3]/100)^5) #calculate pressure along L-V curve at Th
            
            datavalues$data[xi+1,8] = 374.1 + 8.8*datavalues$data[xi+1,4] + 0.1771*datavalues$data[xi+1,4]^2 - 0.02113*datavalues$data[xi+1,4]^3 + 0.0007334*datavalues$data[xi+1,4]^4 #calculate the critical Temperature for the FI system
            
            datavalues$data[xi+1,9] = 2094 - 20.56*datavalues$data[xi+1,8] + 0.06896*datavalues$data[xi+1,8]^2 - 0.00008903*datavalues$data[xi+1,8]^3 + 0.00000004214*datavalues$data[xi+1,8]^4 #calculate the critical Pressure for the FI system
            
            datavalues$data[xi+1,10] = (18.28 + 1.4413 * datavalues$data[xi+1,4] + 0.0047241 * datavalues$data[xi+1,4]^2 - 0.0024213 * datavalues$data[xi+1,4]^3 + 0.000038064 * datavalues$data[xi+1,4]^4) + (0.019041 - 0.015268 * datavalues$data[xi+1,4] + 0.000566012 * datavalues$data[xi+1,4]^2 - 0.0000042329 * datavalues$data[xi+1,4]^3 - 0.000000030354 * datavalues$data[xi+1,4]^4) * datavalues$data[xi+1,3] + (-0.00015988 + 0.000036892 * datavalues$data[xi+1,4] - 0.0000019473 * datavalues$data[xi+1,4]^2 + 0.000000041674 * datavalues$data[xi+1,4]^3 - 0.00000000033008 * datavalues$data[xi+1,4]^4) * (datavalues$data[xi+1,3]^2) #calculate an isochore using Bodnar and Vityk (1994)

}
)

output$plot <- renderPlot({
    ggplot(datavalues$data, aes(x=Th, y=PatTh)) + 
        geom_segment(aes(xend=1000, yend=dpdt * (1000 - dpdt) + PatTh), size=0.1, color="black") + 
        xlab("Temperature") + 
        ylab("Pressure") + 
        coord_cartesian(xlim = c(0, 600), ylim = c(0,6000)) +
        theme_minimal() +
        facet_grid(FIid)
    })

saveData <- function(){
    write.csv(datavalues$data, file = "HNdata.csv", row.names = FALSE)
}

observeEvent(input$calc, saveData())
}

shinyApp(ui, server)
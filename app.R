#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(caret)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#####################################################################################################################

#Por barrio
#Cargar datos de barrio

cargarBarrio <- function(){

    t2014 <- read.csv("./data/Accidentalidad_georreferenciada_2014.csv", )
    t2014 <- select(t2014,5, 15)
    t2014$FECHA <- substring(t2014$FECHA, 6,10)
    
    t2015 <- read.csv("./data/Accidentalidad_georreferenciada_2015.csv")
    t2015 <- select(t2015,5, 15)
    t2015$FECHA <- substring(t2015$FECHA, 6,10)
    
    t2016 <- read.csv("./data/Accidentalidad_georreferenciada_2016.csv")
    t2016 <- select(t2016,5, 15)
    t2016$FECHA <- substring(t2016$FECHA, 6,10)
    
    t2017 <- read.csv("./data/Accidentalidad_georreferenciada_2017.csv")
    t2017 <- select(t2017,5, 15)
    t2017$FECHA <- substring(t2017$FECHA, 6,10)
    
    t2018 <- read.csv("./data/Accidentalidad_georreferenciada_2018.csv")
    t2018 <- select(t2018,5, 15)
    t2018$FECHA <- substring(t2018$FECHA, 6,10)
    
    porBarrio2014 <- group_by(t2014, BARRIO, FECHA)
    porBarrio2014 <- summarize(porBarrio2014, AÑO_2014 = n())
    
    porBarrio2015 <- group_by(t2015, BARRIO, FECHA)
    porBarrio2015 <- summarize(porBarrio2015, AÑO_2015 = n())
    
    porBarrio2016 <- group_by(t2016, BARRIO, FECHA)
    porBarrio2016 <- summarize(porBarrio2016, AÑO_2016 = n())
    
    porBarrio2017 <- group_by(t2017, BARRIO, FECHA)
    porBarrio2017 <- summarize(porBarrio2017, AÑO_2017 = n())
    
    porBarrio2018 <- group_by(t2018, BARRIO, FECHA)
    porBarrio2018 <- summarize(porBarrio2018, AÑO_2018 = n())
    
    #Join de tablas
    PorBarrioT <- merge(porBarrio2014, porBarrio2015, by = c("BARRIO","FECHA"), all = TRUE)
    PorBarrioT <- merge(PorBarrioT, porBarrio2016, by = c("BARRIO","FECHA"), all = TRUE)
    PorBarrioT <- merge(PorBarrioT, porBarrio2017, by = c("BARRIO","FECHA"), all = TRUE)
    PorBarrioT <- merge(PorBarrioT, porBarrio2018, by = c("BARRIO","FECHA"))
    PorBarrioT[is.na(PorBarrioT)] <- 0
    
    return(PorBarrioT)
}
#####################################################################################################################

#Por comuna
#Cargar datos de comuna
cargarComuna <- function(){
    
    t2014 <- read.csv("./data/Accidentalidad_georreferenciada_2014.csv", )
    t2014 <- select(t2014,5, 16)
    t2014$FECHA <- substring(t2014$FECHA, 6,10)
    
    t2015 <- read.csv("./data/Accidentalidad_georreferenciada_2015.csv")
    t2015 <- select(t2015,5, 16)
    t2015$FECHA <- substring(t2015$FECHA, 6,10)
    
    t2016 <- read.csv("./data/Accidentalidad_georreferenciada_2016.csv")
    t2016 <- select(t2016,5, 16)
    t2016$FECHA <- substring(t2016$FECHA, 6,10)
    
    t2017 <- read.csv("./data/Accidentalidad_georreferenciada_2017.csv")
    t2017 <- select(t2017,5, 16)
    t2017$FECHA <- substring(t2017$FECHA, 6,10)
    
    t2018 <- read.csv("./data/Accidentalidad_georreferenciada_2018.csv")
    t2018 <- select(t2018,5, 16)
    t2018$FECHA <- substring(t2018$FECHA, 6,10)
    
    porComuna2014 <- group_by(t2014, COMUNA, FECHA)
    porComuna2014 <- summarize(porComuna2014, AÑO_2014 = n())
    
    porComuna2015 <- group_by(t2015, COMUNA, FECHA)
    porComuna2015 <- summarize(porComuna2015, AÑO_2015 = n())
    
    porComuna2016 <- group_by(t2016, COMUNA, FECHA)
    porComuna2016 <- summarize(porComuna2016, AÑO_2016 = n())
    
    porComuna2017 <- group_by(t2017, COMUNA, FECHA)
    porComuna2017 <- summarize(porComuna2017, AÑO_2017 = n())
    
    porComuna2018 <- group_by(t2018, COMUNA, FECHA)
    porComuna2018 <- summarize(porComuna2018, AÑO_2018 = n())
    
    #Join de tablas
    PorComunaT <- merge(porComuna2014, porComuna2015, by = c("COMUNA","FECHA"), all = TRUE)
    PorComunaT <- merge(PorComunaT, porComuna2016, by = c("COMUNA","FECHA"), all = TRUE)
    PorComunaT <- merge(PorComunaT, porComuna2017, by = c("COMUNA","FECHA"), all = TRUE)
    PorComunaT <- merge(PorComunaT, porComuna2018, by = c("COMUNA","FECHA"), all = TRUE)
    PorComunaT[is.na(PorComunaT)] <- 0
    
    return(PorComunaT)
}

#####################################################################################################################

mse_k2018<-function(k,data_tr,data_vl,formula_mod){
    adv_knn<-knnreg(formula_mod,data=datos_tr,k=k) 
    y_tr_pred<-predict(adv_knn,datos_tr) 
    mse_tr<-mean((datos_tr$AÑO_2018-y_tr_pred)^2) 
    y_vl_pred<-predict(adv_knn,datos_vl) 
    mse_vl<-mean((datos_vl$AÑO_2018-y_vl_pred)^2) 
    return(list(mse_tr=mse_tr,mse_vl=mse_vl))
}

set.seed(98956561)

diario2018 <- function(datos){
    p_tr<-0.7
    N_datos<-dim(datos)[1]
    n_tr<-round(N_datos*p_tr)
    ix_tr<-sample(N_datos,n_tr,replace = FALSE)
    datos_tr<-datos[ix_tr,]
    datos_vl<-datos[-ix_tr,] 
    
    modelo <- formula("AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017")
    
    num_vec<-1:1000   
    tryCatch( { MSE <- lapply(num_vec,mse_k2018,data_tr=data_tr,data_vl = data_vl,formula_mod = modelo)}, error = function(e) {an.error.occured <<- TRUE})
    num_vec <- (1:length(MSE))
    mse_tr<-sapply(num_vec,function(x,y){`[[`(y,x)$mse_tr},y=MSE)
    mse_vl<-sapply(num_vec,function(x,y){`[[`(y,x)$mse_vl},y=MSE)
    new_k <- which.min(abs(mse_tr-mse_vl))
    
    
    return(new_k)
}

#####################################################################################################################

comuna2018 <- function(tipo){
    DatosComuna <- cargarComuna()
    
    adv_knn<-knnreg(AÑO_2018~AÑO_2014+AÑO_2015+AÑO_2016+AÑO_2017,data=DatosComuna,k=99) 
    y_tr_pred<-predict(adv_knn,DatosComuna) 
    
    comunaPred <- DatosComuna 
    comunaPred["AÑO_2018_PRED"] <- y_tr_pred
    
    comunaPred["FECHA"] <- as.Date(strptime(comunaPred$FECHA,format="%m-%d"), format="%m-%d")
    
    if(tipo == "MES"){
        comunaPred[tipo] <- format(comunaPred$FECHA, format="%m")
        comunaPred <- group_by(comunaPred, MES, COMUNA)
        comunaPred <- summarize(comunaPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
    }else if(tipo == "SEMANA"){
        comunaPred["SEMANA"] <- format(comunaPred$FECHA, format="%W")
        comunaPred <- group_by(comunaPred, SEMANA, COMUNA)
        comunaPred <- summarize(comunaPred, AÑO_2018_PRED = sum(AÑO_2018_PRED))
    }else if(tipo == "DIA"){
        comunaPred <- select(comunaPred,1, 2, 8)
        comunaPred["FECHA"] <- format(comunaPred$FECHA, format="%m-%d")
    }
    return(comunaPred)
}

tablita <- comuna2018("SEMANA")

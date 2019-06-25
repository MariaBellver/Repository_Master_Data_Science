library(shiny)
library(rsconnect)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)




ui<-navbarPage(theme = shinytheme("flatly"),"App Master Ciencia de Datos ", 
               
               tabPanel("Selección de la máquina", 
                        sidebarLayout(
                          sidebarPanel(
                            
                            fileInput("Datos", "Selecciona el fichero de datos", accept=NULL),
                            uiOutput("Maquina")
                            
                          ),
                          mainPanel("Probabilidad de orden",
                                    plotOutput("porden"))
                        )
               ),
               
               navbarMenu("Estado de la máquina",
                          tabPanel("Evolución Temporal Alarmas",
                                   sidebarLayout(
                                     sidebarPanel(h5("ALARMAS"),
                                       uiOutput("Alarma")
                                     ),
                                     mainPanel("Evolución temporal de alarmas",
                                               plotOutput("alarm"))
                                   )
                          ),
                          tabPanel("Registros de la máquina",
                                   sidebarLayout(
                                     sidebarPanel(
                                       uiOutput("Alarma2")
                                     ),
                                     mainPanel("Registros de la máquina seleccionada",
                                               DT::dataTableOutput("alarm2"))
                                   )
                          )
               ),
               
               tabPanel("Estadíasitcas globales temporales",
                        sidebarLayout(
                          sidebarPanel(h5("PERIODO Y ESTADÍSTICAS"),
                                       uiOutput("Date3"),
                                       h5("HISTOGRAMA"),
                                       uiOutput("Alarma3"),
                                       uiOutput("slider3"),
                                       uiOutput("todas")
                          ),
                          mainPanel("Histograma de la alarma seleccionada",
                                    plotOutput("hist"),plotOutput("box"))
                        )
               )
)






server<-function(input, output) {
  
  Datos<-reactive({
    data<-input$Datos
    nombre_variable<-load(data$datapath)   
    d<-eval(parse(text = nombre_variable)) 
    vars<-list()
    vars$DataFrame<-d
    vars$matriculas<-as.list(unique(d$matricula))
    vars$alarmas<-names(d[4:48])
    return(vars)
    
  })
  ######################################################
  output$Maquina<-renderUI({
    selectInput('Sel_maq',"Selecciona máquina" ,
                Datos()$matriculas)
  })
  
  
  
  output$porden <- renderPlot({
    req(input$Sel_maq)
    #maquina<-input$Sel
    datos_porden<-Datos()$DataFrame%>%filter(matricula==input$Sel_maq)
    ggplot(datos_porden ,aes(x=dia,y=p_orden,col=p_orden))+
      geom_point()+geom_line()+scale_color_gradient(low="blue", high="red")
  })
  
  ############################################   
  output$Alarma<-renderUI({
    
    radioButtons("Sel_alarm", "Selecciona la alarma a visualizar",
                 Datos()$alarmas)
  })
  
  
  
  output$alarm <- renderPlot({
    req(input$Sel_alarm)
    #Alarma<-input$Sel_alarm
    datos_porden<-Datos()$DataFrame%>%filter(matricula==input$Sel_maq)
    ggplot(datos_porden,aes_string(x="dia",y=input$Sel_alarm))+
      geom_point()+geom_line()
  })
  
  
  ################################################
  
  output$Alarma2<-renderUI({
    checkboxGroupInput(inputId = 'Sel_alarm2',
                       label = "Selecciona la alarma a visualizar",
                       choices = Datos()$alarmas,
                       selected = "a2")
  })
  
  
  output$alarm2 <- DT::renderDataTable({
    
    datos_porden<-Datos()$DataFrame%>%filter(matricula==input$Sel_maq)
    
    alarm_selected <- reactive({
      req(input$Sel_alarm2) 
      datos_porden %>% select(matricula,dia,input$Sel_alarm2,p_orden)
    })
    
    DT::datatable(data = alarm_selected(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    
  })
  
  ########################################################
  output$Date3<-renderUI({
    dateRangeInput("RangoFechas","Selecciona el periodo", 
                   start = "2016-01-02", 
                   end ="2016-12-14", 
                   min = NULL, 
                   max = NULL, 
                   format = "yyyy-mm-dd",
                   weekstart = 1,
                   language = "es", 
                   separator = "a")
  })
  
  
  output$slider3<-renderUI({
    sliderInput("bins", "Ancho del bin del histograma", 
                min= 1, 
                max = 50, 
                value = 5, 
                step= 1)
  })
  
  
  
  output$Alarma3<-renderUI({
    selectInput('Sel_alarm3',"Alarma",
                Datos()$alarmas)
  })
  
  output$todas<-renderUI({
    checkboxInput('Sel_todas', "Todas las máquinas", 
                  value = FALSE, width = NULL)
  })
  
  output$hist <- renderPlot({
    
    datos_porden<-Datos()$DataFrame%>%filter(matricula==input$Sel_maq,dia>input$RangoFechas[1],dia<input$RangoFechas[2])
    req(input$Sel_alarm3)
    
    ggplot(datos_porden,aes_string(input$Sel_alarm3))+
      geom_histogram(binwidth =input$bins)
    
  })
  
  output$box <- renderPlot({
    if(input$Sel_todas==F){
      datos_porden<-Datos()$DataFrame%>%filter(matricula==input$Sel_maq,dia>input$RangoFechas[1],dia<input$RangoFechas[2])
      
    }else{
      datos_porden<-Datos()$DataFrame%>%filter(dia>input$RangoFechas[1],dia<input$RangoFechas[2])
      
    }
    
    req(input$Sel_alarm3)
    
    ggplot(datos_porden,aes_string("matricula",input$Sel_alarm3))+
      geom_boxplot(binwidth =input$bins)
    
    
  })
  
  
}



shinyApp(ui=ui, server=server)
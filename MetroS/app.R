library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(plotly)
library(knitr)
library(tseries)
ui <- fluidPage(theme = shinytheme("superhero"),
    titlePanel('Metro de la CDMX'),
    sidebarLayout(
        sidebarPanel(
          img(src = "logo.png", height = 90, width = 90),
            uiOutput('linea'),
            checkboxInput('log','Escala de logaritmica ',value=F),
            checkboxInput('dif','Diferencia',value=F),
             checkboxInput('cov','Sin Covid',value=F)
            
        ),
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel("Vista individual",plotlyOutput('graf1')),
                        tabPanel("Desglose",plotlyOutput('des')),
                        tabPanel('Media y Varianza de los datos',plotOutput('graf2')),
                        tabPanel("ACF",plotOutput('graf3')),
                        tabPanel("PACF",plotOutput('graf4')),
                        tabPanel("Prediccion",plotOutput('graf5')))
        )
    )
    
)


server <- function(input, output) {
    
    datos<-reactive({
        df<-read.csv('./informacion-ingresos-cierre-2012_03-2021-ok.csv')
        return(df)
    
    })
    
    TS<-reactive({
      if(input$linea!='Todas'){
        met=datos()%>%select(FECHA,paste('LINEA',input$linea,sep = '_'))
        met$FECHA %<>% as.Date("%d/%m/%Y")
        met %<>% filter(FECHA > "2014-01-01")
        names(met)[2]<-'total'
        met%<>% mutate(dia=format(met$FECHA, "%d"),
                       mes=format(met$FECHA, "%m"), 
                       anio = format(met$FECHA, "%Y")) %>% 
          group_by(anio,mes,dia) %>% 
          summarise(Total = sum(total)) %>% 
          group_by(anio,mes) %>% 
          summarise(Total = sum(Total))
        
        S=ts(met$Total,start = c(2014,1),end =c(2021,3),frequency = 12)
        
      }
      else{
        met=datos()
        met$Total= met[,-1:-2] %>% rowSums()
        met$FECHA %<>% as.Date("%d/%m/%Y")
        met %<>% filter(FECHA > "2014-01-01")
        met%<>% mutate(dia=format(met$FECHA, "%d"),
                       mes=format(met$FECHA, "%m"), 
                       anio = format(met$FECHA, "%Y")) %>% 
          group_by(anio,mes,dia) %>% 
          summarise(Total = sum(Total)) %>% 
          group_by(anio,mes) %>% 
          summarise(Total = sum(Total))
        S=ts(met$Total,start = c(2014,1),end =c(2021,3),frequency = 12)
      
      }
      
      if(input$log){
        S %<>% log() 
      }
 
      if(input$dif){return(diff(S))}
      else{return(S)}
    
      })
    
    g1<-reactive({
      
      met=datos()
      met$Total= met[,-1:-2] %>% rowSums()
      met$FECHA %<>% as.Date("%d/%m/%Y")
      met %<>% filter(FECHA > "2014-01-01")
      met%<>% mutate(dia=format(met$FECHA, "%d"),
                     mes=format(met$FECHA, "%m"), 
                     anio = format(met$FECHA, "%Y")) %>% 
        group_by(anio,mes,dia) %>% 
        summarise(Total = sum(Total)) %>% 
        group_by(anio,mes) %>% 
        summarise(Total = sum(Total))
      S=ts(met$Total,start = c(2014,1),end =c(2021,3),frequency = 12)
      
    
    if(input$log){
      S %<>% log() 
      
    } 
    return(S)
      
    })
    
    g2<-reactive({
      
      met=datos()
      met$Total= met[,-1:-2] %>% rowSums()
      met$FECHA %<>% as.Date("%d/%m/%Y")
      met %<>% filter(FECHA < "2020-02-01", FECHA > "2014-01-01")
      met%<>% mutate(dia=format(met$FECHA, "%d"),
                     mes=format(met$FECHA, "%m"), 
                     anio = format(met$FECHA, "%Y")) %>% 
        group_by(anio,mes,dia) %>% 
        summarise(Total = sum(Total)) %>% 
        group_by(anio,mes) %>% 
        summarise(Total = sum(Total))
      tBol=ts(met$Total,start = c(2014,1),end =c(2020,1),frequency = 12)
    
    
    if(input$log){
      tBol %<>% log() 
    }
    
    return(tBol)
    })
    
    output$linea<-renderUI({
        l<-c('1':'9','A','B','12','Todas')
        selectInput('linea','Linea del metro',
                    choices = l,
                    multiple = F)
    })
    
    MsC<-reactive({
      df=datos()
      if(input$linea!='Todas'){
        met=df%>%select(FECHA,paste('LINEA',input$linea,sep = '_'))
        met$FECHA %<>% as.Date("%d/%m/%Y")
        met %<>% filter(FECHA < "2020-02-01", FECHA > "2014-01-01")
        names(met)[2]<-'total'
        met%<>% mutate(dia=format(met$FECHA, "%d"),
                       mes=format(met$FECHA, "%m"), 
                       anio = format(met$FECHA, "%Y")) %>% 
          group_by(anio,mes,dia) %>% 
          summarise(Total = sum(total)) %>% 
          group_by(anio,mes) %>% 
          summarise(Total = sum(Total))
        
        tBol=ts(met$Total,start = c(2014,1),end =c(2020,1),frequency = 12)
        
      }
      else{
        met=df
        met$Total= met[,-1:-2] %>% rowSums()
        met$FECHA %<>% as.Date("%d/%m/%Y")
        met %<>% filter(FECHA < "2020-02-01", FECHA > "2014-01-01")
        met%<>% mutate(dia=format(met$FECHA, "%d"),
                       mes=format(met$FECHA, "%m"), 
                       anio = format(met$FECHA, "%Y")) %>% 
          group_by(anio,mes,dia) %>% 
          summarise(Total = sum(Total)) %>% 
          group_by(anio,mes) %>% 
          summarise(Total = sum(Total))
        tBol=ts(met$Total,start = c(2014,1),end =c(2020,1),frequency = 12)
      }
      
      if(input$log){
        tBol %<>% log() 
      }
      if(input$dif){return(diff(tBol))}
      else{return(tBol)}
    })
 
    
    
    output$graf1<-renderPlotly({
        coloresM<-c('hotpink2','deepskyblue','darkolivegreen3',
                    'mediumaquamarine','yellow2','red2',
                    'orangered','darkgreen','brown',
                    'purple1','gray48','gold4','Black')
        if(input$cov){
        
        if(input$linea!='Todas'){
          titulo= paste('Linea ',input$linea,'sin Covid')
        }
        else{
          titulo="Ingresos totales del Metro sin covid"
         }
        }
        else{
          if(input$linea!='Todas'){
            titulo= paste('Linea ',input$linea,"con Covid")
          }
          else{
            titulo="Ingresos totales del Metro con Covid"
          }
        }
        if(input$linea=='A'){
          c<-10
        }
        else if(input$linea=='B'){
          c<-11
        }
        else if(input$linea=='Todas'){
          c<-13
        }
        else{
          c<-as.integer(input$linea)
        }
        if(input$cov){
          t=MsC()
        }
        else{
          t=TS()
        }
    autoplot(t,col=coloresM[c],ylab=paste('Venta de ',input$tipo),
            main=titulo)
        
        
    })
    
    output$des<-renderPlotly({
      if(input$cov){
        t=MsC()
        if(input$linea!='Todas'){
          titulo= paste('Linea ',input$linea,'sin Covid')
        }
        else{
          titulo="Ingresos totales del Metro sin covid"
        }
      }
      else{
        if(input$linea!='Todas'){
          titulo= paste('Linea ',input$linea,"con Covid")
        }
        else{
          titulo="Ingresos totales del Metro con Covid"
        }
        t=TS()
      }
      ggseasonplot(t,year.labels = TRUE,year.labels.left = TRUE)+
        ylab("Ingresos")+
        ggtitle(titulo)+xlab("Meses")
    })
    
    output$graf2<-renderPlot({
     coloresM<-c('hotpink2','deepskyblue','darkolivegreen3',
                 'mediumaquamarine','yellow2','red2',
                 'orangered','darkgreen','brown',
                 'purple1','gray48','gold4','Black')
     if(input$linea=='A'){
       c<-10
     }
     else if(input$linea=='B'){
       c<-11
     }
     else if(input$linea=='Todas'){
       c<-13
     }
     else{
       c<-as.integer(input$linea)
     }
     if(input$cov){
       t=MsC()
     }
     else{
       t=TS()
     }
     m_Serie<-0
     for(i in 1:length(t)){m_Serie[i]<-mean(t[1:i])}
     v_Serie<-0
     for(i in 1:length(t)){v_Serie[i]<-var(t[1:i])}
     par(mfrow=c(1,2))
     plot(m_Serie, type = "l",col=coloresM[c],
          main = 'Media')
     plot(v_Serie, type = "l",col=coloresM[c],
          main = 'Varianza')
   })
  
     output$graf3<-renderPlot({
       if(input$linea!='Todas'){
         titulo= paste('Linea ',input$linea)
       }
       else{
         titulo="Metro"
       }
       
       if(input$cov){
         t=MsC()
       }
       else{
         t=TS()
       } 
      acf(t, main=paste('ACF ',titulo), lag.max = 100)
    
       })
   
   output$graf4<-renderPlot({
     if(input$linea!='Todas'){
       titulo= paste('Linea ',input$linea)
     }
     else{
       titulo="Metro"
     }
     if(input$cov){
       t=MsC()
     }
     else{
       t=TS()
     } 
     pacf(t, main=paste('PACF LINEA ',titulo), lag.max = 100)
   })
    
   output$graf5<-renderPlot({
     tsc=g2()
     tcc=g1()
     ARIMA_2_1_3<-arima(tsc, order = c(2,1,3))
     fore<-predict(ARIMA_2_1_3,n.ahead = 14) 
     i=1
     ts.plot(tsc, (fore$pred)^i,col='red',ylim=c(min(tcc),max(tcc)))
     U <- (fore$pred)^i+(fore$se)^i
     L <- (fore$pred)^i-(fore$se)^i
     xx <- c(time(U), rev(time(U))) 
     yy <- c(L, rev(U))
     polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
     lines(tcc,col='blue')

   })
   
   
    
}




shinyApp(ui = ui, server = server)


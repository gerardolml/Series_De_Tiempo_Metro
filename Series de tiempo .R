library(tidyverse)
library(magrittr)
library(knitr)
library(tseries)
library(lmtest)
library(stats4)
library(strucchange)
library(FinTS)
library(car)
library(wbstats)
library(forecast)
#Lectura de datos 
df<-read.csv('informacion-ingresos-cierre-2012_03-2021-ok.csv')

dev.off()

coloresM<-c('hotpink2','deepskyblue','darkolivegreen3',
            'mediumaquamarine','yellow2','red2',
            'orangered','darkgreen','brown',
            'purple1','gray48','gold4')

# Funcion para seleccionar linea de metro  --------------------

Metro<-function(linea='total',tranformacion=T){
  if(linea!='total'){
  met=df%>%select(FECHA,paste('LINEA',linea,sep = '_'))
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
  tBol=ts(met$Total,start = c(2014,1),end =c(2021,3),frequency = 12)
  
  }
  else{
    met=df
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
  tBol=ts(met$Total,start = c(2014,1),end =c(2021,3),frequency = 12)
  }
  if(tranformacion){
    return(tBol %>% log())
  }
  else{
  return(tBol)
  }
}



# Analisis grafico Por linea del metro  -----------------------------------

G<-par(mfrow=c(3,4))
for(i in 1:12){
  c=i
  if(i==10){i='A'}
  else if(i==11){i='B'}
  t=Metro(i) 
  plot(t,main=paste('LINEA',i,sep = '_'),ylab='ingreso total',
       col=coloresM[c])
}  


# Analisis de la ventas totales  ------------------------------------------

dev.off()
t=Metro()
autoplot(t,main='Ingresos totales del Metro con Covid',ylab='ingreso total')


# Tabla metro sin covid  --------------------------------------------------
#MsC=Metro sin Covid 
MsC<-function(linea='total',tranformacion=T){
  if(linea!='total'){
    met=df%>%select(FECHA,paste('LINEA',linea,sep = '_'))
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
    #p=met %>% 
      #filter(anio %in% c('2014':'2016'),mes=='09')
    #prome=mean(p$Total)
    #met$Total[45]<-prome
    
    tBol=ts(met$Total,start = c(2014,1),end =c(2020,1),frequency = 12)
    
  }
  else{
    met=df
    met$Total=met[,-1:-2] %>% rowSums()
    met$FECHA %<>% as.Date("%d/%m/%Y")
    met %<>% filter(FECHA < "2020-02-01", FECHA > "2014-01-01")
    met%<>% mutate(dia=format(met$FECHA, "%d"),
                   mes=format(met$FECHA, "%m"), 
                   anio = format(met$FECHA, "%Y")) %>% 
      group_by(anio,mes,dia) %>% 
      summarise(Total = sum(Total)) %>% 
      group_by(anio,mes) %>% 
      summarise(Total = sum(Total))
    #p=met %>% 
      #filter(anio %in% c('2014':'2016'),mes=='09')
    #prome=mean(p$Total)
    #met$Total[45]<-prome
    tBol=ts(met$Total,start = c(2014,1),end =c(2020,1),frequency = 12)
  }
  if(tranformacion){
    return(tBol %>% log())
  }
  else{
    return(tBol)
  }
}


# Graficas del Metro sin covid --------------------------------------------

G<-par(mfrow=c(3,4))
for(i in 1:12){
  c=i
  if(i==10){i='A'}
  else if(i==11){i='B'}
  t=MsC(i) 
  plot(t,main=paste('LINEA',i,'Sin Covid',sep = '_'),ylab='ingreso total',
       col=coloresM[c])
}  


# Metro en total sin covid ------------------------------------------------

dev.off()
t=MsC()
autoplot(t,main='Ingresos totales del Metro sin Covid',ylab='ingreso total')

autoplot(decompose(t,type = 'multiplicative'))
# Media de nuestros datos  ------------------------------------------------

m_Serie<-0
for(i in 1:length(t)){m_Serie[i]<-mean(t[1:i])}
plot(m_Serie, type = "l",main='Media de Ingresos totales del Metro sin Covid')



# Varianza de nuestros datos  ---------------------------------------------

v_Serie<-0
for(i in 1:length(t)){v_Serie[i]<-var(t[1:i])}
plot(v_Serie, type = "l",main='Varianza de Ingresos totales del Metro sin Covid')


# ACF Y PACF  -------------------------------------------------------------

ggtsdisplay(diff(t,1),main="Diferencia Típica")

# Estacionalidad  --------------------------------------------------------

pp.test(diff(t), alternative="stationary")

adf.test(diff(t))

autoplot(decompose(diff(t)))

ggseasonplot(t,polar = TRUE,main='Análisis de Estacionalidad')

ggseasonplot(t,year.labels = TRUE,year.labels.left = TRUE)+
  ylab("Ingresos")+
  ggtitle("Ingresos totales del Metro sin Covid")+xlab("Meses")
# Modelos  ----------------------------------------------------------------

#Proponemos los siguientes modelos 

ARIMA_2_1_3<-arima(t, order = c(2,1,3))
ARIMA_2_1_3

SARIMA_1_1_1_12_0_12 <- arima(t, order = c(1,1,1),
                              seasonal = list(order = c(1,0,1), period = 12))
SARIMA_1_1_1_12_0_12 

SARIMA_2_1_3_12_0_12 <- arima(t,
                              order = c(2,1,3), 
                              seasonal = list(order = c(1,0,1), period = 12),
                              method=("ML"))
SARIMA_2_1_3_12_0_12 


#Su penultimo coeficiente no es significativo 
coeftest(ARIMA_2_1_3) 


#Solo el primer coeficiente no es significativo
coeftest(SARIMA_1_1_1_12_0_12)


#Todos son significativos
coeftest(SARIMA_2_1_3_12_0_12)




# Supuestos  -------------------------------------------------


resid<-SARIMA_2_1_3_12_0_12 $residuals ## Obtenemos los residuales

checkresiduals(SARIMA_2_1_3_12_0_12)


# EST Res ----------------------------------------------------------------

pp.test(resid, alternative="stationary")
adf.test(resid)
# Ljnug-Box TEST ----------------------------------------------------------

lb_pv_resid<-NULL
for(i in 1:74){lb_pv_resid[i]<-as.numeric(Box.test(resid, type = 'Ljung-Box', 
                                                   lag = i)$p.value)}
lb_pv_resid
Box.test(resid, type = 'Ljung-Box')
# Prediccion  -------------------------------------------------------------


fore<-predict(SARIMA_2_1_3_12_0_12 ,n.ahead = 14) 
dev.off()
ts.plot(exp(t), exp(fore$pred),col='red',
        ylim=c(min(Metro(tranformacion = F)),
               max(Metro(tranformacion = F))),
        main="Comparación de pronostico ")
U <- exp(fore$pred+fore$se)
L <- exp(fore$pred-fore$se)
xx <- c(time(U), rev(time(U))) 
yy <- c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(Metro(tranformacion = F),col='blue')


Comparacion=df
Comparacion$Total=Comparacion[,-1:-2] %>% rowSums()
Comparacion$FECHA %<>% as.Date("%d/%m/%Y")
Comparacion %<>% filter(FECHA >"2020-02-01")
Comparacion%<>%  mutate(dia=format(Comparacion$FECHA, "%d"),
                        mes=format(Comparacion$FECHA, "%m"), 
                        anio = format(Comparacion$FECHA, "%Y")) %>% 
  group_by(anio,mes,dia) %>% 
  summarise(Total = sum(Total)) %>% 
  group_by(anio,mes) %>% 
  summarise(Total = sum(Total))

Comparacion$Prediccion= exp(fore$pred)
Comparacion$Banda_inferior=L
Comparacion$Banda_superior=rev(U)

Comparacion %>% head() 
t




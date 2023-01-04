###------------------------ PRESENTACIÓN 3, PARTE3 ------------------------ ####
# Preparación y carga de datos:
rm(list=ls())
library(readxl)
dat <- read_excel("PE&LOES pres3_revisado.xlsx")
attach(dat)

###------------------------ Transformación de datos ----------------------- ####
library(dplyr)
dat_1 <- dat %>%
  summarise(
    TIIE.d=diff((TIIE_day)),
    LIBOR.d=diff((LIBOR_day)),
    USDMXN.d=diff((USDMXN)),
    USDEUR.d=diff((USDEUR)),
    USDSEK.d=diff((USDSEK)),
    USDCNY.d=diff((USDCNY)),
    gas_el_paso_index.d=diff((gas_el_paso_index)),
    s_oro.d=diff((s_oro)),
    s_plata.d=diff((s_plata)),
    s_zinc.d=diff((s_zinc)),
    s_plomo.d=diff((s_plomo)),
    s_cobre.d=diff((s_cobre)),
    ing.d=diff((ing)),
    costo_ing.d=(diff(costo_ing)),
    EBITDA.d=diff((EBITDA)),
    ing_oro.d=diff((ing_oro)),
    ing_plata.d=diff((ing_plata)),
    ing_zinc.d=diff((ing_zinc)),
    ing_plomo.d=diff((ing_plomo)),
    ing_cobre.d=diff((ing_cobre)),
    DXY_index.d=diff((DXY_index)),
    VIX_index.d=diff((VIX_index)),
    USAGDP_index.d=diff((USAGDP_index)),
    MEXGDP_index.d=diff((MEXGDP_index)),
    EURGDP_index.d=diff((EURGDP_index)),
    CHNGDP_index.d=diff((CHNGDP_index)),
    USACPI_index.d=diff((USACPI_index)),
    MEXCPI_index.d=diff((MEXCPI_index)),
    EURCPI_index.d=diff((EURCPI_index)),
    CHNCPI_index.d=diff((CHNCPI_index)),
    USA3MB.d=diff((USA3MB)),
    MEX3MB.d=diff((MEX3MB)),
    EUR3MB.d=diff((EUR3MB)),
    CHN3MB.d=diff((CHN3MB)),
    MAYA_CRUDE_US.d=diff((MAYA_CRUDE_US)))

###------------------------ Modelos econometricos: Oro -------------------- ####
#################################################################### Niveles.
m1ing_oro_NIVELES <- lm(ing_oro~
                  s_oro+
                  lag(ing_oro, degree=1))
summary(m1ing_oro_NIVELES)
dwtest(m1ing_oro_NIVELES, alternative = "two.sided")
#DW>r^2

u_oro <- resid(m1ing_oro_NIVELES)
library(tseries)
adf.test(u_oro)
pp.test(u_oro)

y0_1 <- (ing_oro)
x0_1 <- (s_oro)
library(urca)
jotest=ca.jo(data.frame(y0_1,x0_1), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
#LP, no tiene relación, pero si en el CP.

#################################################################### D(1).
# ingoro=so+(ingorot-1=sot-1+) Ecuación en diferencias.
m1ing_oro <- lm(ing_oro.d~
                  s_oro.d+
                  lag(ing_oro.d, degree=1),
                data=dat_1)
summary(m1ing_oro)
dwtest(m1ing_oro, alternative = "two.sided")


# grangertest is a generic function for performing a test for Granger causality.
library (lmtest)
grangertest(ing_oro.d~s_oro.d, order=1, data=dat_1)
# El lag no causa el ing oro del sig trim.

# El lag en la regresión es un AR (Autoregresión), la variacion del oro
# de este trim, se ve impactado por la variación del trim anterior.
#   ¿Que pasa si produjiste mucho, quizas el tim siguiente produces.
# Lag, genera el impacto de la producción* Nota: Causalidad de Granger, 
# que el precio de una variable est causando a otra.


# Cuando se debilita el dolar, bajan los ingresos. Revisar a detalle.

###------------------------ Modelos econometricos: Plata------------------- ####
#################################################################### Niveles.
m1ing_plata_NIVELES <- lm(ing_plata~
                          s_plata)

summary(m1ing_plata_NIVELES)
dwtest(m1ing_plata_NIVELES, alternative = "two.sided")
#FW>r^2
u_plata <- resid(m1ing_plata_NIVELES)
library(tseries)
adf.test(u_plata)
pp.test(u_plata)
# Cointegración PP.

y0_2 <- (ing_plata)
x0_2 <- (s_plata)
library(urca)
jotest=ca.jo(data.frame(y0_2,x0_2), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
# r<=1
# Mas de dos, vectores lienalmente independientes, se echaza porque solo son dos
# r=0
# Al menos un vector linealmente independiente.

#################################################################### D(1).
m2ing_plata <- lm(ing_plata.d~
                  s_plata.d,
                      data=dat_1)
summary(m2ing_plata)

###------------------------ Modelos econometricos: EBITDA ----------------- ####
#################################################################### Niveles.
m1ing_EBITDA_NIVELES <- lm(EBITDA~
                            gas_el_paso_index)
summary(m1ing_EBITDA_NIVELES)
dwtest(m1ing_EBITDA_NIVELES, alternative = "two.sided")
#DW>r^2

u_EBITDA <- resid(m1ing_EBITDA_NIVELES)
library(tseries)
adf.test(u_EBITDA)
pp.test(u_EBITDA)

y0_3 <- (EBITDA)
x0_3 <- (gas_el_paso_index)
library(urca)
jotest=ca.jo(data.frame(y0_3,x0_3), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
# Al menos un vector linealmente independiente, 

#################################################################### D(1).
m3EBITDA <- lm(EBITDA.d~
               gas_el_paso_index.d,
                      data=dat_1)
summary(m3EBITDA)
# Diessel. WTI referencia. Moctezuma.
# Precio de la energia electrica. CFE/CENACE
# Terminos practicos. 

###------------------------ Pruebas diagnostico: Oro ---------------------- ####
#   Pruebas                       Modelo +     p-values

#1. Prueba de media cero:         Cumple.   
#                                 p-v:1 t-test
#2. Pureba de normalidad:         Cumple.   
#                                 p-v:0.08717 Shapiro-wilk
#3. Prueba de heterocedasticidad: El modelo es homocedástico.
#                                 p-v:0.04385 Breusch-Pagan
#4. Prueba de correlación serial: No se detecta correlación serial en los errores.
#                                 p-v:0.397  Durbin-Watson
#5. Prueba RESET:                 El modelo está correctamente especificado.
#                                 p-v:0.1838

# 1. Prueba de media cero:                                                  ####
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m1ing_oro$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

# 2. Prueba de normalidad:                                                  ####
# H0 : Los errores son normales
# H1 : Los errores no son normales
shapiro.test(m1ing_oro$residuals)
library(nortest)
ad.test(m1ing_oro$residuals)
# p-value > 0.05, no se rechaza HO: en este caso los errores son normales.
qqnorm(m1ing_oro$residuals)
qqline(m1ing_oro$residuals, col = "red")

# 3. Prueba de heterocedasticidad:                                          ####
# H0 : Los errores son homocedásticos
# H1 : Los errores no son homocedásticos
library(lmtest)
bptest(m1ing_oro)
# p-value > 0.05, no se rechaza HO: en este caso los errores cumplen
# con supuestos de homocedásticidad.

# 4. Prueba de correlación serial:                                          ####

# 4.1. Prueba Durbin-Watson
# H0 : Los errores no tienen correlación serial sucesiva
# H1 : Los errores tienen correlación serial sucesiva
dwtest(m1ing_oro, alternative = "two.sided")
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva.

# 4.2. Prueba Breush-Godfrey
# H0 : Los errores no tienen correlación serial sucesiva hasta orden r
# H1 : Los errores no tienen correlación serial sucesiva hasta orden r
bgtest(m1ing_oro, order = 3)
bgtest(m1ing_oro, order = 6)
bgtest(m1ing_oro, order = 9)
bgtest(m1ing_oro, order = 12)
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva de orden r.

# 4. Prueba RESET:                                                          ####
# H0 : El modelo está correctamente especificado
# H1 : El modelo no está correctamente especificado
library(lmtest)
library(sandwich)
resettest(m1ing_oro, power = 2:3, vcov = vcovHAC)
# p-value > 0.05, no se rechaza HO: en este caso el modelo esta correctamente
# especificado.


###------------------------ Pruebas diagnostico: Plata -------------------- ####
#   Pruebas                       Modelo +     p-values

#1. Prueba de media cero:         Cumple.   
#                                 p-v:1 t-test
#2. Pureba de normalidad:         Cumple.   
#                                 p-v:0.07475 Shapiro-wilk
#3. Prueba de heterocedasticidad: El modelo es homocedástico.
#                                 p-v:0.07366 Breusch-Pagan
#4. Prueba de correlación serial: No se detecta correlación serial en los errores.
#                                 p-v:0.8887  Durbin-Watson
#5. Prueba RESET:                 El modelo está correctamente especificado.
#                                 p-v:0.4168

# 1. Prueba de media cero:                                                  ####
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m2ing_plata$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

# 2. Prueba de normalidad:                                                  ####
# H0 : Los errores son normales
# H1 : Los errores no son normales
shapiro.test(m2ing_plata$residuals)
library(nortest)
ad.test(m2ing_plata$residuals)
# p-value > 0.05, no se rechaza HO: en este caso los errores son normales.
qqnorm(m2ing_plata$residuals)
qqline(m2ing_plata$residuals, col = "red")

# 3. Prueba de heterocedasticidad:                                          ####
# H0 : Los errores son homocedásticos
# H1 : Los errores no son homocedásticos
library(lmtest)
bptest(m2ing_plata)
# p-value > 0.05, no se rechaza HO: en este caso los errores cumplen
# con supuestos de homocedásticidad.

# 4. Prueba de correlación serial:                                          ####

# 4.1. Prueba Durbin-Watson
# H0 : Los errores no tienen correlación serial sucesiva
# H1 : Los errores tienen correlación serial sucesiva
dwtest(m2ing_plata, alternative = "two.sided")
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva.

# 4.2. Prueba Breush-Godfrey
# H0 : Los errores no tienen correlación serial sucesiva hasta orden r
# H1 : Los errores no tienen correlación serial sucesiva hasta orden r
bgtest(m2ing_plata, order = 3)
bgtest(m2ing_plata, order = 6)
bgtest(m2ing_plata, order = 9)
bgtest(m2ing_plata, order = 12)
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva de orden r.

# 4. Prueba RESET:                                                          ####
# H0 : El modelo está correctamente especificado
# H1 : El modelo no está correctamente especificado
library(lmtest)
library(sandwich)
resettest(m2ing_plata, power = 2:3, vcov = vcovHAC)
# p-value > 0.05, no se rechaza HO: en este caso el modelo esta correctamente
# especificado.
###------------------------ Pruebas diagnostico: EBITDA ------------------- ####
#   Pruebas                       Modelo +     p-values

#1. Prueba de media cero:         Cumple.   
#                                 p-v:1 t-test
#2. Pureba de normalidad:         Cumple.   
#                                 p-v:0.1715 Shapiro-wilk
#3. Prueba  de heterocedasticidad: El modelo es homocedástico.
#                                 p-v:0.6144 Breusch-Pagan
#4. Prueba de correlación serial: No se detecta correlación serial en los errores.
#                                 p-v:0.3586  Durbin-Watson
#5. Prueba RESET:                 El modelo está correctamente especificado.
#                                 p-v:0.07206

# 1. Prueba de media cero:                                                  ####
# H0 : E[u] = 0
# H1 : E[u] != 0
t.test(m3EBITDA$residuals,mu=0)
# p-value > 0.05, no se rechaza HO: en este caso los errores tienen media cero.

# 2. Prueba de normalidad:                                                  ####
# H0 : Los errores son normales
# H1 : Los errores no son normales
shapiro.test(m3EBITDA$residuals)
library(nortest)
ad.test(m3EBITDA$residuals)
# p-value > 0.05, no se rechaza HO: en este caso los errores son normales.
qqnorm(m3EBITDA$residuals)
qqline(m3EBITDA$residuals, col = "red")

# 3. Prueba de heterocedasticidad:                                          ####
# H0 : Los errores son homocedásticos
# H1 : Los errores no son homocedásticos
library(lmtest)
bptest(m3EBITDA)
# p-value > 0.05, no se rechaza HO: en este caso los errores cumplen
# con supuestos de homocedásticidad.

# 4. Prueba de correlación serial:                                          ####

# 4.1. Prueba Durbin-Watson
# H0 : Los errores no tienen correlación serial sucesiva
# H1 : Los errores tienen correlación serial sucesiva
dwtest(m3EBITDA, alternative = "two.sided")
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva.

# 4.2. Prueba Breush-Godfrey
# H0 : Los errores no tienen correlación serial sucesiva hasta orden r
# H1 : Los errores no tienen correlación serial sucesiva hasta orden r
bgtest(m3EBITDA, order = 3)
bgtest(m3EBITDA, order = 6)
bgtest(m3EBITDA, order = 9)
bgtest(m3EBITDA, order = 12)
# p-value > 0.05, no se rechaza HO: en este caso existe evidencia de
# correlación serial sucesiva de orden r.

# 4. Prueba RESET:                                                          ####
# H0 : El modelo está correctamente especificado
# H1 : El modelo no está correctamente especificado
library(lmtest)
library(sandwich)
resettest(m3EBITDA, power = 2:3, vcov = vcovHAC)
# p-value > 0.05, no se rechaza HO: en este caso el modelo esta correctamente
# especificado.





###------------------------ Notas ----------------------------------------- ####

library(tseries)
adf.test(spx.p)
adf.test(spx.r)
pp.test(spx.r)	

# summarise genera bases de datos al transformar datos.
#Problema de solo correr dos variables
# diff log=rendimientos continuos, tasa de crecimiento continuo
# proceso de capializacon, se suele usar para los modelos financieros.
# Cambos porcentuales son aproximaciones discretas porque estas viendo 
# cuanto cambia de un periodo a otro de manera continua
# Justificado que use trimestral





rm(list=ls())

#### libraries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(janitor)
library(modelsummary)
library(fixest)
library(sandwich)
library(lmtest)
library(readxl)
library(GGally)
library(fpp3)
library(xts)
library(forecast)
library(tseries)
library(DT)
library(gridExtra)
library(vars)
library(lpirfs)

#### set WD in your data folder
setwd("C:\\Users\\Fraba\\Downloads")
getwd()

gdp_energy <- read_excel("P_Data_Extract_From_World_Development_Indicators.xlsx")[,-1]

names(gdp_energy)

###rinomino le variabili e filtro###
gdp_energy = gdp_energy%>%
  rename(
    Country = "Country Code",
    PGDP= "Adjusted net national income per capita (constant 2015 US$) [NY.ADJ.NNTY.PC.KD]",
    PEC = "Energy use (kg of oil equivalent per capita) [EG.USE.PCAP.KG.OE]"
  )%>%
  filter(Country %in% c("ITA",
                    "DEU",
                    "SWE",
                    "BEL",
                    "NLD"
  )
  )

str(gdp_energy)
names(gdp_energy)

#### controlla la percentuale di NA per colonna
colMeans(is.na(gdp_energy))

###ora creiamo il dataset per i primi grafici
names(gdp_energy)

Graph_PGDP_PEC =
  gdp_energy%>%
  filter(Year<=2015)%>%
  dplyr::select("Year",
           "Country",
           "PGDP",
           "PEC"
  )


str(Graph_PGDP_PEC)
summary(Graph_PGDP_PEC)



###ora creiamo il dataset per le analisi econometriche
PGDP_PEC =
  gdp_energy%>%
  filter(Year<=2015)%>%
  dplyr::select("Year",
         "Country",
         "PGDP",
         "PEC"
  )%>%
  mutate(PGDP=log(PGDP),
         PEC=log(PEC))


str(PGDP_PEC)
summary(PGDP_PEC)

###GRAPHIC ANALISY

pgdp_plot=
  Graph_PGDP_PEC%>%
  ggplot()+
  aes(x=Year, y=PGDP, color = Country)+
  geom_line(size = 1)+
  theme_bw()+
  labs(title="Trend GDP", x="Year", y="PGDP PerCapita ($)")
pgdp_plot


pec_plot=
  Graph_PGDP_PEC%>%
  ggplot()+
  aes(x=Year, y=PEC, color = Country)+
  geom_line(size = 1)+
  theme_bw()+
  labs(title="Trend PEC", x="Year", y="Energy Consumption PerCapita(kg of oil)")
pec_plot


###grafici con i logaritmi

log_pgdp_plot=
  PGDP_PEC%>%
  ggplot()+
  aes(x=Year, y=PGDP, color = Country)+
  geom_line(size = 1)+
  theme_bw()+
  labs(x="Year", y="GDP PerCapita")
log_pgdp_plot


log_pec_plot=
  PGDP_PEC%>%
  ggplot()+
  aes(x=Year, y=PEC, color = Country)+
  geom_line(size = 1)+
  theme_bw()+
  labs(x="Year", y="Energy Consumption PerCapita")
log_pec_plot

###verificare la stazionarietà tramite l' autocorrelazione, paese per paese

PGDP_PEC_ITA<-PGDP_PEC%>%
  filter(Country=="ITA")
PGDP_PEC_DEU<-PGDP_PEC%>%
  filter(Country=="DEU")
PGDP_PEC_BEL<-PGDP_PEC%>%
  filter(Country=="BEL")
PGDP_PEC_NLD<-PGDP_PEC%>%
  filter(Country=="NLD")
PGDP_PEC_SWE<-PGDP_PEC%>%
  filter(Country=="SWE")

par(mfrow=c(2,3))

PGDP_PEC_ITA$PGDP%>%
  acf(lag.max = 44, main = "ACF of the PGDP_ITA - Logs")
PGDP_PEC_DEU$PGDP%>%
  acf(lag.max = 44, main = "ACF of the PGDP_DEU - Logs")
PGDP_PEC_BEL$PGDP%>%
  acf(lag.max = 44, main = "ACF of the PGDP_BEL - Logs")
PGDP_PEC_NLD$PGDP%>%
  acf(lag.max = 44, main = "ACF of the PGDP_NLD - Logs")
PGDP_PEC_SWE$PGDP%>%
  acf(lag.max = 44, main = "ACF of the PGDP_SWE - Logs")
#############################################################

PGDP_PEC_ITA<-PGDP_PEC%>%
  filter(Country=="ITA")
PGDP_PEC_DEU<-PGDP_PEC%>%
  filter(Country=="DEU")
PGDP_PEC_BEL<-PGDP_PEC%>%
  filter(Country=="BEL")
PGDP_PEC_NLD<-PGDP_PEC%>%
  filter(Country=="NLD")
PGDP_PEC_SWE<-PGDP_PEC%>%
  filter(Country=="SWE")

par(mfrow=c(2,3))

PGDP_PEC_ITA$PEC%>%
  acf(lag.max = 44, main = "ACF of the PEC_ITA - Logs")
PGDP_PEC_DEU$PEC%>%
  acf(lag.max = 44, main = "ACF of the PEC_DEU - Logs")
PGDP_PEC_BEL$PEC%>%
  acf(lag.max = 44, main = "ACF of the PEC_BEL - Logs")
PGDP_PEC_NLD$PEC%>%
  acf(lag.max = 44, main = "ACF of the PEC_NLD - Logs")
PGDP_PEC_SWE$PEC%>%
  acf(lag.max = 44, main = "ACF of the PEC_SWE - Logs")
################################################################################

###troviamo il lag migliore tramite i modelli AIC e BIC (per arf)

##creo un nuovo data set, contenenti le sole variabili del paese ITA, 
#inserisco poi 4 nuove variabili sia per PGDP e PEC contenenti i 4 ritardi
PGDP_PEC_ITA=
  PGDP_PEC_ITA%>%
  mutate(PGDP_L1=lag(PGDP_PEC_ITA$PGDP,1),
         PGDP_L2=lag(PGDP_PEC_ITA$PGDP,2),
         PGDP_L3=lag(PGDP_PEC_ITA$PGDP,3),
         PGDP_L4=lag(PGDP_PEC_ITA$PGDP,4),
         PEC_L1=lag(PGDP_PEC_ITA$PEC,1),
         PEC_L2=lag(PGDP_PEC_ITA$PEC,2),
         PEC_L3=lag(PGDP_PEC_ITA$PEC,3),
         PEC_L4=lag(PGDP_PEC_ITA$PEC,4))

#faccio l' autoregressione della variamile PGDP tramite la funzione lm (una per ogni ritardo creato)
PGDP_AR1_ITA<-lm(data = PGDP_PEC_ITA,
              formula = PGDP~PGDP_L1)

PGDP_AR2_ITA<-lm(data = PGDP_PEC_ITA,
              formula = PGDP~PGDP_L1+PGDP_L2)

PGDP_AR3_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3)

PGDP_AR4_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3+PGDP_L4)

###################

#faccio l' autoregressione della variamile PEC tramite la funzione lm (una per ogni ritardo creato)
PEC_AR1_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PEC~PEC_L1)

PEC_AR2_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PEC~PEC_L1+PEC_L2)

PEC_AR3_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PEC~PEC_L1+PEC_L2+PEC_L3)

PEC_AR4_ITA<-lm(data = PGDP_PEC_ITA,
                 formula = PEC~PEC_L1+PEC_L2+PEC_L3+PEC_L4)

###################
###################
###################

##creo un nuovo data set, contenenti le sole variabili del paese DEu, 
#inserisco poi 4 nuove variabili sia per PGDP e PEC contenenti i 4 ritardi
PGDP_PEC_DEU=
  PGDP_PEC_DEU%>%
  mutate(PGDP_L1=lag(PGDP_PEC_DEU$PGDP,1),
         PGDP_L2=lag(PGDP_PEC_DEU$PGDP,2),
         PGDP_L3=lag(PGDP_PEC_DEU$PGDP,3),
         PGDP_L4=lag(PGDP_PEC_DEU$PGDP,4),
         PEC_L1=lag(PGDP_PEC_DEU$PEC,1),
         PEC_L2=lag(PGDP_PEC_DEU$PEC,2),
         PEC_L3=lag(PGDP_PEC_DEU$PEC,3),
         PEC_L4=lag(PGDP_PEC_DEU$PEC,4))

#faccio l' autoregressione della variamile PGDP tramite la funzione lm (una per ogni ritardo creato)
PGDP_AR1_DEU<-lm(data = PGDP_PEC_DEU,
                 formula = PGDP~PGDP_L1)

PGDP_AR2_DEU<-lm(data = PGDP_PEC_DEU,
                 formula = PGDP~PGDP_L1+PGDP_L2)

PGDP_AR3_DEU<-lm(data = PGDP_PEC_DEU,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3)

PGDP_AR4_DEU<-lm(data = PGDP_PEC_DEU,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3+PGDP_L4)


######################

#faccio l' autoregressione della variamile PEC tramite la funzione lm (una per ogni ritardo creato)
PEC_AR1_DEU<-lm(data = PGDP_PEC_DEU,
                formula = PEC~PEC_L1)

PEC_AR2_DEU<-lm(data = PGDP_PEC_DEU,
                formula = PEC~PEC_L1+PEC_L2)

PEC_AR3_DEU<-lm(data = PGDP_PEC_DEU,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3)

PEC_AR4_DEU<-lm(data = PGDP_PEC_DEU,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3+PEC_L4)

###################
###################
###################

##creo un nuovo data set, contenenti le sole variabili del paese BEL, 
#inserisco poi 4 nuove variabili sia per PGDP e PEC contenenti i 4 ritardi
PGDP_PEC_BEL=
  PGDP_PEC_BEL%>%
  mutate(PGDP_L1=lag(PGDP_PEC_BEL$PGDP,1),
         PGDP_L2=lag(PGDP_PEC_BEL$PGDP,2),
         PGDP_L3=lag(PGDP_PEC_BEL$PGDP,3),
         PGDP_L4=lag(PGDP_PEC_BEL$PGDP,4),
         PEC_L1=lag(PGDP_PEC_BEL$PEC,1),
         PEC_L2=lag(PGDP_PEC_BEL$PEC,2),
         PEC_L3=lag(PGDP_PEC_BEL$PEC,3),
         PEC_L4=lag(PGDP_PEC_BEL$PEC,4))

#faccio l' autoregressione della variamile PGDP tramite la funzione lm (una per ogni ritardo creato)
PGDP_AR1_BEL<-lm(data = PGDP_PEC_BEL,
                 formula = PGDP~PGDP_L1)

PGDP_AR2_BEL<-lm(data = PGDP_PEC_BEL,
                 formula = PGDP~PGDP_L1+PGDP_L2)

PGDP_AR3_BEL<-lm(data = PGDP_PEC_BEL,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3)

PGDP_AR4_BEL<-lm(data = PGDP_PEC_BEL,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3+PGDP_L4)

######################

#faccio l' autoregressione della variamile PEC tramite la funzione lm (una per ogni ritardo creato)
PEC_AR1_BEL<-lm(data = PGDP_PEC_BEL,
                formula = PEC~PEC_L1)

PEC_AR2_BEL<-lm(data = PGDP_PEC_BEL,
                formula = PEC~PEC_L1+PEC_L2)

PEC_AR3_BEL<-lm(data = PGDP_PEC_BEL,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3)

PEC_AR4_BEL<-lm(data = PGDP_PEC_BEL,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3+PEC_L4)

###################
###################
###################

##creo un nuovo data set, contenenti le sole variabili del paese NLD, 
#inserisco poi 4 nuove variabili sia per PGDP e PEC contenenti i 4 ritardi
PGDP_PEC_NLD=
  PGDP_PEC_NLD%>%
  mutate(PGDP_L1=lag(PGDP_PEC_NLD$PGDP,1),
         PGDP_L2=lag(PGDP_PEC_NLD$PGDP,2),
         PGDP_L3=lag(PGDP_PEC_NLD$PGDP,3),
         PGDP_L4=lag(PGDP_PEC_NLD$PGDP,4),
         PEC_L1=lag(PGDP_PEC_NLD$PEC,1),
         PEC_L2=lag(PGDP_PEC_NLD$PEC,2),
         PEC_L3=lag(PGDP_PEC_NLD$PEC,3),
         PEC_L4=lag(PGDP_PEC_NLD$PEC,4))

#faccio l' autoregressione della variamile PGDP tramite la funzione lm (una per ogni ritardo creato)
PGDP_AR1_NLD<-lm(data = PGDP_PEC_NLD,
                 formula = PGDP~PGDP_L1)

PGDP_AR2_NLD<-lm(data = PGDP_PEC_NLD,
                 formula = PGDP~PGDP_L1+PGDP_L2)

PGDP_AR3_NLD<-lm(data = PGDP_PEC_NLD,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3)

PGDP_AR4_NLD<-lm(data = PGDP_PEC_NLD,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3+PGDP_L4)

######################

#faccio l' autoregressione della variamile PEC tramite la funzione lm (una per ogni ritardo creato)
PEC_AR1_NLD<-lm(data = PGDP_PEC_NLD,
                formula = PEC~PEC_L1)

PEC_AR2_NLD<-lm(data = PGDP_PEC_NLD,
                formula = PEC~PEC_L1+PEC_L2)

PEC_AR3_NLD<-lm(data = PGDP_PEC_NLD,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3)

PEC_AR4_NLD<-lm(data = PGDP_PEC_NLD,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3+PEC_L4)

###################
###################
###################

##creo un nuovo data set, contenenti le sole variabili del paese SWE, 
#inserisco poi 4 nuove variabili sia per PGDP e PEC contenenti i 4 ritardi
PGDP_PEC_SWE=
  PGDP_PEC_SWE%>%
  mutate(PGDP_L1=lag(PGDP_PEC_SWE$PGDP,1),
         PGDP_L2=lag(PGDP_PEC_SWE$PGDP,2),
         PGDP_L3=lag(PGDP_PEC_SWE$PGDP,3),
         PGDP_L4=lag(PGDP_PEC_SWE$PGDP,4),
         PEC_L1=lag(PGDP_PEC_SWE$PEC,1),
         PEC_L2=lag(PGDP_PEC_SWE$PEC,2),
         PEC_L3=lag(PGDP_PEC_SWE$PEC,3),
         PEC_L4=lag(PGDP_PEC_SWE$PEC,4))

#faccio l' autoregressione della variamile PGDP tramite la funzione lm (una per ogni ritardo creato)
PGDP_AR1_SWE<-lm(data = PGDP_PEC_SWE,
                 formula = PGDP~PGDP_L1)

PGDP_AR2_SWE<-lm(data = PGDP_PEC_SWE,
                 formula = PGDP~PGDP_L1+PGDP_L2)

PGDP_AR3_SWE<-lm(data = PGDP_PEC_SWE,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3)

PGDP_AR4_SWE<-lm(data = PGDP_PEC_SWE,
                 formula = PGDP~PGDP_L1+PGDP_L2+PGDP_L3+PGDP_L4)

######################

#faccio l' autoregressione della variamile PEC tramite la funzione lm (una per ogni ritardo creato)
PEC_AR1_SWE<-lm(data = PGDP_PEC_SWE,
                formula = PEC~PEC_L1)

PEC_AR2_SWE<-lm(data = PGDP_PEC_SWE,
                formula = PEC~PEC_L1+PEC_L2)

PEC_AR3_SWE<-lm(data = PGDP_PEC_SWE,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3)

PEC_AR4_SWE<-lm(data = PGDP_PEC_SWE,
                formula = PEC~PEC_L1+PEC_L2+PEC_L3+PEC_L4)
##################################################################################################
##################################################################################################
##creo 4 liste, tante quanti sono i ritardi, nella prima inseriremo i primi ritardi di tutti e 5 i paesi e di entrambe le variabili.
model_AIC1<-list(PGDP_AR1_ITA, PEC_AR1_ITA, PGDP_AR1_DEU, PEC_AR1_DEU, PGDP_AR1_BEL, PEC_AR1_BEL, PGDP_AR1_NLD, PEC_AR1_NLD, PGDP_AR1_SWE, PEC_AR1_SWE)
model_AIC2<-list(PGDP_AR2_ITA, PEC_AR2_ITA, PGDP_AR2_DEU, PEC_AR2_DEU, PGDP_AR2_BEL, PEC_AR2_BEL, PGDP_AR2_NLD, PEC_AR2_NLD, PGDP_AR2_SWE, PEC_AR2_SWE)
model_AIC3<-list(PGDP_AR3_ITA, PEC_AR3_ITA, PGDP_AR3_DEU, PEC_AR3_DEU, PGDP_AR3_BEL, PEC_AR3_BEL, PGDP_AR3_NLD, PEC_AR3_NLD, PGDP_AR3_SWE, PEC_AR3_SWE)
model_AIC4<-list(PGDP_AR4_ITA, PEC_AR4_ITA, PGDP_AR4_DEU, PEC_AR4_DEU, PGDP_AR4_BEL, PEC_AR4_BEL, PGDP_AR4_NLD, PEC_AR4_NLD, PGDP_AR4_SWE, PEC_AR4_SWE)

#attraverso la funzione sapply creo dei vettori in cui viene applicata la funzione AIC
aic_1 <- sapply(model_AIC1, AIC)
bic_1 <- sapply(model_AIC1, BIC)
aic_2 <- sapply(model_AIC2, AIC)
bic_2 <- sapply(model_AIC2, BIC)
aic_3 <- sapply(model_AIC3, AIC)
bic_3 <- sapply(model_AIC3, BIC)
aic_4 <- sapply(model_AIC4, AIC)
bic_4 <- sapply(model_AIC4, BIC)

#creo un dataframe per la costruzione di una tabella visiva
AR_table_AIC_BIC<-data.frame(
  Country=c("ITA","", "DEU","", "BEL","", "NLD", "", "SWE",""),
  Variable = c("PGDP","PEC", "PGDP","PEC", "PGDP","PEC", "PGDP","PEC", "PGDP","PEC"),
  AIC_1 = round(aic_1,2),
  AIC_2 = round(aic_2,2),
  AIC_3 = round(aic_3,2),
  AIC_4 = round(aic_4,2),
  BIC_1 = round(bic_1,2),
  BIC_2 = round(bic_2,2),
  BIC_3 = round(bic_3,2),
  BIC_4 = round(bic_4,2))%>%
  rename("AIC (1)" = AIC_1,
         "AIC (2)" = AIC_2,
         "AIC (3)" = AIC_3,
         "AIC (4)" = AIC_4,
         "BIC (1)" = BIC_1,
         "BIC (2)" = BIC_2,
         "BIC (3)" = BIC_3,
         "BIC (4)" = BIC_4,)
#creo un vettore contenente le variabili: "AIC_1", "AIC_2".... "BIC_1"....
AIC_names_AR <- names(AR_table_AIC_BIC)[3:6] 
#creo una nuova colonna 
AR_table_AIC_BIC$Optimal_lag<- AIC_names_AR[apply(AR_table_AIC_BIC[, AIC_names_AR], 1, which.min)]
AR_table_AIC_BIC<-AR_table_AIC_BIC%>%
  rename("Optimal lag" = "Optimal_lag")
#creo la tabella visiva tramite la funzione "datatable" del pacchetto DT
datatable(AR_table_AIC_BIC, 
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = FALSE,
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          class = "compact",
          colnames = c("Country", "Variable", "AIC (1)", "AIC (2)","AIC (3)","AIC (4)","BIC (1)","BIC (2)","BIC (3)","BIC (4)","Optimal lag")) %>%
  formatStyle(0:11,  # Indici delle prime due colonne (Country e Variable)
              width = "50px"  # Larghezza desiderata
  )%>%
  formatStyle(
    names(AR_table_AIC_BIC),  # Nomi di tutte le colonne
    textAlign = "center"  # Allineamento al centro
  )%>%
  formatStyle(
    1:length(AR_table_AIC_BIC),  # Indici di tutte le colonne
    `border-left` = "1px solid #BBB"  # Linea separatrice sinistra delle colonne
  )
  

######################
##############creiamo la "prima differenza" dei log di PGDP t e t-1 <- (Grow rate) e la riportiamo in un grafico (per ogni nazione)
######################
PGDP_PEC_ITA$gr_PGDP =  c(NA,diff(PGDP_PEC_ITA$PGDP))
PGDP_PEC_ITA%>%
  ggplot()+
  aes(x=Year, y=gr_PGDP)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'gdp_ita - Growth Rate',
       x = 'Date')

PGDP_PEC_DEU$gr_PGDP =  c(NA,diff(PGDP_PEC_DEU$PGDP))
PGDP_PEC_DEU%>%
  ggplot()+
  aes(x=Year, y=gr_PGDP)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'gdp_deu - Growth Rate',
       x = 'Date')

PGDP_PEC_BEL$gr_PGDP =  c(NA,diff(PGDP_PEC_BEL$PGDP))
PGDP_PEC_BEL%>%
  ggplot()+
  aes(x=Year, y=gr_PGDP)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'gdp_bel - Growth Rate',
       x = 'Date')

PGDP_PEC_NLD$gr_PGDP =  c(NA,diff(PGDP_PEC_NLD$PGDP))
PGDP_PEC_NLD%>%
  ggplot()+
  aes(x=Year, y=gr_PGDP)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'gdp_nld - Growth Rate',
       x = 'Date')

PGDP_PEC_SWE$gr_PGDP =  c(NA,diff(PGDP_PEC_SWE$PGDP))
PGDP_PEC_SWE%>%
  ggplot()+
  aes(x=Year, y=gr_PGDP)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'gdp_swe - Growth Rate',
       x = 'Date')

##facciamo l' ADF Test level e first difference con i valori dell AIC piu bassi 
##cha abbiamo trovato nella tabella precedente
test_var1_ita_pgdp = adf.test(PGDP_PEC_ITA$PGDP, k=2)      
test_var2_ita_pgdp = adf.test(PGDP_PEC_ITA$gr_PGDP %>% na.remove(), k=2)

test_var1_deu_pgdp = adf.test(PGDP_PEC_DEU$PGDP, k=1)      
test_var2_deu_pgdp = adf.test(PGDP_PEC_DEU$gr_PGDP %>% na.remove(), k=1)

test_var1_bel_pgdp = adf.test(PGDP_PEC_BEL$PGDP, k=1)      
test_var2_bel_pgdp = adf.test(PGDP_PEC_BEL$gr_PGDP %>% na.remove(), k=1)

test_var1_nld_pgdp = adf.test(PGDP_PEC_NLD$PGDP, k=1)      
test_var2_nld_pgdp = adf.test(PGDP_PEC_NLD$gr_PGDP %>% na.remove(), k=1)

test_var1_swe_pgdp = adf.test(PGDP_PEC_SWE$PGDP, k=1)      
test_var2_swe_pgdp = adf.test(PGDP_PEC_SWE$gr_PGDP %>% na.remove(), k=1)

##estraiamo dal test i vari valori delle Z
z_test_var1_ita_pgdp = (test_var1_ita_pgdp$statistic)
z_test_var2_ita_pgdp = (test_var2_ita_pgdp$statistic)
z_test_var1_deu_pgdp = (test_var1_deu_pgdp$statistic)
z_test_var2_deu_pgdp = (test_var2_deu_pgdp$statistic)
z_test_var1_bel_pgdp = (test_var1_bel_pgdp$statistic)
z_test_var2_bel_pgdp = (test_var2_bel_pgdp$statistic)
z_test_var1_nld_pgdp = (test_var1_nld_pgdp$statistic)
z_test_var2_nld_pgdp = (test_var2_nld_pgdp$statistic)
z_test_var1_swe_pgdp = (test_var1_swe_pgdp$statistic)
z_test_var2_swe_pgdp = (test_var2_swe_pgdp$statistic)

######################
####################creiamo la "prima differenza" dei log di PEC t e t-1 <- (Grow rate) e la riportiamo in un grafico (per ogni nazione)
######################
PGDP_PEC_ITA$gr_PEC =  c(NA,diff(PGDP_PEC_ITA$PEC))
PGDP_PEC_ITA%>%
  ggplot()+
  aes(x=Year, y=gr_PEC)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'pec_ita - Growth Rate',
       x = 'Date')

PGDP_PEC_DEU$gr_PEC =  c(NA,diff(PGDP_PEC_DEU$PEC))
PGDP_PEC_DEU%>%
  ggplot()+
  aes(x=Year, y=gr_PEC)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'pec_deu - Growth Rate',
       x = 'Date')

PGDP_PEC_BEL$gr_PEC =  c(NA,diff(PGDP_PEC_BEL$PEC))
PGDP_PEC_BEL%>%
  ggplot()+
  aes(x=Year, y=gr_PEC)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'pec_bel - Growth Rate',
       x = 'Date')

PGDP_PEC_NLD$gr_PEC =  c(NA,diff(PGDP_PEC_NLD$PEC))
PGDP_PEC_NLD%>%
  ggplot()+
  aes(x=Year, y=gr_PEC)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'pec_nld - Growth Rate',
       x = 'Date')

PGDP_PEC_SWE$gr_PEC =  c(NA,diff(PGDP_PEC_SWE$PEC))
PGDP_PEC_SWE%>%
  ggplot()+
  aes(x=Year, y=gr_PEC)+
  geom_line(size=1)+
  theme_bw() +
  labs(y = 'pec_swe - Growth Rate',
       x = 'Date')

##facciamo l' ADF Test level e first difference con i valori dell AIC piu bassi 
##cha abbiamo trovato nella tabella precedente
test_var1_ita_pec = adf.test(PGDP_PEC_ITA$PEC, k=1)      
test_var2_ita_pec = adf.test(PGDP_PEC_ITA$gr_PEC %>% na.remove(), k=1)

test_var1_deu_pec = adf.test(PGDP_PEC_DEU$PEC, k=1)      
test_var2_deu_pec = adf.test(PGDP_PEC_DEU$gr_PEC %>% na.remove(), k=1)

test_var1_bel_pec = adf.test(PGDP_PEC_BEL$PEC, k=1)      
test_var2_bel_pec = adf.test(PGDP_PEC_BEL$gr_PEC %>% na.remove(), k=1)

test_var1_nld_pec = adf.test(PGDP_PEC_NLD$PEC, k=1)      
test_var2_nld_pec = adf.test(PGDP_PEC_NLD$gr_PEC %>% na.remove(), k=1)

test_var1_swe_pec = adf.test(PGDP_PEC_SWE$PEC, k=1)      
test_var2_swe_pec = adf.test(PGDP_PEC_SWE$gr_PEC %>% na.remove(), k=1)

##estraiamo dal test i vari valori delle Z
z_test_var1_ita_pec = (test_var1_ita_pec$statistic)
z_test_var2_ita_pec = (test_var2_ita_pec$statistic)
z_test_var1_deu_pec = (test_var1_deu_pec$statistic)
z_test_var2_deu_pec = (test_var2_deu_pec$statistic)
z_test_var1_bel_pec = (test_var1_bel_pec$statistic)
z_test_var2_bel_pec = (test_var2_bel_pec$statistic)
z_test_var1_nld_pec = (test_var1_nld_pec$statistic)
z_test_var2_nld_pec = (test_var2_nld_pec$statistic)
z_test_var1_swe_pec = (test_var1_swe_pec$statistic)
z_test_var2_swe_pec = (test_var2_swe_pec$statistic)

###creiamo il datafreme per la tabella

z_table <- data.frame(Country = c("ITA","", "DEU","", "BEL","", "NLD", "", "SWE",""),
                      Variable = c("PGDP","PEC", "PGDP","PEC", "PGDP","PEC", "PGDP","PEC", "PGDP","PEC"),
                      Level =  round(c(z_test_var1_ita_pgdp, z_test_var1_ita_pec, z_test_var1_deu_pgdp,  z_test_var1_deu_pec, z_test_var1_bel_pgdp, z_test_var1_bel_pec, z_test_var1_nld_pgdp, z_test_var1_nld_pec, z_test_var1_swe_pgdp, z_test_var1_swe_pec),3),
                      First_Difference = round(c(z_test_var2_ita_pgdp, z_test_var2_ita_pec, z_test_var2_deu_pgdp, z_test_var2_deu_pec, z_test_var2_bel_pgdp, z_test_var2_bel_pec, z_test_var2_nld_pgdp, z_test_var2_nld_pec, z_test_var2_swe_pgdp, z_test_var2_swe_pec ),3))
z_table$First_Difference <- ifelse(z_table$First_Difference < -2.86, paste0(z_table$First_Difference, "**"), z_table$First_Difference)
z_table$Level <- ifelse(z_table$Level < -2.57, paste0(z_table$Level, "*"), z_table$Level)
datatable(z_table, 
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = FALSE,
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          class = "compact",
          colnames = c("Country", "Variable","Level","First difference")) %>%
  formatStyle(0:4,  # Indici delle prime due colonne (Country e Variable)
              width = "50px"  # Larghezza desiderata
  )%>%
  formatStyle(
    names(z_table),  # Nomi di tutte le colonne
    textAlign = "center"  # Allineamento al centro
  )%>%
  formatStyle(
    1:length(z_table),  # Indici di tutte le colonne
    `border-left` = "1px solid #BBB"  # Linea separatrice sinistra delle colonne
  )

####creiamo i grafici dell autocorrelazione per valutare la stazionarietà graficamente dei gr_PGDP e gr_PEC

##gr_PGDP
par(mfrow=c(2,3))

PGDP_PEC_ITA$gr_PGDP%>%
  acf(lag.max = 44, main = "ACF of the gr_PGDP_ITA - Logs", na.action = na.pass)
PGDP_PEC_DEU$gr_PGDP%>%
  acf(lag.max = 44, main = "ACF of the gr_PGDP_DEU - Logs", na.action = na.pass)
PGDP_PEC_BEL$gr_PGDP%>%
  acf(lag.max = 44, main = "ACF of the gr_PGDP_BEL - Logs", na.action = na.pass)
PGDP_PEC_NLD$gr_PGDP%>%
  acf(lag.max = 44, main = "ACF of the gr_PGDP_NLD - Logs", na.action = na.pass)
PGDP_PEC_SWE$gr_PGDP%>%
  acf(lag.max = 44, main = "ACF of the gr_PGDP_SWE - Logs", na.action = na.pass)

##gr_pec
par(mfrow=c(2,3))

PGDP_PEC_ITA$gr_PEC%>%
  acf(lag.max = 44, main = "ACF of the gr_PEC_ITA - Logs", na.action = na.pass)
PGDP_PEC_DEU$gr_PEC%>%
  acf(lag.max = 44, main = "ACF of the gr_PEC_DEU - Logs", na.action = na.pass)
PGDP_PEC_BEL$gr_PEC%>%
  acf(lag.max = 44, main = "ACF of the gr_PEC_BEL - Logs", na.action = na.pass)
PGDP_PEC_NLD$gr_PEC%>%
  acf(lag.max = 44, main = "ACF of the gr_PEC_NLD - Logs", na.action = na.pass)
PGDP_PEC_SWE$gr_PEC%>%
  acf(lag.max = 44, main = "ACF of the gr_PEC_SWE - Logs", na.action = na.pass)

##########################################################################################
#################
##############creiamo la tabella AIC e BIC per trovare il ritardo (p) piu adatto per il modello var(p)
#################
##########################################################################################
d_VAR_ITA <- subset(PGDP_PEC_ITA, select = c("gr_PGDP", "gr_PEC"))
d_VAR_ITA<-na.omit(d_VAR_ITA)
VAR_MODEL_ITA<-VARselect(d_VAR_ITA, lag.max = 4, type = "const")
AIC_VAR_ITA <- VAR_MODEL_ITA$criteria[1,]
BIC_VAR_ITA <- VAR_MODEL_ITA$criteria[3,]
unit_ITA<-round(c(AIC_VAR_ITA,BIC_VAR_ITA),2)

d_VAR_DEU <- subset(PGDP_PEC_DEU, select = c("gr_PGDP", "gr_PEC"))
d_VAR_DEU<-na.omit(d_VAR_DEU)
VAR_MODEL_DEU<-VARselect(d_VAR_DEU, lag.max = 4, type = "const")
AIC_VAR_DEU<-VAR_MODEL_DEU$criteria[1,]
BIC_VAR_DEU <- VAR_MODEL_DEU$criteria[3,]
unit_DEU<-round(c(AIC_VAR_DEU,BIC_VAR_DEU),2)

d_VAR_BEL <- subset(PGDP_PEC_BEL, select = c("gr_PGDP", "gr_PEC"))
d_VAR_BEL<-na.omit(d_VAR_BEL)
VAR_MODEL_BEL<-VARselect(d_VAR_BEL, lag.max = 4, type = "const")
AIC_VAR_BEL<-VAR_MODEL_BEL$criteria[1,]
BIC_VAR_BEL <- VAR_MODEL_BEL$criteria[3,]
unit_BEL<-round(c(AIC_VAR_BEL,BIC_VAR_BEL),2)

d_VAR_NLD <- subset(PGDP_PEC_NLD, select = c("gr_PGDP", "gr_PEC"))
d_VAR_NLD<-na.omit(d_VAR_NLD)
VAR_MODEL_NLD<-VARselect(d_VAR_NLD, lag.max = 4, type = "const")
AIC_VAR_NLD<-VAR_MODEL_NLD$criteria[1,]
BIC_VAR_NLD <- VAR_MODEL_NLD$criteria[3,]
unit_NLD<-round(c(AIC_VAR_NLD,BIC_VAR_NLD),2)

d_VAR_SWE <- subset(PGDP_PEC_SWE, select = c("gr_PGDP", "gr_PEC"))
d_VAR_SWE<-na.omit(d_VAR_SWE)
VAR_MODEL_SWE<-VARselect(d_VAR_SWE, lag.max = 4, type = "const")
AIC_VAR_SWE<-VAR_MODEL_SWE$criteria[1,]
BIC_VAR_SWE <- VAR_MODEL_SWE$criteria[3,]
unit_SWE<-round(c(AIC_VAR_SWE,BIC_VAR_SWE),2)

###creo la tabella per AIC e BIC per il modello VAR
VAR_table_AIC_BIC<-bind_rows(unit_ITA,
                             unit_DEU,
                             unit_BEL,
                             unit_NLD,
                             unit_SWE)%>%
  rename("AIC (1)" = '1...1',
         "AIC (2)" = '2...2',
         "AIC (3)" = '3...3',
         "AIC (4)" = '4...4',
         "BIC (1)" = '1...5',
         "BIC (2)" = '2...6',
         "BIC (3)" = '3...7',
         "BIC (4)" = '4...8')

###ora la trasformo in un data.frame per aggiungere normalmente una colonna
VAR_table_AIC_BIC<-as.data.frame(VAR_table_AIC_BIC)
VAR_table_AIC_BIC$Country<-c("ITA", "DEU", "BEL", "NLD", "SWE")

###lo riordino
VAR_table_AIC_BIC<-VAR_table_AIC_BIC%>%
  dplyr::select(Country,
         "AIC (1)","AIC (2)","AIC (3)","AIC (4)",
         "BIC (1)","BIC (2)","BIC (3)","BIC (4)")

#creo un vettore contenente le variabili: "AIC (1)", "AIC (2)".... "BIC (1)"....
AIC_names_VAR <- names(VAR_table_AIC_BIC)[1:4] 
#creo una nuova colonna 
VAR_table_AIC_BIC$Optimal_lag<- AIC_names_VAR[apply(VAR_table_AIC_BIC[, AIC_names_VAR], 1, which.min)]
#creo la tabella visiva tramite la funzione "datatable" del pacchetto DT
datatable(VAR_table_AIC_BIC, 
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = FALSE,
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          class = "compact",
          colnames = c("Country", "AIC (1)", "AIC (2)","AIC (3)","AIC (4)","BIC (1)","BIC (2)","BIC (3)","BIC (4)","Optimal lag")) %>%
  formatStyle(0:10,  # Indici delle prime due colonne (Country e Variable)
              width = "50px"  # Larghezza desiderata
  )%>%
  formatStyle(
    names(VAR_table_AIC_BIC),  # Nomi di tutte le colonne
    textAlign = "center"  # Allineamento al centro
  )%>%
  formatStyle(
    1:length(VAR_table_AIC_BIC),  # Indici di tutte le colonne
    `border-left` = "1px solid #BBB"  # Linea separatrice sinistra delle colonne
  )

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

#CREIAMO IL MODELLO VAR PER OGNI PAESE
VARita = vars::VAR(y = PGDP_PEC_ITA[-1,c(13,14)], p = 1)
#TESTIAMO TRAMITE IL TEST DI GRANCGER LA PRESENZA O MENO DI CASUALITà TRA LE VARIABILI
gt_PGDP_PEC_ITA <- causality(VARita, cause = "gr_PGDP")
gt_PEC_PGDP_ITA <- causality(VARita, cause = "gr_PEC")
gt_PGDP_PEC_ITA
gt_PEC_PGDP_ITA

coeftest(VARita, vcov. = NeweyWest(VARita, lag = 3, prewhite = F)) 
summary(VARita)
#ESTRAIAMO IL VALORE DELLO Z TEST
z_value_gt_pgdp_pec_ita<-paste(round(gt_PGDP_PEC_ITA$Granger$statistic[1,],2),"[",round(gt_PGDP_PEC_ITA$Granger$p.value[1,],3),"]",
                               ifelse(gt_PGDP_PEC_ITA$Granger$p.value[1,]<0.05,"**",ifelse(gt_PGDP_PEC_ITA$Granger$p.value[1,]<0.1&gt_PGDP_PEC_ITA$Granger$p.value[1,]>0.05,"*","")),sep="")
z_value_gt_pec_pgdp_ita<-paste(round(gt_PEC_PGDP_ITA$Granger$statistic[1,],2),"[",round(gt_PEC_PGDP_ITA$Granger$p.value[1,],3),"]",
                               ifelse(gt_PEC_PGDP_ITA$Granger$p.value[1,]<0.05,"**",ifelse(gt_PEC_PGDP_ITA$Granger$p.value[1,]<0.1&gt_PEC_PGDP_ITA$Granger$p.value[1,]>0.05,"*","")),sep="")
################################################################################
#CREIAMO IL MODELLO VAR PER OGNI PAESE
VARdeu = vars::VAR(y = PGDP_PEC_DEU[-1,c(13,14)], p = 3)
#TESTIAMO TRAMITE IL TEST DI GRANCGER LA PRESENZA O MENO DI CASUALITà TRA LE VARIABILI
gt_PGDP_PEC_DEU <- causality(VARdeu, cause = "gr_PGDP")
gt_PEC_PGDP_DEU <- causality(VARdeu, cause = "gr_PEC")
gt_PGDP_PEC_DEU
gt_PEC_PGDP_DEU

coeftest(VARdeu, vcov. = NeweyWest(VARdeu, lag = 3, prewhite = F)) 
summary(VARdeu)
#ESTRAIAMO IL VALORE DELLO Z TEST
z_value_gt_pgdp_pec_deu<-paste(round(gt_PGDP_PEC_DEU$Granger$statistic[1,],2),"[",round(gt_PGDP_PEC_DEU$Granger$p.value[1,],3),"]",
                               ifelse(gt_PGDP_PEC_DEU$Granger$p.value[1,]<0.05,"**",ifelse(gt_PGDP_PEC_DEU$Granger$p.value[1,]<0.1&gt_PGDP_PEC_DEU$Granger$p.value[1,]>0.05,"*","")),sep="")
z_value_gt_pec_pgdp_deu<-paste(round(gt_PEC_PGDP_DEU$Granger$statistic[1,],2),"[",round(gt_PEC_PGDP_DEU$Granger$p.value[1,],3),"]",
                               ifelse(gt_PEC_PGDP_DEU$Granger$p.value[1,]<0.05,"**",ifelse(gt_PEC_PGDP_DEU$Granger$p.value[1,]<0.1&gt_PEC_PGDP_DEU$Granger$p.value[1,]>0.05,"*","")),sep="")

################################################################################
#CREIAMO IL MODELLO VAR PER OGNI PAESE
VARbel = vars::VAR(y = PGDP_PEC_BEL[-1,c(13,14)], p = 1)
#TESTIAMO TRAMITE IL TEST DI GRANCGER LA PRESENZA O MENO DI CASUALITà TRA LE VARIABILI
gt_PGDP_PEC_BEL <- causality(VARbel, cause = "gr_PGDP")
gt_PEC_PGDP_BEL <- causality(VARbel, cause = "gr_PEC")
gt_PGDP_PEC_BEL
gt_PEC_PGDP_BEL

coeftest(VARbel, vcov. = NeweyWest(VARbel, lag = 3, prewhite = F)) 
summary(VARbel)
#ESTRAIAMO IL VALORE DELLO Z TEST
z_value_gt_pgdp_pec_bel<-paste(round(gt_PGDP_PEC_BEL$Granger$statistic[1,],2),"[",round(gt_PGDP_PEC_BEL$Granger$p.value[1,],3),"]",
                               ifelse(gt_PGDP_PEC_BEL$Granger$p.value[1,]<0.05,"**",ifelse(gt_PGDP_PEC_BEL$Granger$p.value[1,]<0.1&gt_PGDP_PEC_BEL$Granger$p.value[1,]>0.05,"*","")),sep="")
z_value_gt_pec_pgdp_bel<-paste(round(gt_PEC_PGDP_BEL$Granger$statistic[1,],2),"[",round(gt_PEC_PGDP_BEL$Granger$p.value[1,],3),"]",
                               ifelse(gt_PEC_PGDP_BEL$Granger$p.value[1,]<0.05,"**",ifelse(gt_PEC_PGDP_BEL$Granger$p.value[1,]<0.1&gt_PEC_PGDP_BEL$Granger$p.value[1,]>0.05,"*","")),sep="")
################################################################################
#CREIAMO IL MODELLO VAR PER OGNI PAESE
VARnld = vars::VAR(y = PGDP_PEC_NLD[-1,c(13,14)], p = 1)
#TESTIAMO TRAMITE IL TEST DI GRANCGER LA PRESENZA O MENO DI CASUALITà TRA LE VARIABILI
gt_PGDP_PEC_NLD <- causality(VARnld, cause = "gr_PGDP")
gt_PEC_PGDP_NLD <- causality(VARnld, cause = "gr_PEC")
gt_PGDP_PEC_NLD
gt_PEC_PGDP_NLD

coeftest(VARnld, vcov. = NeweyWest(VARnld, lag = 3, prewhite = F)) 
summary(VARnld)
#ESTRAIAMO IL VALORE DELLO Z TEST
z_value_gt_pgdp_pec_nld<-paste(round(gt_PGDP_PEC_NLD$Granger$statistic[1,],2),"[",round(gt_PGDP_PEC_NLD$Granger$p.value[1,],3),"]",
                               ifelse(gt_PGDP_PEC_NLD$Granger$p.value[1,]<0.05,"**",ifelse(gt_PGDP_PEC_NLD$Granger$p.value[1,]<0.1&gt_PGDP_PEC_NLD$Granger$p.value[1,]>0.05,"*","")),sep="")
z_value_gt_pec_pgdp_nld<-paste(round(gt_PEC_PGDP_NLD$Granger$statistic[1,],2),"[",round(gt_PEC_PGDP_NLD$Granger$p.value[1,],3),"]",
                               ifelse(gt_PEC_PGDP_NLD$Granger$p.value[1,]<0.05,"**",ifelse(gt_PEC_PGDP_NLD$Granger$p.value[1,]<0.1&gt_PEC_PGDP_NLD$Granger$p.value[1,]>0.05,"*","")),sep="")
################################################################################
#CREIAMO IL MODELLO VAR PER OGNI PAESE
VARswe = vars::VAR(y = PGDP_PEC_SWE[-1,c(13,14)], p = 1)
#TESTIAMO TRAMITE IL TEST DI GRANCGER LA PRESENZA O MENO DI CASUALITà TRA LE VARIABILI
gt_PGDP_PEC_SWE <- causality(VARswe, cause = "gr_PGDP")
gt_PEC_PGDP_SWE <- causality(VARswe, cause = "gr_PEC")
gt_PGDP_PEC_SWE
gt_PEC_PGDP_SWE

coeftest(VARswe, vcov. = NeweyWest(VARswe, lag = 3, prewhite = F)) 
summary(VARswe)
#ESTRAIAMO IL VALORE DELLO Z TEST
z_value_gt_pgdp_pec_swe<-paste(round(gt_PGDP_PEC_SWE$Granger$statistic[1,],2),"[",round(gt_PGDP_PEC_SWE$Granger$p.value[1,],3),"]",
                               ifelse(gt_PGDP_PEC_SWE$Granger$p.value[1,]<0.05,"**",ifelse(gt_PGDP_PEC_SWE$Granger$p.value[1,]<0.1&gt_PGDP_PEC_SWE$Granger$p.value[1,]>0.05,"*","")),sep="")
z_value_gt_pec_pgdp_swe<-paste(round(gt_PEC_PGDP_SWE$Granger$statistic[1,],2),"[",round(gt_PEC_PGDP_SWE$Granger$p.value[1,],3),"]",
                               ifelse(gt_PEC_PGDP_SWE$Granger$p.value[1,]<0.05,"**",ifelse(gt_PEC_PGDP_SWE$Granger$p.value[1,]<0.1&gt_PEC_PGDP_SWE$Granger$p.value[1,]>0.05,"*","")),sep="")
################################################################################
##CREO UN DF PER RIASSUMERE IN UNA TABELLA IL TEST DI GRANGER
table_granger_test<-data.frame(
  Country = c("ITA", "","", "DEU", "","", "BEL", "","", "NLD", "","", "SWE", ""),
  PEC_Granger_causes_PGDP = c(z_value_gt_pec_pgdp_ita,"","", z_value_gt_pec_pgdp_deu,"","", z_value_gt_pec_pgdp_bel,"","", z_value_gt_pec_pgdp_nld,"","", z_value_gt_pec_pgdp_swe,""),
  PGDP_Granger_causes_PEC = c(z_value_gt_pgdp_pec_ita,"","", z_value_gt_pgdp_pec_deu,"","", z_value_gt_pgdp_pec_bel,"","", z_value_gt_pgdp_pec_nld,"","", z_value_gt_pgdp_pec_swe,""),
  Direction_of_causality = c("PEC=/>PGDP", "PGDP=>PEC","", "PEC=/>PGDP", "PGDP=/>PEC","", "PEC=/>PGDP", "PGDP=>PEC","", "PEC=/>PGDP", "PGDP=/>PEC","", "PEC=/>PGDP", "PGDP=/>PEC")
)

datatable(table_granger_test, 
          options = list(
            paging = TRUE,
            pageLength = 15,
            lengthMenu = FALSE,
            searching = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          class = "compact",
          colnames = c("Country", "PEC Granger causes PGDP", "PGDP Granger causes PEC","Direction of causality")) %>%
  formatStyle(0:4,  # Indici delle prime due colonne (Country e Variable)
              width = "50px"  # Larghezza desiderata
  )%>%
  formatStyle(
    names(table_granger_test),  # Nomi di tutte le colonne
    textAlign = "center"  # Allineamento al centro
  )%>%
  formatStyle(
    1:length(table_granger_test),  # Indici di tutte le colonne
    `border-left` = "1px solid #BBB"  # Linea separatrice sinistra delle colonne
  )

###########################################################################################################################################
##IMPULS RESPONSE FUNCTIONS
irfITA = lp_lin(endog_data = PGDP_PEC_ITA[-1,c(13,14)],  # the variables
              lags_endog_lin = 1,        # the number of lags
              trend = 0,                 # we do not include a trend
              shock_type = 0,            # shock equal to 1 standard deviation
              confint = 1.96,            # 95% confidence interval (from standard normal)
              hor = 8)                   # how many periods ahead (2 years)

plot(irfITA)
##IMPULS RESPONSE FUNCTIONS
irfDEU = lp_lin(endog_data = PGDP_PEC_DEU[-1,c(13,14)],  # the variables
                lags_endog_lin = 3,        # the number of lags
                trend = 0,                 # we do not include a trend
                shock_type = 0,            # shock equal to 1 standard deviation
                confint = 1.96,            # 95% confidence interval (from standard normal)
                hor = 8)                   # how many periods ahead (2 years)

plot(irfDEU)
##IMPULS RESPONSE FUNCTIONS
irfBEL = lp_lin(endog_data = PGDP_PEC_BEL[-1,c(13,14)],  # the variables
                lags_endog_lin = 1,        # the number of lags
                trend = 0,                 # we do not include a trend
                shock_type = 0,            # shock equal to 1 standard deviation
                confint = 1.96,            # 95% confidence interval (from standard normal)
                hor = 8)                   # how many periods ahead (2 years)

plot(irfBEL)
##IMPULS RESPONSE FUNCTIONS
irfNLD = lp_lin(endog_data = PGDP_PEC_NLD[-1,c(13,14)],  # the variables
                lags_endog_lin = 1,        # the number of lags
                trend = 0,                 # we do not include a trend
                shock_type = 0,            # shock equal to 1 standard deviation
                confint = 1.96,            # 95% confidence interval (from standard normal)
                hor = 8)                   # how many periods ahead (2 years)

plot(irfNLD)
##IMPULS RESPONSE FUNCTIONS
irfSWE = lp_lin(endog_data = PGDP_PEC_SWE[-1,c(13,14)],  # the variables
                lags_endog_lin = 1,        # the number of lags
                trend = 0,                 # we do not include a trend
                shock_type = 0,            # shock equal to 1 standard deviation
                confint = 1.96,            # 95% confidence interval (from standard normal)
                hor = 8)                   # how many periods ahead (2 years)

plot(irfSWE)
#############################################################################################################################################









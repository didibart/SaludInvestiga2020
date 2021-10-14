#datos eficiencia del sistema de salud
library(sqldf)
install.packages("dplyr")
library(dplyr)
datSexoDNI <- read.csv("SaludInvestiga2020/SNVS.dni.sexo.csv"
                       ,header = TRUE
                       ,sep = ",", stringsAsFactors = TRUE
                       ,encoding = "UTF-8")
datBaseGrande <- read.csv("SaludInvestiga2020/merge.SNVS_S_I.2.csv"
                          ,header = TRUE
                          ,sep = ",", stringsAsFactors = TRUE
                          ,encoding = "UTF-8")
datEncuestaCI <- read.csv("SaludInvestiga2020/Encuesta_CI_Final.csv"
                          ,header = TRUE
                          ,sep = ",", stringsAsFactors = TRUE
                          ,encoding = "UTF-8")
nrow(datBaseGrande)
#regiones sanitarias
levels(datBaseGrande$Region_Sanitaria_RESIDENCIA)

datBaseDistintosRS <- datBaseGrande %>% distinct(Region_Sanitaria_RESIDENCIA)
View(datBaseDistintosRS)
View(datBaseGrande)
View(datSexoDNI)
nrow(datSexoDNI)
View(datEncuestaCI)
#remuevo duplicados de DNI
datSexoDNIUnicos <- datSexoDNI[!duplicated(datSexoDNI$NRO_DOC), ]
View(datSexoDNIUnicos)
datBaseCompDNI <- merge(datBaseGrande, 
                        datSexoDNIUnicos, by = "NRO_DOC")
View(datBaseCompDNI)
#agrego sexo
datBaseGrande$SEXO <- datBaseCompDNI$SEXO
#58723 son los que coinciden en el DNI
#saco la diferencia entrte la fecha de internacion y la de ocurrencia
datBaseGrande$difdia<- difftime(as.POSIXct(datBaseCompDNI$FECHA_INGRESO, format="%Y-%m-%d"),
              as.POSIXct(datBaseCompDNI$FECHA_GRAFICO, format="%Y-%m-%d"), units = c("days"))
View(datBaseGrande)
#borro los no internados (tienen NA en difdia)
datBaseGrandeSinNA <- datBaseGrande[!is.na(datBaseGrande$difdia), ]
nrow(datBaseGrandeSinNA)
# 117564 (totales internados)
View(datBaseGrandeSinNA)
nrow(datBaseGrandeSinNA[datBaseGrandeSinNA$difdia < 0, ])
# 18960 (internados antes del diagnostico covid)
nrow(datBaseGrandeSinNA[datBaseGrandeSinNA$difdia > 45, ])
# 33634 (internados 45 días después del covid)
117326 - (18960 + 33634)
# 64732 estos son los internados por covid
datBaseInternados <- subset(datBaseGrandeSinNA, 
                            datBaseGrandeSinNA$difdia > 0 )
nrow(datBaseInternados)
View(datBaseInternados)
# 90412 son los internados antes del covid
datBaseInternadosComp <- subset(datBaseInternados, 
                                datBaseInternados$difdia <= 45)
nrow(datBaseInternadosComp)
View(datBaseInternadosComp)
#56778 son los internados por covid
View(datEncuestaCI)
datBaseIntSexoEnc <- merge(datBaseInternadosComp, 
                           datEncuestaCI, by = "ESTABLECIMIENTO")
nrow(datBaseIntSexoEnc)
#698 con solo los hospitales de la encuesta
View(datBaseIntSexoEnc)

#write.csv(datBaseIntSexoEnc, file = "SaludInvestiga2020/datBaseIntSexoEnc.csv", sep =";", col.names = TRUE)

# le agrego los datos de nbi y cobertura en residencia y ocurrencia
datBaseIntSexoEnc$MUNICIPIO_RESIDENCIA <- toupper(as.character(datBaseIntSexoEnc$DEPARTAMENTO_RESIDENCIA)) 
datBaseDistintos <- datBaseIntSexoEnc %>% distinct(MUNICIPIO_RESIDENCIA)
nrow(datBaseDistintos)
datMuniNBIResidencia <- read.csv("SaludInvestiga2020/MuniNBICobResidencia.csv"
                          ,header = TRUE
                          ,sep = ",", stringsAsFactors = TRUE
                          ,encoding = "UTF-8")

datBaseIntSexoEncNBIRes <- merge(datBaseIntSexoEnc, 
                              datMuniNBIResidencia, by = "MUNICIPIO_RESIDENCIA")
View(datBaseIntSexoEncNBIRes)
datMuniNBIOcurrencia <- read.csv("SaludInvestiga2020/MuniNBICobOcurrencia.csv"
                                 ,header = TRUE
                                 ,sep = ",", stringsAsFactors = TRUE
                                 ,encoding = "UTF-8")
datBaseIntSexoEncNBIResOcu <- merge(datBaseIntSexoEncNBIRes, 
                                 datMuniNBIOcurrencia, by = "PARTIDO_Ocurrencia")
View(datBaseIntSexoEncNBIResOcu)
#nro final 1294
datBaseIntSexoEncNBIResOcu1 <- datBaseIntSexoEncNBIResOcu
#class para ver tipo de dato
class(datBaseIntSexoEncNBIResOcu$FALLECIDO)
#con esto reemplazo empty strings en dataframe column
# pongo todos los en blanco como si fueran No fallecidos ND y en blanco
which.one <- which( levels(datBaseIntSexoEncNBIResOcu$FALLECIDO) == "ND" )
levels(datBaseIntSexoEncNBIResOcu$FALLECIDO)[which.one] <- "NO"
levels(datBaseIntSexoEncNBIResOcu$FALLECIDO)
#capacidad instalda puse como NO ADECUADA todos los ND y los vacíos tb
which.one <- which( levels(datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA) == "" )
levels(datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA)[which.one] <- "NO ADECUADA"
levels(datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA)
#tipo cuidado los que son vacíos o guion los puse MINIMOS
which.one <- which( levels(datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO) == "-" )
levels(datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO)[which.one] <- "MINIMOS"
levels(datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO)
#uti los vacios los puse como no
which.one <- which( levels(datBaseIntSexoEncNBIResOcu$UTI) == "" )
levels(datBaseIntSexoEncNBIResOcu$UTI)[which.one] <- "no"
levels(datBaseIntSexoEncNBIResOcu$UTI)
#reviso regiones sanitarias, están representadas sólo los que contestaron la encuesta
datRSFALL <- subset(datBaseIntSexoEncNBIResOcu, FALLECIDO == "SI")
View(datRSFALL)
#reviso fase, falta fase  (pongo fase 3)
datBaseDistintosFase <- datBaseIntSexoEncNBIResOcu %>% distinct(FASE)
datFASE <- subset(datBaseIntSexoEncNBIResOcu, is.na(FASE))
View(datFASE)
datBaseIntSexoEncNBIResOcu[["FASE"]][is.na(datBaseIntSexoEncNBIResOcu[["FASE"]])] <- 3

View(datBaseIntSexoEncNBIResOcu)
write.csv(datBaseIntSexoEncNBIResOcu, file = "SaludInvestiga2020/datBaseIntSexoEncNBIResOcu.csv", sep =";", col.names = TRUE)
datBaseIntSexoEncNBIResOcu <- read.csv("SaludInvestiga2020/datBaseIntSexoEncNBIResOcu.csv"
                                       ,header = TRUE
                                       ,sep = ",", stringsAsFactors = TRUE
                                       ,encoding = "UTF-8")
nrow(datBaseIntSexoEncNBIResOcu)
# graficas
library(ggplot2)
#ggplot(data = datBaseIntSexoEncNBIResOcu, aes(x = Pob_OCURRENCIA, y = Por_Pob_NBI_OCURRENCIA)) + geom_point()

# tiro asociación chi cuadrado capacidad instalada -> fallecido
chisq.test(datBaseIntSexoEncNBIResOcu$FALLECIDO, datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA)
ggplot(data = datBaseIntSexoEncNBIResOcu, aes(x = FALLECIDO, y = CAPACIDAD_INSTALADA, color = CAPACIDAD_INSTALADA)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")
# tiro chi cuadrado uti -> fallecido
chisq.test(datBaseIntSexoEncNBIResOcu$FALLECIDO, datBaseIntSexoEncNBIResOcu$UTI)
ggplot(data = datBaseIntSexoEncNBIResOcu, aes(x = FALLECIDO, y = UTI, color = FALLECIDO)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")
# tiro chi cuadrado tipo_cuidado -> fallecido
chisq.test(datBaseIntSexoEncNBIResOcu$FALLECIDO, datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO)
ggplot(data = datBaseIntSexoEncNBIResOcu, aes(x = FALLECIDO, y = TIPO_CUIDADO, color = FALLECIDO)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")
# tiro9 chi cuadrado tipo_cuidado -> capacidad instalada
chisq.test(datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA, datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO)
ggplot(data = datBaseIntSexoEncNBIResOcu, aes(x = CAPACIDAD_INSTALADA, y = TIPO_CUIDADO, color = CAPACIDAD_INSTALADA)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")
#tablas de frecuencias
table(datBaseIntSexoEncNBIResOcu$FALLECIDO)
table(datBaseIntSexoEncNBIResOcu$CAPACIDAD_INSTALADA)
table(datBaseIntSexoEncNBIResOcu$UTI)
table(datBaseIntSexoEncNBIResOcu$TIPO_CUIDADO)
table(datBaseIntSexoEncNBIResOcu$FASE)
# agrego variable dummy de fallecido
datBaseIntSexoEncNBIResOcu$FALLECIDODUMMY <- ifelse(datBaseIntSexoEncNBIResOcu$FALLECIDO == "SI", 1, 0)
# tire regresión logística nbi ocurrencia -> fallecido
summary(glm(FALLECIDODUMMY ~ Por_Pob_NBI_OCURRENCIA, data = datBaseIntSexoEncNBIResOcu, family = "binomial"))
ggplot(datBaseIntSexoEncNBIResOcu, aes(Por_Pob_NBI_OCURRENCIA)) +
  geom_histogram(aes(fill = FALLECIDO), color = "black", binwidth = 2)

ggplot(datBaseIntSexoEncNBIResOcu, 
       aes(x=Por_Pob_NBI_OCURRENCIA, y=FALLECIDODUMMY)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
# tire regresión logística nbi residencia -> fallecido
summary(glm(FALLECIDODUMMY ~ Por_Pob_NBI_RESIDENCIA, data = datBaseIntSexoEncNBIResOcu, family = "binomial"))
ggplot(datBaseIntSexoEncNBIResOcu, aes(Por_Pob_NBI_RESIDENCIA)) +
  geom_histogram(aes(fill = FALLECIDO), color = "black", binwidth = 2)

ggplot(datBaseIntSexoEncNBIResOcu, 
       aes(x=Por_Pob_NBI_RESIDENCIA, y=FALLECIDODUMMY)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

# tire regresion logistica para cobertura -> fallecido
summary(glm(FALLECIDODUMMY ~ Por_Pob_SC_OCURRENCIA, data = datBaseIntSexoEncNBIResOcu, family = "binomial"))
ggplot(datBaseIntSexoEncNBIResOcu, aes(Por_Pob_SC_OCURRENCIA)) +
  geom_histogram(aes(fill = FALLECIDO), color = "black", binwidth = 2)

ggplot(datBaseIntSexoEncNBIResOcu, 
       aes(x=Por_Pob_SC_OCURRENCIA, y=FALLECIDODUMMY)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
# tiro arbol de decision
install.packages("rpart")
library(rpart)
# crecemos el arbol
fit <- rpart(FALLECIDO~UTI + CAPACIDAD_INSTALADA + 
               FASE + TIPO_CUIDADO, method="anova", 
             data=datBaseIntSexoEncNBIResOcu)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE,
     main="Regresión para FALLECIDO")
text(fit, pos = 3, offset = -1, use.n=TRUE, all=TRUE, cex=.6)

dfHogaresCenso2010 <- read.csv("SaludInvestiga2020/Censo2010/HOGAR.csv"
                               ,header = TRUE
                               ,sep = ";")
head(dfHogaresCenso2010)
dfViviendasCenso2010 <- read.csv("SaludInvestiga2020/Censo2010/VIVIENDA.csv"
                               ,header = TRUE
                               ,sep = ";")
#####################################################
### Datos de los google cals que subió Vanesa ####
####################################################
dfIndi1 <- read.csv("SaludInvestiga2020/indicadores1.csv"
                    ,header = TRUE
                    ,sep = ",", stringsAsFactors = TRUE)
dfIndi2 <- read.csv("SaludInvestiga2020/indicadores2.csv"
                    ,header = TRUE
                    ,sep = ",", stringsAsFactors = TRUE)

View(dfIndi1)
View(dfIndi2)
library(dplyr)
#achico el nombre de region sanitaria a region
dfIndi1 <- dfIndi1 %>% dplyr::rename(Region = Region.Sanitaria)

library(sqldf)
#unifico el nombre del municipio de nueve de julio
levels(dfIndi1$Municipio)[levels(dfIndi1$Municipio)=="9 DE JULIO"] <- "NUEVE DE JULIO"
levels(dfIndi1$Municipio)[levels(dfIndi1$Municipio)=="25 DE MAYO"] <- "VEINTICINCO DE MAYO"
#elijo todos los dato para cada municipio
dfOrdenIndi1 <- sqldf("SELECT * FROM dfIndi1 WHERE Region = 12 
                      ORDER BY Municipio")
View(dfOrdenIndi1)
#escribo el csv para luego copiar y pegar en el google calc
write.csv(dfOrdenIndi1,file="SaludInvestiga2020/region1.csv")

dfOrdenIndi2 <- sqldf("SELECT * FROM dfIndi2 WHERE Region = 12 
                      ORDER BY Municipio")
View(dfOrdenIndi2)
write.csv(dfOrdenIndi2,file="SaludInvestiga2020/region2.csv")
####################################################################
install.packages("readxl")
install.packages("sqldf")
library(readxl)
library(sqldf)
Dataxls <- read_excel("SaludInvestiga2020/ocurrencia.residencia2020.xlsx")
head(Dataxls)                    
dim (Dataxls)
var(Dataxls)
#en que rs ocurrio el evento
View(Dataxls$RS_OCURRENCIA)
Dataxls$COVID
#es cie 10?
View(Dataxls$DIAG_PRIMARIO_COD)
Dataxls$FECHA_INGRESO
Dataxls$clasif
#AMBA y Resto
Dataxls$AREA_OCURRENCIA
Dataxls$UTI
Dataxls$edad
#AMBA y Resto
View(Dataxls$AREA_RESIDENCIA)
View(Dataxls$DEPARTAMENTO_RESIDENCIA)
Dataxls$CLASIF_RESUMEN
View(Dataxls$FALLECIDO_SISA)
colMeans(is.na(Dataxls))
p <- mean(complete.cases(Dataxls))
#arranque con csv mas simple
DatosEnfermedades <- read.csv("SaludInvestiga2020/ENFERMEDADES.csv"
                              ,header = TRUE
                              ,sep = ",", stringsAsFactors = TRUE)
DatosPedro <- read.csv("SaludInvestiga2020/ocurrencia.residencia2020.csv"
                    ,header = TRUE
                    ,sep = ",", stringsAsFactors = TRUE
                    )
colMeans(is.na(DatosPedro))
View(DatosPedro)
View(DatosEnfermedades)
View(DatosPedroEnf)
write.csv(x=DatosPedroEnf, file="SaludInvestiga2020/ocurrencia.residencia2020.enfermedades.csv")
DatosPedroSector <- sqldf("SELECT DISTINCT Sector FROM DatosPedro")
#armé una pequeña red
DatosPedroEnf <- sqldf("SELECT DatosPedro.*, DatosEnfermedades.NombreDeEnfermedad
                       FROM DatosPedro, DatosEnfermedades 
                       WHERE DIAG_PRIMARIO_COD = CodigoDeEnfermedad")
#RS_RESIDENCIA y RS_OCURRENCIA
DatosPedroCovid <- sqldf("SELECT * FROM DatosPedro Where COVID = 'si'")
nrow(DatosPedroCovid)
RedResiInter <- data.frame(DatosPedroCovid$RS_RESIDENCIA,DatosPedroCovid$RS_OCURRENCIA)
View(RedResiInterLimpia)
nrow(RedResiInter)
RedResiInterLimpia <- na.omit(RedResiInter)
nrow(RedResiInterLimpia)
library(igraph)
library(sqldf)
head(RedResiInterLimpia)
RedResiInterLimpia1 <- sqldf("Select Distinct * From RedResiInterLimpia")

nrow(RedResiInterLimpia1)
RedResiInterGr <- graph.data.frame(RedResiInterLimpia,directed=FALSE)
plot(RedResiInterGr, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2)
#grado
deg <- degree(RedResiInterGr, mode="all")
deg
write.csv(x=deg, file="SaludInvestiga2020/degree.csv")
#intermediacion
bet <- betweenness(RedResiInterGr, directed=T, weights=NA)
bet
write.csv(x=bet, file="SaludInvestiga2020/betweness.csv")
#cercania
closen <- closeness(RedResiInterGr, mode="all", weights=NA) 
closen
write.csv(x=closen, file="SaludInvestiga2020/cercania.csv")
#densidad de la red
densidad <- edge_density(simplify(RedResiInterGr), loops=T)
densidad
write.csv(x=densidad, file="SaludInvestiga2020/densidad.csv")
#diamtero, mayor distancia geodesica
diame <- diameter(RedResiInterGr, directed=F, weights=NA)
diame
write.csv(x=diame, file="SaludInvestiga2020/diametro.csv")
################################
### RS, COVID, sociodemograficos
DatosSocio <- read.csv("SaludInvestiga2020/sociodemoregiones.csv"
                       ,header = TRUE
                       ,sep = ",", stringsAsFactors = TRUE, 
                       encoding="UTF-8")

View(DatosSocio)
colnames(DatosSocio)[21] <- "Camas_UCI"
#11 es Pob.s.Cob
colnames(DatosSocio)[11] <- "Pob_Cob_Est"
#9 es NBI
colnames(DatosSocio)[9] <- "Pob_NBI"
DatosNBI_Cobertura <- sqldf("SELECT Pob_NBI, Pob_Cob_Est FROM DatosSocio")
View(DatosNBI_Cobertura)
write.csv(x=DatosNBI_Cobertura, file = "SaludInvestiga2020/datos-nbi-cobertura.csv")
DatosNBI_Cobertura_Reg <- sqldf("SELECT Regiones, Pob_NBI, Pob_Cob_Est FROM DatosSocio")
View(DatosNBI_Cobertura_Reg)
library(ggplot2)
ggplot(data = DatosNBI_Cobertura, aes(x = Pob_NBI, y = Pob_Cob_Est)) + geom_point()                  
rlang::last_error()
DatosSocioCamas <- sqldf("SELECT Regiones, Camas, Camas_UCI FROM DatosSocio")
write.csv(x=DatosSocioCamas, file = "SaludInvestiga2020/datos-socio-camas.csv")
View(DatosPedroEnf)
head(DatosPedroEnf)
DatosPedroEnfTab <- as.data.frame.table(DatosPedroEnf)
DatosEnfGruUTIno <- sqldf("SELECT RS_OCURRENCIA, COUNT(UTI) FROM DatosPedroEnf 
                        WHERE UTI = 'no'
                        GROUP BY RS_OCURRENCIA")
colnames(DatosEnfGruUTIno)[2] <- 'UTIno'
View(DatosEnfGruUTIno)
DatosEnfGruUTIsi <- sqldf("SELECT RS_OCURRENCIA, COUNT(UTI) FROM DatosPedroEnf 
                        WHERE UTI = 'si'
                        GROUP BY RS_OCURRENCIA")
colnames(DatosEnfGruUTIsi)[2] <- 'UTIsi'
View(DatosEnfGruUTIno)
View(DatosEnfGruUTIsi)
write.csv(x=DatosEnfGruUTIsi, file = "SaludInvestiga2020/datos-socio-camas-uti.csv")
DatosSocioUTI <- sqldf("SELECT DatosSocio.*, 
                       DatosEnfGruUTIsi.UTIsi,
                       DatosEnfGruUTIno.UTIno
                       FROM DatosSocio, DatosEnfGruUTIsi, DatosEnfGruUTIno
                       WHERE DatosSocio.CodigoRegion = DatosEnfGruUTIsi.RS_OCURRENCIA
                       AND DatosSocio.CodigoRegion = DatosEnfGruUTIno.RS_OCURRENCIA")
View(DatosSocioUTI)
DatosRSPobUTIsiUTIno <- sqldf("SELECT Regiones, Población, UTIsi, UTIno
                              FROM DatosSocioUTI")
View (DatosRSPobUTIsiUTIno)
##RS 1 66.65
(2.2 + 131.1) / 2
##RS 2 6
##RS 7 dato rraro
DatosEnfGruSECTORpu <- sqldf("SELECT RS_OCURRENCIA, COUNT(SECTOR) FROM DatosPedroEnf 
                        WHERE SECTOR = 'PU'
                        GROUP BY RS_OCURRENCIA")
DatosEnfGruSECTORpr <- sqldf("SELECT RS_OCURRENCIA, COUNT(SECTOR) FROM DatosPedroEnf 
                        WHERE SECTOR = 'PR'
                             GROUP BY RS_OCURRENCIA")
View(DatosEnfGruSECTORpu)
colnames(DatosEnfGruSECTORpr)[2] <- 'SectorPR'
DatosRSPobUTIsiUTISECTOR <- sqldf("SELECT DatosSocio.*, 
                       DatosEnfGruUTIsi.UTIsi,
                       DatosEnfGruUTIno.UTIno,
                       DatosEnfGruSECTORpu.SectorPU,
                       DatosEnfGruSECTORpr.SectorPR
                       FROM DatosSocio, DatosEnfGruUTIsi, 
                       DatosEnfGruUTIno, DatosEnfGruSECTORpu,
                       DatosEnfGruSECTORpr
                       WHERE 
                       DatosSocio.CodigoRegion = DatosEnfGruUTIsi.RS_OCURRENCIA
                       AND DatosSocio.CodigoRegion = DatosEnfGruUTIno.RS_OCURRENCIA
                       AND DatosSocio.CodigoRegion = DatosEnfGruSECTORpu.RS_OCURRENCIA
                       AND DatosSocio.CodigoRegion = DatosEnfGruSECTORpr.RS_OCURRENCIA            
                       ")
View(DatosRSPobUTIsiUTISECTOR)
##RS 7 dato rraro
DatosEnfGruCOVIDsi <- sqldf("SELECT RS_OCURRENCIA, COUNT(COVID) AS COVIDsi FROM DatosPedroEnf 
                        WHERE COVID = 'si'
                        GROUP BY RS_OCURRENCIA")
DatosEnfGruCOVIDno <- sqldf("SELECT RS_OCURRENCIA, COUNT(COVID) AS COVIDno FROM DatosPedroEnf 
                        WHERE COVID = 'no'
                             GROUP BY RS_OCURRENCIA")
colnames(DatosEnfGruCOVIDsi)[2] <- 'COVIDsi'
DatosRSPobUTIsiUTISECTORCOVID <- sqldf("SELECT DatosSocio.*, 
                                  DatosEnfGruUTIsi.UTIsi,
                                  DatosEnfGruUTIno.UTIno,
                                  DatosEnfGruSECTORpu.SectorPU,
                                  DatosEnfGruSECTORpr.SectorPR,
                                  DatosEnfGruCOVIDsi.COVIDsi,
                                  DatosEnfGruCOVIDno.COVIDno
                                  FROM DatosSocio, DatosEnfGruUTIsi, 
                                  DatosEnfGruUTIno, DatosEnfGruSECTORpu,
                                  DatosEnfGruSECTORpr, DatosEnfGruCOVIDsi,
                                  DatosEnfGruCOVIDno
                                  WHERE 
                                  DatosSocio.CodigoRegion = DatosEnfGruUTIsi.RS_OCURRENCIA
                                  AND DatosSocio.CodigoRegion = DatosEnfGruUTIno.RS_OCURRENCIA
                                  AND DatosSocio.CodigoRegion = DatosEnfGruSECTORpu.RS_OCURRENCIA
                                  AND DatosSocio.CodigoRegion = DatosEnfGruSECTORpr.RS_OCURRENCIA            
                                  AND DatosSocio.CodigoRegion = DatosEnfGruCOVIDsi.RS_OCURRENCIA
                                  AND DatosSocio.CodigoRegion = DatosEnfGruCOVIDno.RS_OCURRENCIA
                                  ")
View(DatosRSPobUTIsiUTISECTORCOVID)
write.csv(x=DatosRSPobUTIsiUTISECTORCOVID, file = "SaludInvestiga2020/datos-socio-ocurrencias.csv")

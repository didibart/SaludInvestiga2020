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
#elijo todos los dato para cada municipio
dfOrdenIndi1 <- sqldf("SELECT * FROM dfIndi1 WHERE Region = 3 
                      ORDER BY Municipio")
View(dfOrdenIndi1)
#escribo el csv para luego copiar y pegar en el google calc
write.csv(dfOrdenIndi1,file="SaludInvestiga2020/region1.csv")

dfOrdenIndi2 <- sqldf("SELECT * FROM dfIndi2 WHERE Region = 3 
                      ORDER BY Municipio")
View(dfOrdenIndi2)
write.csv(dfOrdenIndi2,file="SaludInvestiga2020/region2.csv")

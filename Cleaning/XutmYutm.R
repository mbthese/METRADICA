
#Data 
library(readxl)

# recollecting UTM for Paracou individuals. FTH was already done. Using idTree
Metradica <-  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1rOl9sZl3A055GTajMcZxrJL_Ozx2bJfM_VEBdcpkD00/edit#gid=1375896520", range = "Metradica")

Paracou <- read_excel("../Metradica_Paracou/Document/Paracou_database20210830.xlsx")

Paracou <- Paracou %>%
  select(idTree, Xutm, Yutm)

joined_data <- left_join(Metradica, Paracou, by = "idTree", "Xutm"= "Xutm", "Yutm"="Yutm")

library(rio)
#write.csv(joined_data, file = "ParacouUTM.csv")

#Bafog
Bafog <- read_excel("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/Metradica_Bafog/Rawdata/Donnees_brutes/feuilles_terrain/202012_SelectionIndividus_V6.1Bafog.xlsx", sheet = "ListeTerrain")

Bafog <- Bafog %>%
  select(idTree, Xutm, Yutm)

joined_data2 <- left_join(Metradica, Bafog, by = "idTree", "Xutm"= "Xutm", "Yutm"="Yutm")

library(rio)
#write.csv(joined_data2, file = "BafogUTM.csv")


#Kaw 
Kaw <- read_excel("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/Metradica_Est/Document/20210527_KawGf_Trésor.xlsx")

Kaw <- Kaw %>%
  select(idTree, Xutm, Yutm)

joined_data3 <- left_join(Metradica, Kaw, by = "idTree", "Xutm"= "Xutm", "Yutm"="Yutm")
library(rio)
write.csv(joined_data3, file = "KAWUTM.csv")

#Kaw HP
KawHP <- read_excel("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/Metradica_Est/Document/GPS_HP.xlsx")
joined_data4 <- left_join(Metradica, KawHP, by = "Code", "Xutm"= "Xutm", "Yutm"="Yutm")
write.csv(joined_data4, file = "KAWHPUTM.csv")



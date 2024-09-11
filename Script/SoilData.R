# Soil analyses from excel files given by Gaelle Jaouen 06/2022
# Compilation in Gentry from DIADEMA, AMALIN & BRIDGE projects led by Christopher Baraloto and Claire Fortunel
# Paracou data from Vincent Freycon & Bruno Ferry with metadata in Paracou Data dictionary
# Other data from GUYAFOR database 
# Soong et al 2022 data as well

#libraries----
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

# Paracou -------
## data----

library(readxl)
Data1_Paracou <- read_excel("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/Divers/Sol/20220611PedoParacouCompletionSIG.xlsx")

Data2_all <- read_excel("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/Divers/Sol/202206_DonneesSolSyntheseKawParBaf.xlsx")

## summary-----

Summary_TF <- Data1_Paracou %>% 
  filter(TopoEnSIG =="Plateau") %>%
  summarise(CARBONEORG, AZOTETOTAL, C_N, P_OLSEN, AL_ÉCH_KCL, H_ÉCH_KCL, CA_ÉCH, MG_ÉCH, K_ÉCH, NA_ÉCH, MO, CEC) %>%
  mutate(Habitat = "Terra firme") %>%
  drop_na()
  
Summary_Bottomland <- Data1_Paracou %>% 
  filter(TopoEnSIG =="Bottomland") %>%
  summarise(CARBONEORG, AZOTETOTAL, C_N, P_OLSEN, AL_ÉCH_KCL, H_ÉCH_KCL, CA_ÉCH, MG_ÉCH, K_ÉCH, NA_ÉCH, MO, CEC) %>%
  mutate(Habitat = "Seasonally flooded forest") %>%
  drop_na()

Summary_Swamp <- Data1_Paracou %>% 
  filter(TopoEnSIG =="Swamp") %>%
  summarise(CARBONEORG, AZOTETOTAL, C_N, P_OLSEN, AL_ÉCH_KCL, H_ÉCH_KCL, CA_ÉCH, MG_ÉCH, K_ÉCH, NA_ÉCH, MO, CEC) %>%
  mutate(Habitat = "Seasonally flooded forest") %>%
  drop_na()

Soil_Paracou <- bind_rows(Summary_Bottomland, Summary_Swamp, Summary_TF)


## graphs-----

a <- ggplot(Soil_Paracou) +
 aes(x = Habitat, y = CARBONEORG, fill = Habitat) +
 geom_boxplot(adjust = 1L, scale = "area") +
 scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
 labs(y = "Organic Carbon (%)") +
 theme_minimal() +
 theme(axis.title.x=element_blank(),axis.text.x=element_blank())

b <- ggplot(Soil_Paracou) +
 aes(x = Habitat, y = AZOTETOTAL, fill = Habitat) +
 geom_boxplot(adjust = 1L, scale = "area") +
 scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
 labs(y = "Nitrogen (‰)") +
 theme_minimal() + 
 theme(axis.title.x=element_blank(),axis.text.x=element_blank())

c <- ggplot(Soil_Paracou) +
  aes(x = Habitat, y = C_N, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "C/N ratio") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())


e <-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = P_OLSEN, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Phosphorus (mg/kg) extracted \n using Olsen method") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

f <-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = AL_ÉCH_KCL, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Al exchangeable \n in KCl (cmol+/kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

g <-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = H_ÉCH_KCL, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "H exchangeable \n in KCl (cmol+/kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

h<-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = CA_ÉCH, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Ca exchangeable \n by Metson method (cmol+/kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +
  ylim(0, 0.75)

i<-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = MG_ÉCH, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Mg exchangeable \n by Metson method (cmol+ / kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

j<-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = K_ÉCH, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "K exchangeable \n by Metson method (cmol+/ kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

k<-ggplot(Soil_Paracou) +
  aes(x = Habitat, y = NA_ÉCH, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Na exchangeable \n by Metson method (cmol+ / kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

l<- ggplot(Soil_Paracou) +
  aes(x = Habitat, y = MO, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "Organic matter (%)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())

m<- ggplot(Soil_Paracou) +
  aes(x = Habitat, y = CEC, fill = Habitat) +
  geom_boxplot(adjust = 1L, scale = "area") +
  scale_fill_manual(values = list(`Seasonally flooded forest`= "#53CBE7", `Terra firme` = "#FFCB61")) +
  labs(y = "cation exchange capacity \n by method Metson (cmol+/ kg)") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank())



## Paracou grid ------

ggarrange(a, b, c, e, f, g, h, i, j, k, l, m,  # list of plots
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom") # legend position


## Figure SX: DBH influence

#same beginning of script as for the Figure S6: Spearman corerlation.
Data <- read_csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD_18042023.csv") %>% 
  dplyr::select(-Genus, -Species) %>%
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  filter(DBH != 0) %>%#one individual NA 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium", "SD", "DBH"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium", "SD", "DBH"), log) %>% 
  mutate(TLP = -TLP)%>%
  rename(Species = Name)


# Fit linear models
# Fit mixed-effect models using nlme::lme
model_gmin <- nlme::lme(Gmin ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_gmin <- predict(model_gmin)   #Add model fits to dataframe
model_tlp <- nlme::lme(TLP ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_tlp <- predict(model_tlp)  
model_lswc <- nlme::lme(LSWC ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_lswc <- predict(model_lswc)  
model_majvla <- nlme::lme(MajVLA ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_majvla <- predict(model_majvla)  
model_N <- nlme::lme(Nitrogen ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_N <- predict(model_N)  
model_C <- nlme::lme(Carbon ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_C <- predict(model_C)  
model_P <- nlme::lme(Phosphorous ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_P <- predict(model_P)  
model_K <- nlme::lme(Potassium ~ DBH, random = ~ 1 | Species, data = Data, na.action = na.omit, method = "ML")
Data$fit_K <- predict(model_K)  
# Handle NAs in SD before fitting the model
Data_SD <- Data %>% filter(!is.na(SD))
model_sd <- nlme::lme(SD ~ DBH, random = ~ 1 | Species, data = Data_SD, na.action = na.omit, method = "ML")
Data_SD$fit_sd <- predict(model_sd)

# Print summaries
summary(model_gmin)
summary(model_tlp) #yes, there is statistically significant negative relationship between DBH and TLP, but the relationship is weak. The very low R-squared value indicates that DBH explains only a very small fraction of the variance in TLP: 0.01465
summary(model_lswc)
summary(model_majvla) #yes, there a strong evidence that DBH affects MajVLA. However, the R-squared value of 11.85% indicates that it only explains a modest proportion of the variance in MajVLA: Adjusted R-squared:  0.1169 
summary(model_N)
summary(model_C)
summary(model_P) #yes, but very low R-squared value indicating that DBH explains only a very small fraction of the variance in P : Adjusted R-squared:  0.009545
summary(model_K) #same here: Adjusted R-squared:  0.0385 
summary(model_sd) #same here: 	Adjusted R-squared:  0.06477 


# Plotting with mixed-effect model lines
gmin.plot <- ggplot(Data, aes(x = DBH, y = Gmin, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "Gmin") +
  theme_minimal() +
  geom_line(aes(y=fit_gmin, color=Species), size=0.8) 

TLP.plot <- ggplot(Data, aes(x = DBH, y = TLP, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "TLP") +
  theme_minimal() +
  geom_line(aes(y=fit_tlp, color=Species), size=0.8) 

LSWC.plot <- ggplot(Data, aes(x = DBH, y = LSWC, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "LSWC") +
  theme_minimal() +
  geom_line(aes(y=fit_lswc, color=Species), size=0.8) 

MajVLA.plot <- ggplot(Data, aes(x = DBH, y = MajVLA, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "MajVLA") +
  theme_minimal() +
  geom_line(aes(y=fit_majvla, color=Species), size=0.8) 

N.plot <- ggplot(Data, aes(x = DBH, y = Nitrogen, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "Nitrogen") +
  theme_minimal() +
  geom_line(aes(y=fit_N, color=Species), size=0.8) 

C.plot <- ggplot(Data, aes(x = DBH, y = Carbon, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "Carbon") +
  theme_minimal() +
  geom_line(aes(y=fit_C, color=Species), size=0.8) 

Phos.plot <- ggplot(Data, aes(x = DBH, y = Phosphorous, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "Phosphorous") +
  theme_minimal() +
  geom_line(aes(y=fit_P, color=Species), size=0.8) 

Pot.plot <- ggplot(Data, aes(x = DBH, y = Potassium, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "Potassium") +
  theme_minimal() +
  geom_line(aes(y=fit_K, color=Species), size=0.8) 

SD.plot <- ggplot(Data_SD, aes(x = DBH, y = SD, color = Species)) +
  geom_point() +
  labs(x = "DBH", y = "SD") +
  theme_minimal() +
  geom_line(aes(y=fit_sd, color=Species), size=0.8) 



#arrange all

DBH_plot <- ggarrange(gmin.plot, TLP.plot, LSWC.plot, MajVLA.plot, SD.plot, N.plot, C.plot, Pot.plot, Phos.plot, common.legend = T, legend = "bottom")

DBH_plot

ggsave(filename = "DBH.jpeg", plot = DBH_plot, bg = "white", width = 10, height = 8, dpi = 600)


------------------
  Data <- read_csv("C:/Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/METRADICAproject/Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD.csv") %>% 
  dplyr::select(-Code, -Genus, -Species, Habitat, Type, -SD) %>%
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium"), log) %>% 
  mutate(TLP = -TLP)

  traits_spe_TF <- Data %>% 
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "TF") %>%
  dplyr::select(-Name, -Habitat, -Type, -Forest, -Plot, -TWI, -DBH) 

res_TF <- round(cor(traits_spe_TF, method = "spearman"), digits = 2)
testRes_TF = cor.mtest(traits_spe_TF, conf.level = 0.95, exact= FALSE, method="spearman")

testRes_TF$p %>% 
  kbl(caption ="", digits =3)  %>%
  kable_classic(full_width = F, html_font = "Cambria") 


library(dplyr)
Metradica_log_total <- read.csv("C://Users/marion.boisseaux/Dropbox/Mon PC (Jaboty20)/Documents/METRADICA/METRADICAproject/Dataset/OUTPUT_cleaning/ALL/Final_Metradica_OUT_30112022.csv") 

sub_data_TF <- Metradica_log_total %>%
  filter(Name %in% c("Dicorynia_guianensis", "Iryanthera_sagotiana", "Gustavia_hexapetala", "Licania_membranacea", "Poraqueiba_guianensis", "Virola_michelii")) %>% filter(Habitat == "TF")

sub_data_SF <- Metradica_log_total %>%
  filter(Name %in% c("Eschweilera_coriacea","Eperua_falcata", "Iryanthera_hostmannii", "Laetia_procera", "Protium_opacum subsp. rabelianum", "Pterocarpus_officinalis", "Symphonia_globulifera", "Virola_surinamensis", "Carapa_surinamensis")) %>% filter(Habitat == "BF")

sub_data_G <-  Metradica_log_total %>%
  filter(Name %in% c("Bocoa_prouacensis", "Conceveiba_guianensis", "Jacaranda_copaia subsp. copaia", "Hymenopus_heteromorphus", "Protium_stevensonii", "Tachigali_melinonii"))

sub_data <- bind_rows(sub_data_TF, sub_data_SF, sub_data_G)

sub_data <- sub_data %>% mutate(Type= ifelse(Name == 'Virola_michelii','TF',Type))

sub_data <- sub_data %>% mutate(Type= ifelse(Name == 'Eschweilera_coriacea','BF',Type))

Metradica_log <- sub_data #552 indv-----------------
  
#########################################################################################
## R script related to the multitrait coordiantion along the topographic wetness index ##
#########################################################################################

library(devtools)
library(ggfortify)
library(ggplot2)


###########################################
# Data preparation and log-transformation #
###########################################

Metradica_log <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD.csv") %>% 
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  relocate(SD, .after = MajVLA) %>%
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), log) %>% 
  mutate(TLP = -TLP) 

############################
#Construction of golbal PCA#
############################
Data_PCA <- Metradica_log

Data_PCA$Type <-  dplyr::recode(Data_PCA$Type, BF='SF Specialist', TF= 'TF Specialist')
Data_PCA$Habitat <- dplyr::recode(Data_PCA$Habitat, BF='Seasonally flooded', TF= 'Terra firme')

PCA_type <- autoplot(princomp(~ Gmin + TLP + LSWC + MajVLA + Potassium + Phosphorous + Carbon + Nitrogen, data = Data_PCA, cor = T), #  If TRUE, the data will be centered and scaled before the analysis.
              data = Data_PCA, colour = "Type", alpha = 0.4, size = 2, shape = "Type",
              loadings = T, loadings.label = T, loadings.label.repel = T, 
              loadings.label.colour = 'black', loadings.colour = 'black',
              loadings.label.size = 4) +
  geom_hline(aes(yintercept = 0), col = 'black', linetype = "dotted") +
  geom_vline(aes(xintercept = 0), col = 'black', linetype = "dotted") +
  theme_classic(base_size = 10) +
  # scale_y_reverse() +
  scale_color_manual("Species' preferences", values = c("#009E72", "#56B4E9", "#E79F02")) +
  scale_shape_discrete("Species' preferences") +
  stat_ellipse(aes(col = Type), level = 0.85, size = 1.2)+
  guides(color = guide_legend(title = "Species' preferences", 
                              title.position = "top", 
                              nrow = 2, 
                              override.aes = list(size = 3)), 
         shape = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "bottom")


PCA_collect <- autoplot(princomp(~ Gmin + TLP + LSWC + MajVLA + Potassium + Phosphorous + Carbon + Nitrogen, data = Data_PCA, cor = T), #  If TRUE, the data will be centered and scaled before the analysis.
                     data = Data_PCA, colour = "Habitat", alpha = 0.4, size = 2, shape = "Habitat",
                     loadings = T, loadings.label = T, loadings.label.repel = T, 
                     loadings.label.colour = 'black', loadings.colour = 'black',
                     loadings.label.size = 4) +
  geom_hline(aes(yintercept = 0), col = 'black', linetype = "dotted") +
  geom_vline(aes(xintercept = 0), col = 'black', linetype = "dotted") +
  theme_classic(base_size = 10) +
  # scale_y_reverse() +
  scale_color_manual("Habitat of collect", values = c("#56B4E9", "#E79F02", "")) +
  scale_shape_discrete("Habitat of collect") +
  stat_ellipse(aes(col = Habitat), level = 0.85, size = 1.2)+
  guides(color = guide_legend(title.position = "top", nrow = 3)) +
  theme(legend.position ="bottom")


#arrange plots together

plot_global_pca <- ggpubr::ggarrange(PCA_type, PCA_collect, labels = c("A", "B"), ncol = 2, nrow = 1, common.legend = FALSE, heights = c(4, 4))
ggsave(filename = "PCA_full_Habitat_pref_collect_imputed.png", plot = plot_global_pca, bg = "white", width = 7, height = 4, dpi = 600)

###########################################
# Dividing the range of TWI into classes#
###########################################
#Metradica_log <- Metradica_log %>% filter(Type != "Generalist") for specialists
Metradica_log <- Metradica_log %>% filter(Type == "Generalist") #for generalists
nb_class <- 8 #7 for specialists and then merge classes to get enough individuals per class
class_size <- (max(Metradica_log$TWI)-min(Metradica_log$TWI))/nb_class #each class have the same range of TWI
data <- as.data.frame(seq(1:nb_class))
colnames(data)[1] <- "class"
data$min <- c() #min TWI of the class
data$max <- c() #max TWI of the class
data$mean <- c() #mean TWI of the class
Metradica_log$class_TWI <- as.numeric(NA)

for (i in 1:nb_class){ #attribute the individuals of the dataset to each of the 8 classes
  
  data$min[i] <- min(Metradica_log$TWI) + (i-1) * class_size
  data$max[i] <- min(Metradica_log$TWI) +i * class_size
  data$mean[i] <- (data$max[i] + data$min[i])/2
  
  Metradica_log$class_TWI[which(Metradica_log$TWI <= data$max[i] & Metradica_log$TWI > data$min[i]) ] <- i
  
}

#attribute the individuals with the lowest TWI to the first class
Metradica_log$class_TWI[which(Metradica_log$TWI == data$min[1])]  <- 1

#merge TWI classes together to have approximately the same individuals per class

#Metradica_log$class_TWI[which(Metradica_log$class_TWI == 7)]  <- 6 #for specialists

Metradica_log$class_TWI[which(Metradica_log$class_TWI == 7)]  <- 6 #for generalists
Metradica_log$class_TWI[which(Metradica_log$class_TWI == 8)]  <- 6 #for generalists

table(Metradica_log$class_TWI) #number of individuals per class
table(is.na(Metradica_log$class_TWI)) #verify all individuals have a class

#in the data of classes, merge categories
#data$max[which(data$class == 6 )] <- data$max[which(data$class == 7)] #for specialists
data$max[which(data$class == 6 )] <- data$max[which(data$class == 8)] #for generalists
data$mean[which(data$class == 6 )] <- (data$max[6] + data$min[6])/2
data <- slice(data, 1:(n() - 2)) 
data

###################################################################################################
#Construction of the PCAs for each TWI class level and calculating the observed trait correlation
###################################################################################################

# Function to calculate the observed index values per community and the standardized index values by the associated null community

TI_index <- function(NbClass, Data, Data_TWI){ 
  #data that has a column named TWI as calculated before
  essai <- data.frame(TWI_class = numeric(), range_i = numeric(), sd_i = numeric()) #create an empty dataframe with columns

  for (i in 1:NbClass){  
    
    #calculate observed index
    
    Data_i <- Data %>% filter(class_TWI == i) #filter the data set for the first class of TWI
    
    res.pca_i <- PCA(Data_i %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
    
    range_i <- res.pca_i$eig[1,1] - res.pca_i$eig[8,1] #observed range of the eigen values
    
    sd_i <- sd(res.pca_i$eig[,1]) #observed standard deviation of the eigen values
    
    class_i <- c()
    class_i_sd <- c()
    indv_rich_i <- length(levels(as.factor(Data_i$Code)))
    indv_rich_total <- length(levels(as.factor(Data$Code)))
    for (i in 1:1000){  #sampling 1,000 random communities from the whole individual pool
      indv_list <- sample(levels(as.factor(Data$Code)), size = indv_rich_i, replace =FALSE) # sampling random individuals from the whole dataset, same number of individuals as the dataset of class TWI 1 
      Data_i_abon <- c() 
      for (s in 1:indv_rich_i){
        
        Data_i_abon <- rbind(Data_i_abon, 
                             Data[sample(which(Data$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 1 and getting all the characteristics (i.e. traits) for these individuals
      }
      
      PCA_comm_i <- PCA(Data_i_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
      class_i <- c(class_i, PCA_comm_i$eig[1,1] - PCA_comm_i$eig[8,1])
      class_i_sd <- c(class_i_sd , sd(PCA_comm_i$eig[,1]))
    }
    
      #Calculating the multivariate covariation  between traits index, standardized by the effect size since the comparison between groups is likely to be biased by the number of individuals used. 
    ITI_i <- (range_i - mean(class_i)) / sd(class_i)
    ITI_i_sd <- (sd_i - mean(class_i_sd))/sd(class_i_sd)
    
    essai_i <- data.frame(range_i, sd_i, ITI_i, ITI_i_sd)
    essai <- rbind(essai, essai_i) 
    
   
  }

  #Plot ranges along TWI
  Data_TWI <- cbind(Data_TWI, essai)
  A <- ggplot(Data_TWI) +
    aes(x = mean, y = range_i) +
    geom_point(shape = "circle", size = 4, colour = "#112446") +
    theme_minimal()+
    ylab("Ranges")+
    xlab("")+
    theme(axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16))
  
  #Plot SD along TWI
  B <- ggplot(Data_TWI) +
    aes(x = mean, y = sd_i) +
    geom_point(shape = "circle", size = 4, colour = "#112446") +
    theme_minimal()+
    ylab("sd")+
    xlab("")+
    theme(axis.text = element_text(size = 14),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16))
  
  
  #Plot TI (ranges standardized by effect size) along TWI 
  C <- ggplot(Data_TWI) +
    aes(x = mean, y = ITI_i) +
    geom_point(shape = "circle", size = 5, aes(color = mean)) +
    scale_color_gradient(low = "#E79F02", high = "#56B4E9") +
    geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
    ylab(expression(atop("Trait integration index", italic("range")))) + #atop creates a line break
    xlab("Topographic wetness index") +
    theme(axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12)) +
    theme_minimal(base_size = 12) +
    guides(color = "none") #remove legend
  
  
  #Plot TI_sd along TWI
  D <- ggplot(Data_TWI) +
    aes(x = mean, y = ITI_i_sd) +
    geom_point(shape = "circle", size = 5, aes(color = mean)) +
    scale_color_gradient(low = "#E79F02", high = "#56B4E9") +
    geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
    ylab(expression(atop("Trait integration index", italic("standard deviation")))) +
    xlab("Topographic wetness index") +
    theme(axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.title.x = element_text(size = 12)) +
    theme_minimal(base_size = 12)+
    guides(color = "none")
  
  
  #arrange plots together
  E <- ggpubr::ggarrange(A, B, C, D, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2, common.legend = TRUE)
  plot_publi <- ggpubr::ggarrange(C, D, labels = c("A", "B"), ncol = 2, nrow = 1, common.legend = TRUE)
  
  
  
  #save plot
  ggsave(filename = "Multivariate covariation_6_class_specialist.png", plot = plot_publi, bg = "white", width = 7, height = 4, dpi = 600)
    ggsave(filename = "Multivariate covariation_6_class_4plot_specialist.png", plot = E, bg = "white", width = 7, height = 4, dpi = 600)
  return(list(essai, A, B, C, D, E, plot_publi))
  
 }


#1------------

Data_1 <- Metradica_log %>% filter(class_TWI == 1) #filter the data set for the first class of TWI
res.pca_1 <- PCA(Data_1 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_1$eig #look at the eigen values 
range_1 <- res.pca_1$eig[1,1] - res.pca_1$eig[8,1] #observed range of the eigen values
sd_1 <- sd(res.pca_1$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_1, addlabels = TRUE, ylim = c(0, 50)) #visualization of the scree plot

#########################################################################
#Construction of the null associated community for each TWI class level 
#########################################################################

class_1 <- c()
class_1_sd <- c()
indv_rich_1 <- length(levels(as.factor(Data_1$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole individual pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_1, replace =FALSE) # sampling random individuals from the whole dataset, same number of individuals as the dataset of class TWI 1 
  Data_1_abon <- c() 
  for (s in 1:indv_rich_1){
      
      Data_1_abon <- rbind(Data_1_abon, 
                           Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 1 and getting all the characteristics (i.e. traits) for these individuals
    }

  PCA_comm_1 <- PCA(Data_1_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_1 <- c(class_1, PCA_comm_1$eig[1,1] - PCA_comm_1$eig[8,1])
  class_1_sd <- c(class_1_sd , sd(PCA_comm_1$eig[,1]))
}

plot_1 <- hist(class_1)

#Calculating the multivariate covariation  between traits index, standardized by the effect size since the comparison between groups is likely to be biased by the number of individuals used. 
ITI_1 <- (range_1 - mean(class_1)) / sd(class_1)
ITI_1_sd <- (sd_1 - mean(class_1_sd))/sd(class_1_sd)

#2--------------------------

Data_2 <- Metradica_log %>% filter(class_TWI == 2) #filter the data set for the first class of TWI
res.pca_2 <- PCA(Data_2 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_2$eig #look at the eigen values 
range_2 <- res.pca_2$eig[1,1] - res.pca_2$eig[8,1] #observed range of the eigen values
sd_2 <- sd(res.pca_2$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_2, addlabels = TRUE, ylim = c(0, 50)) #visualization of the scree plot

#null community associated to the Data_2
class_2 <- c()
class_2_sd <- c()
indv_rich_2 <- length(levels(as.factor(Data_2$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_2, replace =FALSE) # first null community for class TWI 2 sampled from the whole dataset
  Data_2_abon <- c()
  for (s in 1:indv_rich_2){
      
      Data_2_abon <- rbind(Data_2_abon, 
                           Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 2 
    }
  
  PCA_comm_2 <- PCA(Data_2_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_2 <- c(class_2, PCA_comm_2$eig[1,1] - PCA_comm_2$eig[8,1])
  class_2_sd <- c(class_2_sd , sd(PCA_comm_2$eig[,1]))
}

plot_2 <- hist(class_2)

#Calculating the multivariate covariation  between traits index, standardized by the effect size since the comparison between groups is likely to be biased by the number of individuals used. 
ITI_2 <- (range_2 - mean(class_2)) / sd(class_2)
ITI_2_sd <- (sd_2 - mean(class_2_sd))/sd(class_2_sd)

#3--------------

Data_3 <- Metradica_log %>% filter(class_TWI == 3) #filter the data set for the first class of TWI
res.pca_3 <- PCA(Data_3 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_3$eig #look at the eigen values 
range_3 <- res.pca_3$eig[1,1] - res.pca_3$eig[8,1] #observed range of the eigen values
sd_3 <- sd(res.pca_3$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_3, addlabels = TRUE, ylim = c(0, 50)) #visualization of the scree plot

#null community associated to the Data_3
class_3 <- c()
class_3_sd <- c()
indv_rich_3 <- length(levels(as.factor(Data_3$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_3, replace =FALSE) # first null community for class TWI 3 sampled from the whole dataset
  Data_3_abon <- c()
  for (s in 1:indv_rich_3){
    
    Data_3_abon <- rbind(Data_3_abon, 
                         Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 3 
  }
  
  PCA_comm_3 <- PCA(Data_3_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_3 <- c(class_3, PCA_comm_3$eig[1,1] - PCA_comm_3$eig[8,1])
  class_3_sd <- c(class_3_sd , sd(PCA_comm_3$eig[,1]))
}

plot_3 <- hist(class_3)

ITI_3 <- (range_3 - mean(class_3))/sd(class_3)
ITI_3_sd <- (sd_3 - mean(class_3_sd))/sd(class_3_sd)

#4------------
Data_4 <- Metradica_log %>% filter(class_TWI == 4) #filter the data set for the first class of TWI
res.pca_4 <- PCA(Data_4 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_4$eig #look at the eigen values 
range_4 <- res.pca_4$eig[1,1] - res.pca_4$eig[8,1] #observed range of the eigen values
sd_4 <- sd(res.pca_4$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_4, addlabels = TRUE, ylim = c(0, 50)) #visualization of the scree plot

#null community associated to the Data_4
class_4 <- c()
class_4_sd <- c()
indv_rich_4 <- length(levels(as.factor(Data_4$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_4, replace =FALSE) # first null community for class TWI 4 sampled from the whole dataset
  Data_4_abon <- c()
  for (s in 1:indv_rich_4){
    
    Data_4_abon <- rbind(Data_4_abon, 
                         Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 4 
  }
  
  PCA_comm_4 <- PCA(Data_4_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_4 <- c(class_4, PCA_comm_4$eig[1,1] - PCA_comm_4$eig[8,1])
  class_4_sd <- c(class_4_sd , sd(PCA_comm_4$eig[,1]))
}

plot_4 <- hist(class_4)

ITI_4 <- (range_4 - mean(class_4))/sd(class_4)
ITI_4_sd <- (sd_4 - mean(class_4_sd))/sd(class_4_sd)

#5----------
Data_5 <- Metradica_log %>% filter(class_TWI == 5) #filter the data set for the first class of TWI
res.pca_5 <- PCA(Data_5 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_5$eig #look at the eigen values 
range_5 <- res.pca_5$eig[1,1] - res.pca_5$eig[8,1] #observed range of the eigen values
sd_5 <- sd(res.pca_5$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_5, addlabels = TRUE, ylim = c(0, 50)) #visualization of the scree plot

#null community associated to the Data_5
class_5 <- c()
class_5_sd <- c()
indv_rich_5 <- length(levels(as.factor(Data_5$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_5, replace =FALSE) # first null community for class TWI 5 sampled from the whole dataset
  Data_5_abon <- c()
  for (s in 1:indv_rich_5){
    
    Data_5_abon <- rbind(Data_5_abon, 
                         Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 5 
  }
  
  PCA_comm_5 <- PCA(Data_5_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_5 <- c(class_5, PCA_comm_5$eig[1,1] - PCA_comm_5$eig[8,1])
  class_5_sd <- c(class_5_sd , sd(PCA_comm_5$eig[,1]))
}

plot_5 <- hist(class_5)

ITI_5 <- (range_5 - mean(class_5))/sd(class_5)
ITI_5_sd <- (sd_5 - mean(class_5_sd))/sd(class_5_sd)
#6----------------

Data_6 <- Metradica_log %>% filter(class_TWI == 6) #filter the data set for the first class of TWI
res.pca_6 <- PCA(Data_6 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_6$eig #look at the eigen values 
range_6 <- res.pca_6$eig[1,1] - res.pca_6$eig[8,1] #observed range of the eigen values
sd_6 <- sd(res.pca_6$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_6, addlabels = TRUE, ylim = c(0, 60)) #visualization of the scree plot

#null community associated to the Data_6
class_6 <- c()
class_6_sd <- c()
indv_rich_6 <- length(levels(as.factor(Data_6$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_6, replace =FALSE) # first null community for class TWI 6 sampled from the whole dataset
  Data_6_abon <- c()
  for (s in 1:indv_rich_6){
    
    Data_6_abon <- rbind(Data_6_abon, 
                         Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 6 
  }
  
  PCA_comm_6 <- PCA(Data_6_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_6 <- c(class_6, PCA_comm_6$eig[1,1] - PCA_comm_6$eig[8,1])
  class_6_sd <- c(class_6_sd , sd(PCA_comm_6$eig[,1]))
}

plot_6 <- hist(class_6)

ITI_6 <- (range_6 - mean(class_6))/sd(class_6)
ITI_6_sd <- (sd_6 - mean(class_6_sd))/sd(class_6_sd)

#####################
#Plots
#####################
 #data for generalists
data$ranges <- c(range_1, range_2, range_3, range_4, range_5, range_6)
data$sd <- c(sd_1, sd_2, sd_3, sd_4, sd_5, sd_6)
data$ITI <- c(ITI_1, ITI_2, ITI_3, ITI_4, ITI_5, ITI_6)
data$ITI_sd <- c(ITI_1_sd, ITI_2_sd, ITI_3_sd, ITI_4_sd, ITI_5_sd, ITI_6_sd)

#data for specialists
data_s <- data
data_s$ranges <- c(range_1, range_2, range_3, range_4, range_5, range_6)
data_s$sd <- c(sd_1, sd_2, sd_3, sd_4, sd_5, sd_6)
data_s$ITI <- c(ITI_1, ITI_2, ITI_3, ITI_4, ITI_5, ITI_6)
data_s$ITI_sd <- c(ITI_1_sd, ITI_2_sd, ITI_3_sd, ITI_4_sd, ITI_5_sd, ITI_6_sd)

#plot generalists

title <- "Generalist"

# Create a text grob
tgrob <- text_grob(title,size = 16)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))


#Plot TI (ranges standardized by effect size) along TWI 
A <- ggplot(data) +
  aes(x = mean, y = ITI) +
  geom_point(shape = "circle", size = 5, color = "#009E72") +
  geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  ylab(expression(atop("Trait integration index", italic("range")))) + #atop creates a line break
  xlab("Topographic wetness index") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  theme_minimal(base_size = 12) +
  guides(color = "none") #remove legend


#Plot TI_sd along TWI
B <- ggplot(data) +
  aes(x = mean, y = ITI_sd) +
  geom_point(shape = "circle", size = 5, color = "#009E72") +
  geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  ylab(expression(atop("Trait integration index", italic("standard deviation")))) +
  xlab("Topographic wetness index") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  theme_minimal(base_size = 12)+
  guides(color = "none")

#plot specialist
title_s <- "Specialist"

# Create a text grob
tgrob_s <- text_grob(title_s,size = 16)
# Draw the text
plot_s <- as_ggplot(tgrob_s) + theme(plot.margin = margin(0,3,0,0, "cm"))


#Plot TI (ranges standardized by effect size) along TWI 
C <- ggplot(data_s) +
  aes(x = mean, y = ITI) +
  geom_point(shape = "circle", size = 5, aes(color = mean)) +
  scale_color_gradient(low = "#E79F02", high = "#56B4E9") +
  geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  ylab(expression(atop("Trait integration index", italic("range")))) + #atop creates a line break
  xlab("Topographic wetness index") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  theme_minimal(base_size = 12) +
  guides(color = "none") #remove legend


#Plot TI_sd along TWI
D <- ggplot(data_s) +
  aes(x = mean, y = ITI_sd) +
  geom_point(shape = "circle", size = 5, aes(color = mean)) +
  scale_color_gradient(low = "#E79F02", high = "#56B4E9") +
  geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  ylab(expression(atop("Trait integration index", italic("standard deviation")))) +
  xlab("Topographic wetness index") +
  theme(axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12)) +
  theme_minimal(base_size = 12)+
  guides(color = "none")

#arrange plots together
plot_generalists <- ggpubr::ggarrange(plot_0, NULL, A, B, labels = c("", "", "A", "B"), ncol = 2, nrow = 2, common.legend = TRUE, heights = c(1,5))
plot_generalists

plot_specialists <- ggpubr::ggarrange(plot_s, NULL, C, D, labels = c("", "", "C", "D"), ncol = 2, nrow = 2, common.legend = TRUE, heights = c(1,5))
plot_specialists

plot_publi <- ggpubr::ggarrange(plot_generalists, plot_specialists, ncol = 1, nrow = 2)
plot_publi

#save plot
ggsave(filename = "Multivariate covariation_6 class.png", plot = plot_publi, bg = "white", width = 8, height = 7, dpi = 600)

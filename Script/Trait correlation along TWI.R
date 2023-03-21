#########################################################################################
## R script related to the multitrait coordiantion along the topographic wetness index ##
#########################################################################################

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

#####################
# Permanova##########
#####################

### PERMANOVA, (permutational multivariate ANOVA), is a non-parametric alternative to MANOVA, or multivariate ANOVA test. It is appropriate with multiple sets of variables that do not meet the assumptions of MANOVA, namely multivariate normality.

#Null hypothesis: Groups do not differ/ are equivalent in spread or position multivariate space.

data_PCA <- Metradica_log[, c(1:17)][,-c(1,2,3,4,5,6,7,8,13)]

adonis_type<-adonis2(data_PCA~Metradica_log$Type, permutations = 999, method = "euclidean") 
adonis_habitat<-adonis2(data_PCA~Metradica_log$Habitat, permutations = 999, method = "euclidean") 

permanova <-adonis_type %>%
  kbl(caption="Permutational Multivariate Analysis of Variance (PERMANOVA)", digits = 3, booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down", full_width = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") #%>%
#save_kable(file = "Permanova_type.png")

permanova <-adonis_habitat %>%
  kbl(caption="Permutational Multivariate Analysis of Variance (PERMANOVA)", digits = 3, booktabs=TRUE) %>% 
  kable_styling(latex_options="scale_down", full_width = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") #%>%
#save_kable(file = "Permanova_type.png")

#P-value (0.001) *** is significant so our group types have different means.

#Process a pairwise analyse to detect which treatments are different from each other.

library(pairwiseAdonis)
library(kableExtra)
pairwise_adonis_type <- pairwise.adonis(data_PCA,Metradica_log$Type, sim.method = "euclidean")
pairwise_adonis_type_table <- pairwise_adonis_type %>%
  dplyr::select(-pairs) %>%
  dplyr::mutate(pairs = c("TF specialist vs SF specialist", "TF specialist vs generalist", "SF specialist vs generalist")) %>%
  kbl(caption = "A. Species' preferences") %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "pairwise_adonis_type.png", zoom = 10)

pairwise_adonis_habitat <- pairwise.adonis(data_PCA,Metradica_log$Habitat, sim.method = "euclidean")
pairwise_adonis_table_habitat <- pairwise_adonis_habitat %>%
  dplyr::select(-pairs) %>%
  dplyr::mutate(pairs = c("TF habitat vs SF habitat")) %>%
  kbl(caption = "B. Habitat of collect") %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "pairwise_adonis_habitat.png", zoom = 10)


###############
#Correlation###
###############

library(corrplot)

traits <- Data %>% #already log
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  dplyr::select(-Name, -Habitat, -Type, -Forest, -Plot, -TWI, -DBH) 

#to add significance
testRes = cor.mtest(traits, conf.level = 0.95)

#image output, have to run until last live dev.off() for it to work.
png(height=500, width=500, file="mycorr_noTWI_noSD_imputed.png", type = "cairo")

#create correlation matrix
cor_matrix <- traits %>%
  na.omit() %>%
  cor(method = "spearman")

#correlation plot with no rows and column labels and without colors in the diagonal
corrplot(cor_matrix, p.mat = testRes$p, method = 'circle',  #addCoef.col ='black', insig='blank',
         sig.level = 0.10, addrect = 2, diag = FALSE, tl.pos = FALSE)

# Add labels to the diagonal
text(rev(1:nrow(cor_matrix)), 1:nrow(cor_matrix), rev(colnames(cor_matrix)), 
     cex = 1.5, pos = 4, offset = -1.5, col= "red", ifelse(is.na(diag(cor_matrix)), NA, 1))

dev.off()

#significance
install.packages("Hmisc")
library("Hmisc")
res <- rcorr(as.matrix(traits), type = "spearman")
res

# Extract the correlation coefficients
library(kableExtra)
library(magick)

res$r %>%
  kbl(caption ="", digits =2)  %>%
  add_header_above(c("<span style='font-size: 20px; font-weight: bold;'>A. Pairwise table of the correlation coefficients</span>" = 9), escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria")  %>%
  save_kable("corr_coef.png")

img <- image_read("corr_coef.png")
img_resized <- image_scale(img, "800x600")

# Save the resized image
image_write(img_resized, "corr_coef_resized.png")

# Extract p-values
res$P%>%
  kbl(caption ="", digits =3)  %>%
  add_header_above(c("<span style='font-size: 20px; font-weight: bold;'>B. Pairwise table of the associated p-values</span>" = 9), escape = FALSE) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable("corr_pvalue.png")

img <- image_read("corr_pvalue.png")
img_resized <- image_scale(img, "800x600")

# Save the resized image
image_write(img_resized, "corr_pvalue_resized.png")

###########################################
# Dividing the range of TWI into classes#
###########################################

nb_class <- 8
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

#merge TWI class 6,7 and 8 together to have approximately the same individuals per class
#class 6,7 & 8 are the highest classes for seasonally flooded soils
#class 1, 2 & 3 are the highest classes for terra firme soils
Metradica_log$class_TWI[which(Metradica_log$class_TWI == 5)]  <- 4
Metradica_log$class_TWI[which(Metradica_log$class_TWI == 9)]  <- 7
Metradica_log$class_TWI[which(Metradica_log$class_TWI == 10)]  <- 7
table(Metradica_log$class_TWI) #number of individuals per class
table(is.na(Metradica_log$class_TWI)) #verify all individuals have a class

#in the data of classes, merge the 6th, 7th and 8th categories
data$max[which(data$class == 4 )] <- data$max[which(data$class == 5)]
data$mean[which(data$class == 4 )] <- (data$max[4] + data$min[4])/2
data <- slice(data, 1:(n() - 1)) 


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

##

Data_9 <- Metradica_log %>% filter(class_TWI == 9) #filter the data set for the first class of TWI
res.pca_9 <- PCA(Data_9 %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE) #PCA on trait values except SD, that are scaled to unit variance
res.pca_9$eig #look at the eigen values 
range_9 <- res.pca_9$eig[1,1] - res.pca_9$eig[8,1] #observed range of the eigen values
sd_9 <- sd(res.pca_9$eig[,1]) #observed standard deviation of the eigen values
fviz_eig(res.pca_9, addlabels = TRUE, ylim = c(0, 90)) #visualization of the scree plot

#null community associated to the Data_9
class_9 <- c()
class_9_sd <- c()
indv_rich_9 <- length(levels(as.factor(Data_9$Code)))
indv_rich_total <- length(levels(as.factor(Metradica_log$Code)))
for (i in 1:1000){  #sampling 1,000 random communities from the whole indivudal pool
  indv_list <- sample(levels(as.factor(Metradica_log$Code)), size = indv_rich_9, replace =FALSE) # first null community for class TWI 9 sampled from the whole dataset
  Data_9_abon <- c()
  for (s in 1:indv_rich_9){
    
    Data_9_abon <- rbind(Data_9_abon, 
                         Metradica_log[sample(which(Metradica_log$Code == indv_list[s]), 1),]) #constraining the null community to have the same abundance/same number of individuals as the community of class 9 
  }
  
  PCA_comm_9 <- PCA(Data_9_abon %>% dplyr::select(-Plot, -Forest,-Genus, -Species, -Name, -Type, -Habitat, -TWI, -DBH, -SD), scale.unit = TRUE, graph = FALSE)
  class_9 <- c(class_9, PCA_comm_9$eig[1,1] - PCA_comm_9$eig[8,1])
  class_9_sd <- c(class_9_sd , sd(PCA_comm_9$eig[,1]))
}

plot_9 <- hist(class_9)

ITI_9 <- (range_9 - mean(class_9))/sd(class_9)
ITI_9_sd <- (sd_9 - mean(class_9_sd))/sd(class_9_sd)
#####################
#Plots
#####################
 #data for plot
data$ranges <- c(range_1, range_2, range_3, range_4, range_5, range_6, range_7, range_8, range_9)
data$sd <- c(sd_1, sd_2, sd_3, sd_4, sd_5, sd_6, sd_7, sd_8, sd_9)
data$ITI <- c(ITI_1, ITI_2, ITI_3, ITI_4, ITI_5, ITI_6, ITI_7, ITI_8, ITI_9)
data$ITI_sd <- c(ITI_1_sd, ITI_2_sd, ITI_3_sd, ITI_4_sd, ITI_5_sd, ITI_6_sd, ITI_7_sd, ITI_8_sd, ITI_9_sd)

#Plot ranges along TWI
A <- ggplot(data) +
  aes(x = mean, y = ranges) +
  geom_point(shape = "circle", size = 4, colour = "#112446") +
  theme_minimal()+
  ylab("Ranges")+
  xlab("")+
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

#Plot SD along TWI
B <- ggplot(data) +
  aes(x = mean, y = sd) +
  geom_point(shape = "circle", size = 4, colour = "#112446") +
  theme_minimal()+
  ylab("sd")+
  xlab("")+
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))


#Plot TI (ranges standardized by effect size) along TWI 
C <- ggplot(data) +
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
D <- ggplot(data) +
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
plot_publi <- ggpubr::ggarrange(A, B, C, D, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2, common.legend = TRUE)



#save plot
ggsave(filename = "Multivariate covariation_9 class.png", plot = plot_publi, bg = "white", width = 7, height = 4, dpi = 600)
ggsave(filename = "Multivariate covariation.png", plot = C, bg = "white", width = 7, height = 4, dpi = 600)

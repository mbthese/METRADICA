##############################################
#Variance partitioning for all individuals 
##############################################

#library used
library(lme4) #to fit the model and do the variance partitioning
library(dplyr) #to pipe
library(ggplot2) #to plot

#data used (log on all traits)
Data <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD.csv") %>% 
  dplyr::select(-C, -N) %>%
  rename(Potassium = K, 
         Phosphorous = P) %>% 
  relocate(SD, .after = MajVLA) %>%
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous", "Potassium", "TWI"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous", "Potassium", "TWI"), log) %>% 
  mutate(TLP = -TLP)

Data$Forest <- as.factor(Data$Forest)

#function for variance partitioning
Var_par <- function(Trait, Mydata){
  
  #Rename the trait column
  colnames(Mydata)[which(colnames(Mydata) == Trait)] <- "Trait"  
  
  
  # Fit the linear mixed-effects model
  model <- lme4::lmer(Trait ~ TWI + Forest + (1 | Name), data = Mydata)
  
  #Fit the associated null model with random intercept on species
  null_model <- lme4::lmer(Trait~ (1 | Name), data = Mydata)
  
  # Extract the variance components 
  Var_components <- lme4::VarCorr(model)
  var_sp <- Var_components$Name[1] #for the random effects - species
  var_indv <- attr(Var_components, "sc")^2 #model residual, also known as the intraspecific residual variance (linked to the individual but also error measures). I square because it returns the residual standard deviation and I want the residual variance.
  
  # Obtain the variance of the random effect in the null model.
  random_variance_null <- lme4::VarCorr(null_model)$Name[1]
  
  # Obtain the residual variance
  residual_variance_null <- attr(lme4::VarCorr(null_model), "sc")^2
  
  # Obtain the total variance of the null model
  v_0 <- random_variance_null + residual_variance_null
  
  # Calculate the variance component linked to the env't by substracting the residual variance of the model (var_indv) and the variance explained by the random factor, the species (var_sp), from the variance of the null model
  var_envt <- v_0 - var_indv - var_sp
  
  Traits <- c(Trait, Trait, Trait)
  Levels <- c("Environment", "Species", "Individual")
  Variances <- c(var_envt/v_0, var_sp/v_0, var_indv/v_0)
  
  
  return(data.frame(Traits, Levels, Variances))
}

# Loop to calculate the variance partitioning for all traits
vars <- c()

for (i in colnames(Data)[9:15]){
  
  vars <- bind_rows(vars, Var_par(Trait = i, Mydata = Data))
  
}

#plot the results

vars$Levels <- as.factor(vars$Levels)

variance_plot<- vars %>%  
  mutate(Levels = factor(Levels, levels=c("Environment", "Species", "Individual"))) %>%
  mutate(Traits = dplyr::recode(Traits, "Gmin" = "g[min]")) %>% #recode gmin
  ggplot(aes(fill=Levels, y=Variances, x=Traits)) + 
  geom_bar(position="stack", stat="identity") +
  theme_minimal(base_size = 22) +
  ylab("") + xlab("")+
  theme(legend.text = element_text(face = "italic"),legend.position = "bottom") +
  scale_fill_manual("", values=c("#BBBBBB", "#CCBC44", "#029A88"),
                    breaks=c("Individual", "Species", "Environment"),
                    labels=c("Individual", "Species", "Environment"))  +
  coord_flip() +
  scale_x_discrete(labels = scales::label_parse())

variance_plot

#save plot
ggsave(filename = "Variance_partitioning.png", plot = variance_plot, bg = "white", width = 10, height = 8, dpi = 600)


##############################################
#function for plotting residuals distribution 
##############################################


res_plot <- function(Trait, Mydata){
  
  #Rename the trait column
  colnames(Mydata)[which(colnames(Mydata) == Trait)] <- "Trait"  
  
  
  # Fit the linear mixed-effects model
  model <- lmer(Trait ~ TWI + Forest + (1 | Name), data = Mydata)
  plot <- qqnorm(resid(model), main= Trait)
  
  
  return(plot)
}

# Loop to plot residuals of the model for all traits

plotlist <- list()

for (i in colnames(Data)[9:17]){
  
  
  plotlist[[i]] <- local({
    
    i <- i
    
    res_plot(i, Data)
  })
  
  
}

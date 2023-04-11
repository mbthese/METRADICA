##############################################
#Variance partitioning for all individuals 
##############################################

#library used
library(nlme) #to fit the model and do the variance partitioning with ML method
library(lme4) #for the VarCorr function
library(dplyr) #to pipe
library(ggplot2) #to plot
library(kableExtra) #to build nice tables

#data used (log on all traits)
Data <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD.csv") %>% 
  rename(Carbon = C,
         Nitrogen = N,
         Potassium = K, 
         Phosphorous = P) %>% 
  relocate(SD, .after = MajVLA) %>%
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Carbon", "Nitrogen", "Phosphorous", "Potassium", "TWI"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD",  "Carbon", "Nitrogen", "Phosphorous", "Potassium", "TWI"), log) %>% 
  mutate(TLP = -TLP)

Data$Forest <- as.factor(Data$Forest)

#function for variance partitioning
Var_par <- function(Trait, Mydata){

  #Rename the trait column
  colnames(Mydata)[which(colnames(Mydata) == Trait)] <- "Trait"  
  
  # Fit the linear mixed-effects model
  model <- nlme::lme(Trait ~ TWI + Forest,  random=~1|Name, data = Mydata, na.action = na.omit, method = "ML")
  
  #Fit the associated null model with random intercept on species
  null_model <- nlme::lme(Trait~ 1,  random=~1|Name, data = Mydata, na.action = na.omit, method = "ML")
  
  # Extract the variance components 
  Var_components <-lme4::VarCorr(model)
  var_sp <- as.numeric(Var_components[1,1]) #for the random effects - species
  var_indv <- as.numeric(Var_components[2,1]) #model residual, also known as the intraspecific residual variance (linked to the individual but also error measures).
  
  # Obtain the variance of the random effect in the null model.
  random_variance_null <- as.numeric(lme4::VarCorr(null_model)[1,1])
  
  # Obtain the residual variance
  residual_variance_null <- as.numeric(lme4::VarCorr(null_model)[2,1])
  
  # Obtain the total variance of the null model
  v_0 <- random_variance_null + residual_variance_null
  
  # Calculate the variance component linked to the env't by substracting the residual variance of the model (var_indv) and the variance explained by the random factor, the species (var_sp), from the variance of the null model
 
  # Variance partitioning
  var_sp <- round(100 * var_sp / v_0)
  var_indv <- round(100 * var_indv / v_0)
  var_envt <- round(100 - (var_sp + var_indv))  # or it works also , var_envt <- v_0 - var_indv - var_sp Marion calcul
  
  Traits <- c(Trait, Trait, Trait)
  Levels <- c("Environment", "Species", "Individual")
  Variances <- c(var_envt,var_sp,var_indv)

  return(data.frame(Traits, Levels, Variances))
}

# Loop to calculate the variance partitioning for all traits
vars <- c()
Data <- Data %>% filter(Type != "Generalist")

for (i in colnames(Data)[9:17]){
  
  vars <- bind_rows(vars, Var_par(Trait = i, Mydata = Data))
  
}
vars

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

##############################
###Build coefficient table ###
##############################


#function for extracting the coefficient of the model
model_coef_table <- function(Trait, Mydata){

  #Rename the trait column
  colnames(Mydata)[which(colnames(Mydata) == Trait)] <- "Trait"  
  
  # Fit the linear mixed-effects model
  model <- nlme::lme(Trait ~ TWI + Forest,  random=~1|Name, data = Mydata, na.action = na.omit, method = "ML")
  
  # build coef table 
  coef_table <- coef(summary(model))
  coef_table <- as.data.frame(coef_table)
  
  return(coef_table)
}


# For all traits call model_coef_table function
coef_tables <- list()

for (i in colnames(Data)[9:17]){

  coef_tables[[i]] <- model_coef_table(i, Data)
}

# Combine all coefficient tables into one data frame
coef_table <- do.call(rbind, coef_tables)


# create nice table
coef_table %>%
  kbl(caption = "", escape = FALSE, digits = 3) %>%
  row_spec(0, bold = TRUE) %>% 
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "../Tables_SI/Model_summaries.png", zoom = 5)

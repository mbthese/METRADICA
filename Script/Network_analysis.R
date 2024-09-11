#Network analysis general ----

#From Rosas et al., 2018: to characterize trait coordination within and between species, statistically significant correlations among traits were graphically represented using trait covariation networks with the IGRAPH package (Csardi & Nepusz, 2006). Traits were represented as nodes and their correlation as the edges linking them. Two indicators of network centrality were calculated for each trait: the degree (D), defined as the number of edges of a node and the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node (Table S5). In these latter analyses, all traits were loge-transformed to improve the linearity of relationships.

set.seed(123) # for reproducibility

library(here)
library(tidyverse)
library(igraph)
library(corrplot)

Data <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD_18042023.csv") %>% 
  dplyr::select(-Code, -Genus, -Species, Habitat, Type, -SD) %>%
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "Nitrogen", "Carbon", "Phosphorous", "Potassium"), log) %>% 
  mutate(TLP = -TLP)

#Spearman correlation for generalist
traits_generalist <- Data %>% 
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  dplyr::select(-Name, -Habitat, -Type, -Forest, -Plot, -TWI, -DBH) 


#Use this correlation matrix to create an undirected weighted graph.

res <- round(cor(traits_generalist, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

P2 <- graph_from_adjacency_matrix(abs(res), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(P2)

# Extract edges and weights
E(P2)$weight <- E(P2)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(P2))
edges$correlation <- mapply(function(x, y) res[x, y], edges$V1, edges$V2)
E(P2)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(P2)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(P2)$size <- 20
V(P2)$color <- "white"
V(P2)$label.color <- "black"

# Plot the graph with specified attributes
plot(P2, 
    edge.width = E(P2)$weight, 
    edge.color = E(P2)$color, 
    edge.lty = E(P2)$lty, 
    vertex.size = V(P2)$size, 
    vertex.color = V(P2)$color, 
    vertex.label.color = V(P2)$label.color,
    vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist <- degree(P2)
D_generalist <- as.data.frame(D_generalist)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist <- as_adjacency_matrix(P2, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist <- as.data.frame(Dw_generalist)


#SF specialists

#Spearman correlation for generalist
traits_spe_SF <- Data %>% 
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "BF") %>%
  dplyr::select(-Name, -Habitat, -Type, -Forest, -Plot, -TWI, -DBH) 


#Use this correlation matrix to create an undirected weighted graph.

res_SF <- round(cor(traits_spe_SF, method = "spearman"), digits = 2)
p_values_SF <- cor.mtest(traits_spe_SF, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values_SF <- p_values_SF$p

significance_threshold <- 0.05
significant_mask <- p_values_SF < significance_threshold
res_SF[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

P_SF <- graph_from_adjacency_matrix(abs(res_SF), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(P_SF)

# Extract edges and weights
E(P_SF)$weight <- E(P_SF)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(P_SF))
edges$correlation <- mapply(function(x, y) res_SF[x, y], edges$V1, edges$V2)
E(P_SF)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(P_SF)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(P_SF)$size <- 20
V(P_SF)$color <- "white"
V(P_SF)$label.color <- "black"

# Plot the graph with specified attributes
plot(P_SF, 
     edge.width = E(P_SF)$weight, 
     edge.color = E(P_SF)$color, 
     edge.lty = E(P_SF)$lty, 
     vertex.size = V(P_SF)$size, 
     vertex.color = V(P_SF)$color, 
     vertex.label.color = V(P_SF)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("SF specialists")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_SF <- degree(P_SF)
D_spe_SF <- as.data.frame(D_spe_SF)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_SF <- as_adjacency_matrix(P_SF, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_SF <- as.data.frame(Dw_spe_SF)

#TF specialists

#Spearman correlation 
traits_spe_TF <- Data %>% 
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "TF") %>%
  dplyr::select(-Name, -Habitat, -Type, -Forest, -Plot, -TWI, -DBH) 


#Use this correlation matrix to create an undirected weighted graph.

res_TF <- round(cor(traits_spe_TF, method = "spearman"), digits = 2)
p_values_TF <- cor.mtest(traits_spe_TF, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values_TF <- p_values_TF$p

significance_threshold <- 0.05
significant_mask <- p_values_TF < significance_threshold
res_TF[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

P_TF <- graph_from_adjacency_matrix(abs(res_TF), mode = "undirected", weighted = T,
                                    diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(P_TF)

# Extract edges and weights
E(P_TF)$weight <- E(P_TF)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(P_TF))
edges$correlation <- mapply(function(x, y) res_TF[x, y], edges$V1, edges$V2)
E(P_TF)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(P_TF)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(P_TF)$size <- 20
V(P_TF)$color <- "white"
V(P_TF)$label.color <- "black"

# Plot the graph with specified attributes
plot(P_TF, 
     edge.width = E(P_TF)$weight, 
     edge.color = E(P_TF)$color, 
     edge.lty = E(P_TF)$lty, 
     vertex.size = V(P_TF)$size, 
     vertex.color = V(P_TF)$color, 
     vertex.label.color = V(P_TF)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("TF specialists")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_TF <- degree(P_TF)
D_spe_TF <- as.data.frame(D_spe_TF)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_TF <- as_adjacency_matrix(P_TF, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TF <- as.data.frame(Dw_spe_TF)

#Combine

D_table <- cbind(D_generalist, D_spe_SF, D_spe_TF) 
Dw_table <- cbind(Dw_generalist, Dw_spe_SF, Dw_spe_TF)

# Calculate column sums
total_row <- colSums(D_table)
total_row_w <- colSums(Dw_table)

# Append the total row to the original data frame
D_table_with_total <-  rbind(D_table, Total = total_row)
Dw_table_with_total <-  rbind(Dw_table, Total = total_row_w)


D_table_with_total %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")

Dw_table_with_total  %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Along TWI----

Metradica_log <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD_18042023.csv") %>% 
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  relocate(SD, .after = MajVLA) %>%
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), log) %>% 
  mutate(TLP = -TLP) 

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

#Generalist ----
#TWI class 1 
traits_generalist_TWI_class_1 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==1) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_1 <- round(cor(traits_generalist_TWI_class_1, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_1, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_1[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G1 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_1), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G1)

# Extract edges and weights
E(G1)$weight <- E(G1)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G1))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_1[x, y], edges$V1, edges$V2)
E(G1)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G1)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G1)$size <- 20
V(G1)$color <- "white"
V(G1)$label.color <- "black"

# Plot the graph with specified attributes
plot(G1, 
     edge.width = E(G1)$weight, 
     edge.color = E(G1)$color, 
     edge.lty = E(G1)$lty, 
     vertex.size = V(G1)$size, 
     vertex.color = V(G1)$color, 
     vertex.label.color = V(G1)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 1")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_1 <- degree(G1)
D_generalist_TWI_class_1 <- as.data.frame(D_generalist_TWI_class_1)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_1 <- as_adjacency_matrix(G1, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_1 <- as.data.frame(Dw_generalist_TWI_class_1)

#calculate other centrality measures

closeness_generalist_TWI_class_1<- as.data.frame(closeness(G1)) #the greater the value the better, higher value meaning better centrality. If the graph has a weight edge attribute, then this is used by default. Weights are used for calculating weighted shortest paths, so they are interpreted as distances.

#betweenness centrality : how many times a node is a bridge (in the shortest path)
betweenness_generalist_TWI_class_1<- as.data.frame(betweenness(G1))  #the flow of information goes through the one with the highest betweenness "people that like gossip"; transduce signals in proteins. In this sense betweenness is more precise than stress giving also information on how the node is fundamental in the network. If we remove the node with the highest betweeness value, the network will be completely disconnected. 

#TWI class 2 -to change
traits_generalist_TWI_class_2 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==2) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_2 <- round(cor(traits_generalist_TWI_class_2, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_2, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_2[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G2 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_2), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G2)

# Extract edges and weights
E(G2)$weight <- E(G2)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G2))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_2[x, y], edges$V1, edges$V2)
E(G2)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G2)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G2)$size <- 20
V(G2)$color <- "white"
V(G2)$label.color <- "black"

# Plot the graph with specified attributes
plot(G2, 
     edge.width = E(G2)$weight, 
     edge.color = E(G2)$color, 
     edge.lty = E(G2)$lty, 
     vertex.size = V(G2)$size, 
     vertex.color = V(G2)$color, 
     vertex.label.color = V(G2)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 2")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_2 <- degree(G2)
D_generalist_TWI_class_2 <- as.data.frame(D_generalist_TWI_class_2)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_2 <- as_adjacency_matrix(G2, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_2 <- as.data.frame(Dw_generalist_TWI_class_2)


#Generalist TWI =3

traits_generalist_TWI_class_3 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==3) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_3 <- round(cor(traits_generalist_TWI_class_3, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_3, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_3[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G3 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_3), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G3)

# Extract edges and weights
E(G3)$weight <- E(G3)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G3))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_3[x, y], edges$V1, edges$V2)
E(G3)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G3)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G3)$size <- 20
V(G3)$color <- "white"
V(G3)$label.color <- "black"

# Plot the graph with specified attributes
plot(G3, 
     edge.width = E(G3)$weight, 
     edge.color = E(G3)$color, 
     edge.lty = E(G3)$lty, 
     vertex.size = V(G3)$size, 
     vertex.color = V(G3)$color, 
     vertex.label.color = V(G3)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 3")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_3 <- degree(G3)
D_generalist_TWI_class_3 <- as.data.frame(D_generalist_TWI_class_3)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_3 <- as_adjacency_matrix(G3, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_3 <- as.data.frame(Dw_generalist_TWI_class_3)

#Generalist class 4

traits_generalist_TWI_class_4 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==4) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_4 <- round(cor(traits_generalist_TWI_class_4, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_4, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_4[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G4 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_4), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G4)

# Extract edges and weights
E(G4)$weight <- E(G4)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G4))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_4[x, y], edges$V1, edges$V2)
E(G4)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G4)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G4)$size <- 20
V(G4)$color <- "white"
V(G4)$label.color <- "black"

# Plot the graph with specified attributes
plot(G4, 
     edge.width = E(G4)$weight, 
     edge.color = E(G4)$color, 
     edge.lty = E(G4)$lty, 
     vertex.size = V(G4)$size, 
     vertex.color = V(G4)$color, 
     vertex.label.color = V(G4)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 4")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_4 <- degree(G4)
D_generalist_TWI_class_4 <- as.data.frame(D_generalist_TWI_class_4)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_4 <- as_adjacency_matrix(G4, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_4 <- as.data.frame(Dw_generalist_TWI_class_4)

#Generalist class 5

traits_generalist_TWI_class_5 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==5) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_5 <- round(cor(traits_generalist_TWI_class_5, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_5, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_5[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G5 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_5), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G5)

# Extract edges and weights
E(G5)$weight <- E(G5)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G5))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_5[x, y], edges$V1, edges$V2)
E(G5)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G5)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G5)$size <- 20
V(G5)$color <- "white"
V(G5)$label.color <- "black"

# Plot the graph with specified attributes
plot(G5, 
     edge.width = E(G5)$weight, 
     edge.color = E(G5)$color, 
     edge.lty = E(G5)$lty, 
     vertex.size = V(G5)$size, 
     vertex.color = V(G5)$color, 
     vertex.label.color = V(G5)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 5")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_5 <- degree(G5)
D_generalist_TWI_class_5 <- as.data.frame(D_generalist_TWI_class_5)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_5 <- as_adjacency_matrix(G5, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_5 <- as.data.frame(Dw_generalist_TWI_class_5)

#Generalist class 6

traits_generalist_TWI_class_6 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(Type == "Generalist") %>%
  filter(class_TWI==6) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_G_TWI_class_6 <- round(cor(traits_generalist_TWI_class_6, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_generalist_TWI_class_6, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_G_TWI_class_6[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

G6 <- graph_from_adjacency_matrix(abs(res_G_TWI_class_6), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(G6)

# Extract edges and weights
E(G6)$weight <- E(G6)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(G6))
edges$correlation <- mapply(function(x, y) res_G_TWI_class_6[x, y], edges$V1, edges$V2)
E(G6)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(G6)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(G6)$size <- 20
V(G6)$color <- "white"
V(G6)$label.color <- "black"

# Plot the graph with specified attributes
plot(G6, 
     edge.width = E(G6)$weight, 
     edge.color = E(G6)$color, 
     edge.lty = E(G6)$lty, 
     vertex.size = V(G6)$size, 
     vertex.color = V(G6)$color, 
     vertex.label.color = V(G6)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Generalists TWI class 6")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_generalist_TWI_class_6 <- degree(G6)
D_generalist_TWI_class_6 <- as.data.frame(D_generalist_TWI_class_6)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_generalist_TWI_class_6 <- as_adjacency_matrix(G6, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_generalist_TWI_class_6 <- as.data.frame(Dw_generalist_TWI_class_6)

#Combine

D_table <- cbind(D_generalist_TWI_class_1, D_generalist_TWI_class_2, D_generalist_TWI_class_3, D_generalist_TWI_class_4, D_generalist_TWI_class_5, D_generalist_TWI_class_6) 
Dw_table <-cbind(Dw_generalist_TWI_class_1, Dw_generalist_TWI_class_2, Dw_generalist_TWI_class_3, Dw_generalist_TWI_class_4, Dw_generalist_TWI_class_5, Dw_generalist_TWI_class_6) 

# Calculate column sums
total_row <- colSums(D_table)
total_row_w <- colSums(Dw_table)

# Append the total row to the original data frame
D_table_with_total <-  rbind(D_table, Total = total_row)
Dw_table_with_total <-  rbind(Dw_table, Total = total_row_w)


D_table_with_total %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")

Dw_table_with_total  %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Specialist SF + TF----
Metradica_log <- read.csv("Dataset/OUTPUT_cleaning/Subset_imputed/Subset_imputation_exceptSD_18042023.csv") %>% 
  rename(Potassium = K, 
         Phosphorous = P, 
         Nitrogen = N,
         Carbon = C) %>% 
  relocate(SD, .after = MajVLA) %>%
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), abs) %>% 
  mutate_at(c("Gmin","TLP", "LSWC", "MajVLA", "SD", "Phosphorous","Carbon", "Nitrogen", "Potassium", "TWI"), log) %>% 
  mutate(TLP = -TLP) 

Metradica_log <- Metradica_log %>% filter(Type != "Generalist") #for specialists

nb_class <- 7 # for specialists and then merge classes to get enough individuals per class
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

Metradica_log$class_TWI[which(Metradica_log$class_TWI == 7)]  <- 6 #for specialists


table(Metradica_log$class_TWI) #number of individuals per class
table(is.na(Metradica_log$class_TWI)) #verify all individuals have a class

#in the data of classes, merge categories
data$max[which(data$class == 6 )] <- data$max[which(data$class == 7)] #for specialists
data$mean[which(data$class == 6 )] <- (data$max[6] + data$min[6])/2
data <- slice(data, 1:(n() - 1)) 
data

#TWI class 1 
traits_spe_TWI_class_1 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==1) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_1 <- round(cor(traits_spe_TWI_class_1, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_1, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_1[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S1 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_1), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S1)

# Extract edges and weights
E(S1)$weight <- E(S1)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S1))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_1[x, y], edges$V1, edges$V2)
E(S1)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S1)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S1)$size <- 20
V(S1)$color <- "white"
V(S1)$label.color <- "black"

# Plot the graph with specified attributes
plot(S1, 
     edge.width = E(S1)$weight, 
     edge.color = E(S1)$color, 
     edge.lty = E(S1)$lty, 
     vertex.size = V(S1)$size, 
     vertex.color = V(S1)$color, 
     vertex.label.color = V(S1)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 1")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_TWI_class_1 <- degree(S1)
D_spe_TWI_class_1 <- as.data.frame(D_spe_TWI_class_1)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_TWI_class_1 <- as_adjacency_matrix(S1, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_1 <- as.data.frame(Dw_spe_TWI_class_1)

#TWI class 2 
traits_spe_TWI_class_2 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==2) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_2 <- round(cor(traits_spe_TWI_class_2, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_2, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_2[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S2 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_2), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S2)

# Extract edges and weights
E(S2)$weight <- E(S2)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S2))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_2[x, y], edges$V1, edges$V2)
E(S2)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S2)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S2)$size <- 20
V(S2)$color <- "white"
V(S2)$label.color <- "black"

# Plot the graph with specified attributes
plot(S2, 
     edge.width = E(S2)$weight, 
     edge.color = E(S2)$color, 
     edge.lty = E(S2)$lty, 
     vertex.size = V(S2)$size, 
     vertex.color = V(S2)$color, 
     vertex.label.color = V(S2)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 2")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_TWI_class_2 <- degree(S2)
D_spe_TWI_class_2 <- as.data.frame(D_spe_TWI_class_2)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_TWI_class_2 <- as_adjacency_matrix(S2, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_2 <- as.data.frame(Dw_spe_TWI_class_2)

#TWI class 3

traits_spe_TWI_class_3 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==3) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_3 <- round(cor(traits_spe_TWI_class_3, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_3, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_3[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S3 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_3), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S3)

# Extract edges and weights
E(S3)$weight <- E(S3)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S3))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_3[x, y], edges$V1, edges$V2)
E(S3)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S3)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S3)$size <- 20
V(S3)$color <- "white"
V(S3)$label.color <- "black"

# Plot the graph with specified attributes
plot(S3, 
     edge.width = E(S3)$weight, 
     edge.color = E(S3)$color, 
     edge.lty = E(S3)$lty, 
     vertex.size = V(S3)$size, 
     vertex.color = V(S3)$color, 
     vertex.label.color = V(S3)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 3")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_TWI_class_3 <- degree(S3)
D_spe_TWI_class_3 <- as.data.frame(D_spe_TWI_class_3)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_TWI_class_3 <- as_adjacency_matrix(S3, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_3 <- as.data.frame(Dw_spe_TWI_class_3)

#TWI class 4

traits_spe_TWI_class_4 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==4) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_4 <- round(cor(traits_spe_TWI_class_4, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_4, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_4[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S4 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_4), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S4)

# Extract edges and weights
E(S4)$weight <- E(S4)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S4))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_4[x, y], edges$V1, edges$V2)
E(S4)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S4)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S4)$size <- 20
V(S4)$color <- "white"
V(S4)$label.color <- "black"

# Plot the graph with specified attributes
plot(S4, 
     edge.width = E(S4)$weight, 
     edge.color = E(S4)$color, 
     edge.lty = E(S4)$lty, 
     vertex.size = V(S4)$size, 
     vertex.color = V(S4)$color, 
     vertex.label.color = V(S4)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 4")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important

D_spe_TWI_class_4 <- degree(S4)
D_spe_TWI_class_4 <- as.data.frame(D_spe_TWI_class_4)

#weighted degree (Dw)

Dw_spe_TWI_class_4 <- as_adjacency_matrix(S4, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_4 <- as.data.frame(Dw_spe_TWI_class_4)

#TWI class 5

traits_spe_TWI_class_5 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==5) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_5 <- round(cor(traits_spe_TWI_class_5, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_5, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_5[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S5 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_5), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S5)

# Extract edges and weights
E(S5)$weight <- E(S5)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S5))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_5[x, y], edges$V1, edges$V2)
E(S5)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S5)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S5)$size <- 20
V(S5)$color <- "white"
V(S5)$label.color <- "black"

# Plot the graph with specified attributes
plot(S5, 
     edge.width = E(S5)$weight, 
     edge.color = E(S5)$color, 
     edge.lty = E(S5)$lty, 
     vertex.size = V(S5)$size, 
     vertex.color = V(S5)$color, 
     vertex.label.color = V(S5)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 5")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important

D_spe_TWI_class_5 <- degree(S5)
D_spe_TWI_class_5 <- as.data.frame(D_spe_TWI_class_5)

#weighted degree (Dw)

Dw_spe_TWI_class_5 <- as_adjacency_matrix(S5, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_5 <- as.data.frame(Dw_spe_TWI_class_5)

#TWI class 6

traits_spe_TWI_class_6 <- Metradica_log %>%  
  rename(`  K` = Potassium, 
         `  P` = Phosphorous, 
         `  N` = Nitrogen,
         `  C` = Carbon,
         gmin = Gmin) %>% 
  filter(class_TWI==6) %>%
  dplyr::select(-Name, -Code, -Genus, -Species, -Habitat, -Type, -Forest, -Plot, -class_TWI, -DBH, -SD, -TWI) #results in 50 obs


#Use this correlation matrix to create an undirected weighted graph.

res_S_TWI_class_6 <- round(cor(traits_spe_TWI_class_6, method = "spearman"), digits = 2)
p_values <- cor.mtest(traits_spe_TWI_class_6, conf.level = 0.95, exact= FALSE, method = "spearman")
p_values <- p_values$p

significance_threshold <- 0.05
significant_mask <- p_values < significance_threshold
res_S_TWI_class_6[!significant_mask] <- 0  # Set non-significant correlations to zero

#create graph

S6 <- graph_from_adjacency_matrix(abs(res_S_TWI_class_6), mode = "undirected", weighted = T,
                                  diag= F) #don't plot the diagonal because it's a correlation matrix so 1:1

plot(S6)

# Extract edges and weights
E(S6)$weight <- E(S6)$weight * 5  # Adjust weight for better visualization

# Set edge colors and linetypes based on the sign of the correlation
edges <- as.data.frame(get.edgelist(S6))
edges$correlation <- mapply(function(x, y) res_S_TWI_class_6[x, y], edges$V1, edges$V2)
E(S6)$color <- ifelse(edges$correlation > 0, "black", "grey")
E(S6)$lty <- ifelse(edges$correlation > 0, 1, 2)

# Set node attributes for size, color, and label color
V(S6)$size <- 20
V(S6)$color <- "white"
V(S6)$label.color <- "black"

# Plot the graph with specified attributes
plot(S6, 
     edge.width = E(S6)$weight, 
     edge.color = E(S6)$color, 
     edge.lty = E(S6)$lty, 
     vertex.size = V(S6)$size, 
     vertex.color = V(S6)$color, 
     vertex.label.color = V(S6)$label.color,
     vertex.label.cex = 0.8)  # Adjust label size if needed

title("Specialist TWI class 6")

#calculate the degree (D), defined as the number of edges of a node, 
#centrality measure indicating which node is more important


D_spe_TWI_class_6 <- degree(S6)
D_spe_TWI_class_6 <- as.data.frame(D_spe_TWI_class_6)

#problem 1: weight attributes
#don't count the edges but sum the edges if it's a weighted graph!!!
#therefore calculate the weighted degree (Dw), defined as the sum of all significant coefficients of correlation of a node


Dw_spe_TWI_class_6 <- as_adjacency_matrix(S6, attr = "weight") %>%#takes igraph object and turns it into adj matrix
  #degree of node 1, sum all the nodes in row 1, etc. 
  as.matrix() %>%
  rowSums()

Dw_spe_TWI_class_6 <- as.data.frame(Dw_spe_TWI_class_6)


#Combine

D_table_Specialist <- cbind(D_spe_TWI_class_1, D_spe_TWI_class_2, D_spe_TWI_class_3, D_spe_TWI_class_4, D_spe_TWI_class_5, D_spe_TWI_class_6) 
Dw_table_Specialist <-cbind(Dw_spe_TWI_class_1, Dw_spe_TWI_class_2, Dw_spe_TWI_class_3, Dw_spe_TWI_class_4, Dw_spe_TWI_class_5, Dw_spe_TWI_class_6) 

# Calculate column sums
total_row <- colSums(D_table_Specialist)
total_row_w <- colSums(Dw_table_Specialist)

# Append the total row to the original data frame
D_table_with_total_Specialist <-  rbind(D_table_Specialist, Total = total_row)
Dw_table_with_total_Specialist <-  rbind(Dw_table_Specialist, Total = total_row_w)


D_table_with_total_Specialist %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")

Dw_table_with_total_Specialist  %>% kbl(caption ="", digits =3) %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Format 

Dw_table_formatted_G <- Dw_table_with_total %>%
  as.data.frame() %>%
  mutate_all(round, digits = 3) %>%
  rownames_to_column(var = "Trait") %>%
  kbl(caption = "A. Generalist",
      col.names = c("Trait", "Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6"),
      digits = 10) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") 

Dw_table_formatted_S <- Dw_table_with_total_Specialist %>%
  as.data.frame() %>%
  mutate_all(round, digits = 3) %>%
  rownames_to_column(var = "Trait") %>%
  kbl(caption = "B. Specialist",
      col.names = c("Trait", "Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6"),
      digits = 10) %>%
  kable_classic(full_width = FALSE, html_font = "Cambria") 




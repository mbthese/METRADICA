# Load libraries
library(nlme)
library(lme4)

# Load data
Data <- read.csv("Subset_imputation_exceptSD.csv")

# Vector of trait names
trait_names <- c("Gmin", "TLP", "LSWC", "MajVLA", "N", "C", "P", "K", "SD")
n_traits <- length(trait_names)

# Function to compute variance partitioning
variance_partitioning <- function(trait_name, df, pkg="lme4", method_nlme="ML") {

  # Transform data
  if (trait_name != "TLP") {
    df["ltrait_value"] <- log(df[, trait_name])
  } else {
    df["ltrait_value"] <- log(-df[, trait_name])
  }
  df["lTWI"] <- log(df$TWI)
  
  # Only 2 models are needed: (i) null model with species
  # and (ii) mixed model with env and species.
  if (pkg == "nlme") {
    m_null_sp <- nlme::lme(ltrait_value~1, random=~1|Name, data=df,
                           na.action=na.omit, method=method_nlme)
    m_mixed <- nlme::lme(ltrait_value~lTWI+Forest, random=~1|Name, data=df,
                         na.action=na.omit, method=method_nlme)
  } else {
    m_null_sp <- lme4::lmer(ltrait_value~1+(1|Name), data=df)
    m_mixed <- lme4::lmer(ltrait_value~lTWI+Forest+(1|Name), data=df)
  }

  # Variances
  if (pkg == "nlme") {
    v_null_sp <- as.numeric(VarCorr(m_null_sp)[1, 1])
    v_null_res <- as.numeric(VarCorr(m_null_sp)[2, 1])
    v_species <- as.numeric(VarCorr(m_mixed)[1, 1])
    v_ind <- as.numeric(VarCorr(m_mixed)[2, 1])
  } else if (pkg == "lme4") {
    vcov <- as.data.frame(VarCorr(m_null_sp))
    v_null_sp <- vcov[1, "vcov"]
    v_null_res <- vcov[2, "vcov"]
    vcov <- as.data.frame(VarCorr(m_mixed))
    v_species <- vcov[1, "vcov"]
    v_ind <- vcov[2, "vcov"]
  }
  v_null <- v_null_sp + v_null_res

  # Variance partitioning
  p_species <- round(100 * v_species / v_null)
  p_ind <- round(100 * v_ind / v_null)
  p_env <- round(100 - (p_species + p_ind))

  # Returning results
  res <- c(p_species, p_ind, p_env)
  return(res)
}

# ===============================
# nlme - ML
# ===============================

# Data-frame to store the results
df_res <- data.frame(trait=trait_names, p_species=NA, p_ind=NA, p_env=NA)

# Loop on trait names
for (i in 1:n_traits) {
  trait_name <- trait_names[i]
  res <- variance_partitioning(trait_name=trait_name, df=Data, pkg="nlme", method_nlme="ML")
  df_res[df_res$trait==trait_name, 2:4] <- res
}
df_res

# ===============================
# nlme - REML
# ===============================

# Data-frame to store the results
df_res <- data.frame(trait=trait_names, p_species=NA, p_ind=NA, p_env=NA)

# Loop on trait names
for (i in 1:n_traits) {
  trait_name <- trait_names[i]
  res <- variance_partitioning(trait_name=trait_name, df=Data, pkg="nlme", method_nlme="REML")
  df_res[df_res$trait==trait_name, 2:4] <- res
}
df_res

# ===============================
# lme4
# ===============================

# Data-frame to store the results
df_res <- data.frame(trait=trait_names, p_species=NA, p_ind=NA, p_env=NA)

# Loop on trait names
for (i in 1:n_traits) {
  trait_name <- trait_names[i]
  res <- variance_partitioning(trait_name=trait_name, df=Data, pkg="lme4")
  df_res[df_res$trait==trait_name, 2:4] <- res
}
df_res

# Correction for C
df <- Data
df["ltrait_value"] <- log(df[, "C"])
m_null_sp <- lme4::lmer(ltrait_value~1+(1|Name), data=df)
vcov <- as.data.frame(VarCorr(m_null_sp))
v_species <- vcov[1, "vcov"]
v_ind <- vcov[2, "vcov"]
v_null <- v_species + v_ind
p_species <- round(100 * v_species / v_null)
p_ind <- 100 - p_species
p_env <- 0
res <- c(p_species, p_ind, p_env)
df_res[df_res$trait=="C", 2:4] <- res

# End of script

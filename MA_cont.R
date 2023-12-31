# Meta-analysis for mean
# Time to cease shivering (Fig 4) in Wang et al., 2020

# Packages
library(readxl)
library(meta)
library(dmetar)


# Read data ---------------------------------------------------------------

mean_dat <- read_excel("data_cont.xlsx")


# MA for mean -------------------------------------------------------------

mean_res <- metacont(n.e = n_dex, 
                     mean.e = mean_dex, 
                     sd.e = sd_dex, 
                     n.c = n_tra, 
                     mean.c = mean_tra, 
                     sd.c = sd_tra,
                     studlab = study,
                     data = mean_dat,
                     method.tau = "REML", #estimator
                     sm = "MD", 
                     fixed = T, 
                     random = T,
                     prediction = T, 
                     hakn = T, #reduce false positive
                     adhoc.hakn = "iqwig6") #adjust the possible narrow ci caused by hakn
mean_res
forest(mean_res)

# Updated MA
mean_res2 <- update(mean_res, fixed = F)
forest(mean_res2, sortvar = TE)

# Add a few labels in forest plot
forest(mean_res2, 
       sortvar = TE, 
       label.e = "Dexmedetomidine",
       label.c = "Tramadol",
       prediction = T, 
       label.left = "Favours Dexmedetomidine",
       label.right = "Favours Tramadol")


# Publication bias --------------------------------------------------------

# 1. Funnel plot
funnel(mean_res2, studlab = T)

# 2. Statistical test
metabias(mean_res2, plotit = T, method.bias = "Egger") #generic
metabias(mean_res2, plotit = T, method.bias = "Begg") #generic
metabias(mean_res2, method.bias = "Pustejovsky") #specific to SMD

# IF publication bias exist (I^2 should be low)
mean_res_tf <- trimfill(mean_res2)
mean_res_tf #not valid I^2 high


# Outliers ----------------------------------------------------------------

# Assess outlier (I^2 > 50%) 
find.outliers(mean_res2) #cannot have NAs for this

# Influential diagnostic
mean_res2_inf <- InfluenceAnalysis(mean_res2, random = T) #better

plot(mean_res2_inf, "baujat")
plot(mean_res2_inf, "ES")
plot(mean_res2_inf, "I2")
plot(mean_res2_inf, "influence") #a bit advanced


# Subgroup analysis -------------------------------------------------------

# Subgroup analysis (k > 10) 
mean_sub <- update(mean_res2, subgroup = age_gp)
mean_sub

forest(mean_sub, 
       sortvar = TE, 
       bylab = "Age group", 
       label.left = "Favours Dexmedetomidine", 
       label.right = "Favours Tramadol")


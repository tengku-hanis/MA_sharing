# Meta-analysis for risk ratio
# Recurrent rate of shivering (Fig 5) in Wang et al., 2020

# Install packages
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")
install.packages("readxl")
install.packages("meta")
install.packages("dplyr")

# Packages
library(readxl)
library(meta)
library(dmetar)


# Read data ---------------------------------------------------------------

rr_dat <- read_excel("data_bin.xlsx")


# MA for risk ratio -------------------------------------------------------

rr_res <- metabin(event.e = event_dex, 
                  n.e = n_dex, 
                  event.c = event_tra, 
                  n.c = n_tra, 
                  data = rr_dat, 
                  studlab = study,
                  method.tau = "PM", #estimator
                  sm = "RR",
                  fixed = T, 
                  random = T,
                  prediction = T, 
                  hakn = T, #reduce false positive
                  adhoc.hakn = "iqwig6") #adjust the possible narrow ci caused by hakn
rr_res
forest(rr_res)

# Updated MA
rr_res2 <- update(rr_res, random = F)
forest(rr_res2, sortvar = TE)

# Add a few labels in forest plot
forest(rr_res2, 
       sortvar = TE, 
       label.e = "Dexmedetomidine",
       label.c = "Tramadol",
       prediction = T,
       pooled.events = T, 
       label.left = "Favours Dexmedetomidine",
       label.right = "Favours Tramadol")


# Publication bias --------------------------------------------------------

# 1. Funnel plot
funnel(rr_res2, studlab = T)

# 2. Statistical test
metabias(rr_res2, plotit = T, method.bias = "Egger") #generic
metabias(rr_res2, plotit = T, method.bias = "Begg") #generic
metabias(rr_res2, plotit = T, method.bias = "Peter") #specific to metabin
metabias(rr_res2, plotit = T, method.bias = "Harbord") #specific to metabin

# IF publication bias exist (I^2 should be low)
rr_res_tf <- trimfill(rr_res2)
rr_res_tf

funnel(rr_res_tf, studlab = T)


# Outliers ----------------------------------------------------------------

# Assess outlier (I^2 > 50%) 
find.outliers(rr_res2) #cannot have NAs for this

# Influential diagnostic
rr_res2_inf <- InfluenceAnalysis(rr_res2, random = T) #better

plot(rr_res2_inf, "baujat")
plot(rr_res2_inf, "ES")
plot(rr_res2_inf, "I2")
plot(rr_res2_inf, "influence") #a bit advanced


# Subgroup analysis -------------------------------------------------------

# Subgroup analysis (k > 10) 
rr_sub <- update(rr_res2, subgroup = age_gp)
rr_sub

forest(rr_sub, 
       sortvar = TE, 
       bylab = "Age group", 
       label.left = "Favours Dexmedetomidine", 
       label.right = "Favours Tramadol")


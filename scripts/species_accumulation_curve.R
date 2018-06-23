# /////////////////////////////////////////////////////////////////////////
# Build Species Accumulation Curves (SAC) / Sampling Curves and 
# extrapolated ("true") insect species richness (Chao estimator) 
# /////////////////////////////////////////////////////////////////////////

# NOTE: We want to keep on sampling (collecting insects) until we reach saturation 
# on observed insect species richness (until the curve reaches its asymptote; 
# or put it differently, until observed species richness is very close to 
# extrapolated species richness - Chao estimator).


rm(list = ls()) # remove all objects (if any)

library(vegan)


# Read data ---------------------------------------------------------------

# Read network data (similar to what you sampled in the field)
net_dt <- read.csv(file   = "data/ro_network2.csv", 
                   header = TRUE,
                   strip.white = TRUE,
                   stringsAsFactors = FALSE)
# Note the extra argument stringsAsFactors = FALSE
# check ?read.csv for its meaning
# If FALSE - do NOT transform character columns into factors

# Have a look at the data
str(net_dt)
head(net_dt)
summary(net_dt)
View(net_dt)


# Plant species SAC -------------------------------------------------------

# Prepare community matrix (a contingency table)
com_mat_pl <- table(net_dt$date, net_dt$plant_sp)
# Transform table matrix to data frame (helps with visualizing the table)
com_mat_pl <- as.data.frame.matrix(com_mat_pl)
# Visualize the table
View(com_mat_pl)

# NOTE:
# The counts in the community matrix are not important for building the SAC.
# What is important is the presence-absence information (the connection per se).
# So, you could have 0 and 1-s instead of counts in your community matrix.
com_mat_pl[com_mat_pl > 0] <- 1 # you get same SAC with or without this line
View(com_mat_pl)


set.seed(321) # assures reproducibility 
sac_plants <- specaccum(com_mat_pl, method = "random", permutations = 1000)
sac_plants

plot(sac_plants,
     ci.type = "polygon",
     ci.col = "lightblue",
     col = "black",
     lwd = 2,
     ci.lty = 0,
     xlab = "Effort - here days",
     ylab = "Number of plant species")


# Insect species SAC ------------------------------------------------------

com_mat_i <- table(net_dt$date, net_dt$insect_sp)

set.seed(321)
sac_insects <- specaccum(com_mat_i, method = "random", permutations = 1000)
sac_insects

plot(sac_insects,
     ci.type = "polygon",
     ci.col = "lightblue",
     col = "black",
     lwd = 2,
     ci.lty = 0,
     xlab = "Effort - here days",
     ylab = "Number of insect species")


# Plant-insect interaction SAC --------------------------------------------

net_dt$interactions <- paste(net_dt$plant_sp, net_dt$insect_sp, sep = "-")
head(net_dt)

com_mat_int <- table(net_dt$date, net_dt$interactions)

set.seed(321)
sac_interactions <- specaccum(com_mat_int, method = "random", permutations = 1000)
sac_interactions

plot(sac_interactions,
     ci.type = "polygon",
     ci.col = "lightblue",
     col = "black",
     lwd = 2,
     ci.lty = 0,
     xlab = "Effort - here days",
     ylab = "Number of unique plant-insect interactions")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRACTICE WITH YOUR COLLECTED DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXTRA -------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build Species Accumulation Curve (SAC) / Sampling Curve for a plant species

plant <- net_dt[net_dt$plant_sp == "AchMil", ]

n_insects <- tapply(X     = plant$n_obs, 
                    INDEX = plant$insect_sp, 
                    FUN   = sum)
n_insects

# How many unique pollinators do you have for your species?
length(n_insects)

# How many individuals (insects) in total?
sum(n_insects)

# Sampling curve
vegan::rarecurve(t(n_insects), 
                 xlab  = "Number of individuals (insects)", 
                 ylab  = "Pollinator richness (insect species)",
                 main  = "Species richness for ...",
                 label = FALSE)

# Get the extrapolated (estimated) species richness
vegan::estimateR(n_insects, index = "chao")
# see ?estimateR
# S.obs    = number of unique insect species observed (observed species richness)
# S.chao1  = extrapolated species richness (Chao estimator)
# se.chao1 = standard error of S.chao1
# S.ACE    = extrapolated species richness (ACE estimator)
# se.ACE   = standard error of S.ACE

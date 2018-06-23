# /////////////////////////////////////////////////////////////////////////
# Network graphs and metrics
# /////////////////////////////////////////////////////////////////////////

rm(list = ls()) # remove all objects (if any)

library(bipartite)


# Read data ---------------------------------------------------------------

# Read network data (similar to what you sampled in the field)
net_dt <- read.csv(file   = "data/ro_network2.csv", 
                   header = TRUE,
                   strip.white = TRUE,
                   stringsAsFactors = FALSE)

# Have a look at the data
str(net_dt)
head(net_dt)
View(net_dt)


# Community matrix --------------------------------------------------------

# Prepare community matrix (a contingency table).
# It is "a matrix representing the interactions observed between 
# higher trophic level species (columns = insects) 
# and lower trophic level species (rows = plants). 
# Usually this will be number of pollinators on each species of plants" 
# (from ?visweb)

# "Explode" rows - if an insect visited more than one time, then
# replicate (repeat) that particular row in the table
net_dt <- net_dt[rep(seq(1, nrow(net_dt)), net_dt$n_obs),]
View(net_dt)

com_mat <- table(net_dt$plant_sp, net_dt$insect_sp)
# Transform table matrix to data frame (helps with visualizing the table)
com_mat <- as.data.frame.matrix(com_mat)
# Visualize the table
View(com_mat)

# If you need to respect certain thresholds, for example
# to select only those plants that have more than 10 insect counts, then
# we can index the contingency table with a logical vector
com_mat <- com_mat[rowSums(com_mat) > 10, ]

# If you need only zeroes and ones 
# (so, just the interaction, without how strong it is),
# then replace anything bigger than zero with 1
com_mat_01 <- com_mat
com_mat_01[com_mat_01 > 0] <- 1


# Network plots -----------------------------------------------------------

# The matrix plot shows where the interactions plant-insects are and if 
# using square="interaction" can also show how strong interactions are 
# using levels of gray (white means no interaction)
visweb(com_mat,
       type = "nested",
       labsize = 1.4,
       plotsize = 12,
       xlabel = "Pollinators",
       ylabel = "Plants",
       square = "interaction")

# No difference between com_mat and com_mat_01 when not being interested 
# in how strong the interaction is (square = "black"):
visweb(com_mat,
       type = "nested",
       labsize = 1.4,
       plotsize = 12,
       xlabel = "Pollinators",
       ylabel = "Plants",
       square = "black")
# Same as above
visweb(com_mat_01,
       type = "nested",
       labsize = 1.4,
       plotsize = 12,
       xlabel = "Pollinators",
       ylabel = "Plants",
       square = "black")


# Bipartite interaction plot - using the counts community matrix
plotweb(com_mat,
        method = "cca",
        labsize = 1,
        text.rot = 90,
        y.width.low = 0.01,
        y.width.high = 0.01,
        arrow = "down",
        high.y = 1.2,
        low.y = 0.6)

# Bipartite interaction plot - using the community 0-1 matrix
plotweb(com_mat_01,
        method = "cca",
        labsize = 1,
        text.rot = 90,
        y.width.low = 0.01,
        y.width.high = 0.01,
        arrow = "down",
        high.y = 1.2,
        low.y = 0.6)


# Network indices (statistics/metrics) ------------------------------------

networklevel(web = com_mat, index = "connectance", level = "lower")
networklevel(web = com_mat_01, index = "connectance", level = "lower")

set.seed(321)
networklevel(web = com_mat, index = "nestedness", level = "lower")
networklevel(web = com_mat_01, index = "nestedness", level = "lower")

# If you need all indices for plant species
# net_idx_plants <- networklevel(web = com_mat, level = "lower")

# check out ?networklevel for details about the metrics.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRACTICE WITH YOUR COLLECTED DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

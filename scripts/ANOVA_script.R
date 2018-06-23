# /////////////////////////////////////////////////////////////////////////
# ANOVA for pollen supplementation and breeding system experiment
# /////////////////////////////////////////////////////////////////////////

rm(list = ls()) # remove all objects (if any)


# Read data ---------------------------------------------------------------

# Read csv file as data frame
Carduus_data <- read.csv(file   = "data/Carduus_data.csv", 
                         header = TRUE,
                         strip.white = TRUE)
# NOTE: 
# for the German Windows operating system you most probably need to replace
# read.csv() with read.csv2() function (that is, sep = ";" and dec = ",")


# Run ANOVA ---------------------------------------------------------------

# We first run the ANOVA model to get its error terms/residuals and 
# check assumptions on them.
# ANOVA is a parametric test and because of that we need to check 
# the assumptions of normality and homogeneity of the residuals.
# EXTRA: what if assumptions are note met:
# https://stats.stackexchange.com/a/91881/95505

aov_model <- aov(Ratio ~ Treatment, data = Carduus_data)
# or also: 
# aov_model <- anova(lm(Ratio ~ Treatment, data = Carduus_data))

# Many objects in R have names
names(aov_model)

# Get the error terms/residuals
resid <- aov_model$residuals


# Check normality assuption -----------------------------------------------

# We check the normality of the error terms/residuals from the model:

# ... Using graphical tools -----------------------------------------------

# Histogram
hist(resid, probability = TRUE)
mean(resid)
abline(v = mean(resid), col = "red")
curve(dnorm(x,
            mean = mean(resid, na.rm = TRUE),
            sd   = sd(resid, na.rm = TRUE)),
      add = TRUE, 
      col = "red")

# Q-Q plots (quantile versus quantile plot)
qqnorm(resid); qqline(resid, col="red")


# ... Using statistical tests ---------------------------------------------

# Common tests for normality are Kolmogorov-Smirnov test and Shapiro-Wilk.

ks.test(resid, "pnorm", mean(resid), sd(resid)) 
shapiro.test(resid)

# For each case, the p-values were above the 5% significance level, 
# so there is not enough evidence to reject the null hypothesis.
# The null hypothesis assumes that our residuals come from a normally 
# distributed population of residuals.
# So our data might not be different from a normal distribution 
# (therefore, we can assume that the residuals are normally distributed).


# Check homogeneity of variance -------------------------------------------

# We check the homogeneity of variance of the error terms/residuals 
# from the model:

# ... Using graphical tools -----------------------------------------------

# Boxplot
boxplot(Ratio ~ Treatment,
        data = Carduus_data,
        main = "Viable Seeds/Flower ratio by treatment",
        ylab = "viable seeds/flower ratio")
# When the hinge-spread in one box is two- to three-times greater 
# in most variable and least variable groups should alert you to 
# possible heteroscedasticity 
# (but this can be unreliable when samples are small)
# from: http://www.sjsu.edu/faculty/gerstman/StatPrimer/anova-b.pdf


# ... Using statistical tests ---------------------------------------------

# Bartlett's test
bartlett.test(Ratio ~ Treatment, data = Carduus_data)

# Levene's test
# Is considered more robust to departures from normality than Bartlett's test.
# install.packages("car")
library(car)
# Computes Levene's test for homogeneity of variance across groups
leveneTest(Ratio ~ Treatment, data = Carduus_data)

# The group variances are not statistically different.


# ANOVA summary & "post hoc" tests ----------------------------------------

summary(aov_model)

# The F-value is 4.758, which is bigger than 1 and the p-value 0.0192 
# is smaller than the 0.05 significance level. 
# As a result, you have almost 5 times (4.758~5) as much between group variance 
# as within group variance, 
# so there is evidence for an effect of the treatments. 
# The F-test indicates a statistical significant difference between groups. 
# Now you might want to know specifically between which groups 
# there are statistically significant differences.

# To address this question we need to run a "post hoc" test. 
# Differences between the means can be displayed with TukeyHSD 
# (Honest Significant Differences)
TukeyHSD(aov_model)
# We can see that the difference in means between supplement and bag 
# is statistically significant 
# (p-value 0.019 < 0.05 significance level).

# Another way to check for statistically significant differences between means 
# is to check if their confidence intervals are containing zero or not:
plot(TukeyHSD(aov_model))
# If a CI contains zero, it means that that particular difference 
# is not statistically different from zero. 
# That means, that there is no statistically significant differences 
# between the two groups.
# Note here that even though the p-val of Control-Bag difference 
# is not significant, the CI still contains the zero. 
# Perhaps if the sample size is increased then such contradiction 
# will disappear. 
# Note that P-value based conclusions are more popular.


# Degree autogamy and effect size of pollen limitation --------------------

# Degree of Autogamy (AFI ratio , auto-fertility index)
bag <- Carduus_data[Carduus_data$Treatment == "Bag", "Ratio"]
sup <- Carduus_data[Carduus_data$Treatment == "Supplement", "Ratio"]
mean(bag) / mean(sup)
# [1] 0.4803156
# Ratio value above 0.2 indicates that the plant is not pollinator dependent 
# (Rogers & Ellis 2016).
# A low value (i.e. close to 0) indicates high dependency on animal pollination

# Magnitude (effect size) of Pollen Limitation (the log response ratio)
con <- Carduus_data[Carduus_data$Treatment == "Control", "Ratio"]
log(mean(sup) + 0.5) - log(mean(con) + 0.5)
# [1] 0.04350471
# A high value for degree of pollen limitation indicates that the plant 
# is pollen limited.
# A low value (i.e., close to 0) indicates no pollen limitation.

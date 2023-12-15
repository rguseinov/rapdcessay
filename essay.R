#### PART 0 ####
library(haven)
library(psych)
library(dplyr)
library(arm)
library(multilevel)
library(lattice)
library(ggplot2)
library(lmerTest)
library(influence.ME)
library(glmmTMB)
library(sjPlot)
library(CR2)
library(countrycode)
library(plm)
library(stargazer)
library(democracyData)

#### PART 1 ####
df <- read.csv(file.choose())
df <- na.omit(df)
df$personalistregime <- ifelse(df$gwf_regimetype %in% c('monarchy', 'personal'), 1, 0) #recode for personalist regimes
df$ifdem <- ifelse(df$gwf_regimetype %in% c(0), 1, 0)
df$polydem <- ifelse(df$v2x_polyarchy <= 0.3, 1, 0)
country_unique <- unique(df$country)
df$country_id = as.factor(df$country)

#### Get Descriptive statisitcs ####
stargazer(df, 
          type = 'latex', min.max=TRUE, mean.sd = TRUE, 
          nobs = FALSE, median = TRUE, iqr = FALSE,
          digits=1, align=T,
          title = "Descriptive statistics")

#### Do we need a mixed effects model? ####
null <- lmer(protestnum ~ 1 + (1|country), REML = FALSE, data = df)
performance::icc(null) #ICC is 0.6 - ME makes sense
graph.ran.mean(df$protestnum, df$country_id, nreps=1000, bootci=TRUE) #additional check

#### Modelling ####
options(scipen = 999)
model1 <- lmer(protestnum ~ sanctionnum + (1 + sanctionnum|country), REML = FALSE, data = df)
model2 <- lmer(protestnum ~ sanctionnum + tenure_lagged + (1 + sanctionnum|country), REML = FALSE, data = df)
model3 <- lmer(protestnum ~ sanctionnum + log(I(gdpval+0.01)) + (1 + sanctionnum|country), REML = FALSE, data = df)
model4 <- lmer(protestnum ~ sanctionnum + tenure_lagged + log(I(gdpval+0.01)) + (1 + sanctionnum|country), REML = FALSE, data = df)
modelinteract <- lmer(protestnum ~ sanctionnum*ifdem + log(I(gdpval+0.01)) + ifdem + (1 + sanctionnum|country), REML = FALSE, data = df)

class(model1) <- "lmerMod"
class(model2) <- "lmerMod"
class(model3) <- "lmerMod"
class(model4) <- "lmerMod"
class(modelinteract) <- "lmerMod"
stargazer(model1, model2, model3, model4, modelinteract, type = 'latex', keep.stat="n")

dotplot(ranef(modelinteract, condVar=TRUE)) #check for heterogeneity
plot_model(modelinteract, type = 'pred', terms = c('sanctionnum', 'ifdem')) #plot interaction effect

#### Influential observations, Robustness checks ########
inf <- influence(modelinteract, group = "country", data = df)
length(which(cooks.distance.estex(inf, sort=TRUE) > 4/length(unique(df$country)), arr.ind=TRUE))
plot(inf, which = "cook", xlab = "COOK'S MEASURE", cutoff = 4/length(country_unique)) #a lot of influential observations - countries under sanctions
influence_data <- data.frame(dfbetas(inf))
inf_sanction <- influence_data %>%
  filter(abs(sanctionnum) > 2/sqrt(length(as.factor(df$country))))
df_cut <- df %>%
  filter(!country %in% rownames(inf_sanction))

modelinteract_upd <- lmer(protestnum ~ sanctionnum*ifdem + log(I(gdpval+0.01)) + ifdem + (1 + sanctionnum|country), REML = FALSE, data = df_cut)
class(modelinteract_upd) <- "lmerMod"
stargazer(modelinteract_upd, type = 'latex', keep.stat="n")

#Re-estimate model with robust standard errors
robust_mixed(modelinteract)

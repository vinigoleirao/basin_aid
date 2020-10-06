#############################################################################################
#############################################################################################
############################ BASIN POVERTY ##################################################
############################ FINAL PAPER ####################################################
#############################################################################################
########################  VINICIUS SILVA SANTANA ############################################
#############################################################################################
#############################################################################################


############################################################################################
############################ LOADING PACKAGES AND DATAFRAME ################################
############################################################################################

getwd()
setwd("./Curso R/QOG Data")

require(tidyverse)
require(ggplot2)
require(ggpubr)
require(gridExtra)
library(dplyr)
library(tibble)
library(modelr)

pov.aid <- read.csv("./vinicius-santana-ad-ufpe-2019.csv")

view(pov.aid)
names(pov.aid)
summary(pov.aid)

#########################################################################################
################################ EXPLORATORY DATA ANALYSIS ##############################
#########################################################################################

# NUMBER OF COUNTRIES PER BASIN AREA
ggplot(data = pov.aid) +
  geom_bar(mapping = aes(x = cregion)) + coord_flip() +
  labs(title = "Number of countries per basin area")

# DISTRIBUTION AID AID PER CAPITA IN THE REGIONS 
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_capta)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of International Aid per Capta")

# DISTRIBUTION OF TOTAL AID IN THE REGIONS
ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_total)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Total Aid")

# AID AND AID PER CAPITA
aid.total.plot <- ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_total)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Total Aid")

aid.capta.plot <- ggplot(data = pov.aid, mapping = aes(x = cregion, y = aid_capta)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of International Aid per Capta")


grid.arrange(aid.total.plot, aid.capta.plot, ncol = 2)

# calculating median aid per capta in the bay of bengal
summary(pov.aid$aid_capta[pov.aid$cregion == "Bay of Bengal"])

# calculating median aid per capta in the bay of bengal
summary(pov.aid$aid_capta[pov.aid$cregion == "Caspian Sea"])

# DISTRIBUTION OF POVERTY RATES IN THE REGIONS
ggplot(data = pov.aid, mapping = aes(x = cregion, y = pov_rate)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Distribution of Poverty Rates within Regions")

########### HIGHEST POVERTY RATES ARE ENCOUNTERED IN ALL THREE AFRICAN REGIONS

# checking mean income and aid_crsio
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsio))

# mean income and aid_crsio log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_crsio), color = cregion)) +
  geom_text(label = rownames(data), nudge_x = 0.25, nudge_y = 0.25, 
            check_overlap = T)

# median_inc e aid crsc
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_crsc))

# log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_crsc)))

# median income and aid total
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_total))

#log
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_total)))
# IT DOESNT SEEM THAT MEDIAN INCOME MATTERS MUCH FOR THE SIZE OF AID

# mean income and aid capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = log(aid_capta)))

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = median_inc, y = aid_capta))

##########################################################################################
##########################################################################################
##########################################################################################


ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = aid_total)) +
  geom_smooth(mapping = aes(x = gdp_size, y = aid_total))
######## GDP SIZE SEEMS TO BE UNIMPORTANT AT FIRST SIGHT

# what about if GDP is logged?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = log(gdp_size), y = aid_total)) +
  geom_smooth(mapping = aes(x = log(gdp_size), y = aid_total))
####### LOGGED GDP HOWEVER DOES SHOW STRONG CORRELATION WITH THE TOTAL AMOUNT OF AID

# what if aid is logged?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = log(aid_total))) # not very conclusive, but
#it seems to be the case when the amount of aid gets higher.

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gdp_size, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gdp_size, y = log(aid_total)))

# what if both are logged
ggplot(data = pov.aid, mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = log(gdp_size), y = log(aid_total)))
  labs(title = "Relation between GDP and Aid - both logged")
####### EVEN STRONGER CORRELATION
  
###########################################################################################

ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = gpd_capta, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = gpd_capta, y = log(aid_total)))
###### GDP PER CAPITA AND LOGGED AID IS INCONCLUSIVE BUT WITH HIGER GDP PER CAPITA +7500 THE HIGHER
###### AID

############################################################################################

# WHAT ABOUT POVERTY RATES?
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_total))
# data suggests that countries that receive low aid_total tend to have their aid slightly
# increased as poverty rates are higher. High aid total was only given to countries with less
# than 40% pov rate

# logging it
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = log(aid_total))) +
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_total))) # when logged, data correlation
# seems to disappear but the line indicates a rather slight trend towards reducing aid as
# poverty rates get higher

# checking by aid_capta
ggplot(data = pov.aid) +
  geom_point(mapping = aes(x = pov_rate, y = aid_capta)) +
  geom_smooth(mapping = aes(x = pov_rate, y = aid_capta))


# AID CAPITA AND AID TOTAL
ggplot(data = pov.aid, mapping = aes(x = aid_capta, y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = aid_capta, y= log(aid_total))) +
  labs(title = "Relation between Aid Total and Aid per capta")
# the lower aid capta, the higher aid total for the Bay of Bengal; Nile Basin has similar
# aid capta range, as seen also in the box plot, but aid total varies


# what about gdp growth?
ggplot(data = pov.aid, mapping = aes(x = gdp_growth, y = log(aid_total))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = gdp_growth, y= log(aid_total))) +
  labs(title = "Relation between Aid Total and GDP growth") +
  xlab(label = "GDP GROWTH") +
  ylab(label = "TOTAL AID - logged")
#The lower the gdp growth the less logged aid

#LOG AIND CRSIO AND GDP GROWTH

ggplot(data = pov.aid, mapping = aes(x = gdp_growth, y = log(aid_crsio))) + 
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth(mapping = aes(x = gdp_growth, y= log(aid_crsio)))


# checking relation between pov_rate and aid_total for each region
ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_total))) + 
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_total))) +
  facet_wrap(~ cregion, nrow = 2) #not use this.. warning say few values for degrees of freedom

ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_capta))) + 
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_capta))) +
  facet_wrap(~ cregion, nrow = 2)

ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = pov_rate, y = log(aid_poor))) + 
  geom_smooth(mapping = aes(x = pov_rate, y = log(aid_poor))) +
  facet_wrap(~ cregion, nrow = 2)

##################

ggplot(data = pov.aid) + 
  geom_point(mapping = aes(x = log(gdp_size), y = log(aid_total))) + 
  geom_smooth(mapping = aes(x = log(gdp_size), y = log(aid_total))) +
  facet_wrap(~ cregion, nrow = 2)

# THE HIGHER GDP SIZE(logged), THE HIGHER AID TOTAL LOGGED. THE RELATIONSHIP IS LESS CLEAR IN
# the congo and niger basin and partially clear in the caspian sea.. for the other three groups
# the pattern is more evident, with one country in the Amazon region falling significantly off
# the curve.


# gdp capta and aid total
ggplot(data = pov.aid) +
  geom_hex(mapping = aes(x = gpd_capta, y = aid_total))

# what about gdp size and aid_total when they are logged?
ggplot(data = pov.aid, aes(x = gdp_size, y = aid_total)) +
  geom_point() + geom_smooth() +
  scale_x_log10() + scale_y_log10() # it also seems the higher the gdp size, the higher the aid

# same for pov rate
ggplot(data = pov.aid, aes(x = pov_rate, y = log(aid_total))) +
  geom_point(mapping = aes(color = cregion)) +
  geom_smooth() # which in turn did not show as much correlation


#checking correlation between the two main variables
cor(pov.aid$pov_rate, log(pov.aid$gdp_size), method = "pearson")
# -.54 meaning only partially correlated

#testing covariation between IV and DV
cor(log(pov.aid$aid_total), log(pov.aid$gdp_size), method = "pearson") #0.68
cor(log(pov.aid$aid_total), pov.aid$pov_rate, method = "pearson") #-.20 very low neg correlation

cov(log(pov.aid$aid_total), log(pov.aid$gdp_size), method = "pearson") #0.68
cov(log(pov.aid$aid_total), pov.aid$pov_rate, method = "pearson")

##########################################################################
# APPLYING A LINEAR REGRESSION USING GDP SIZE TO ESTIMATE THE VALUE OF TOTAL AID
povaidreg3 <- lm(data = pov.aid, log(aid_total) ~ log(gdp_size) + pov_rate)
summary(povaidreg3)

# WITH A HIGH SIGNFICANT VALUE, AND A POSITIVE SIGNAL, IT SEEMS HIGHER GDP ALLOWS FOR MORE
# AID RECEIVED IN TOTAL

plot(povaidreg3)
###########################################################################
# IF GDP ALLOWS MORE AID, HOW DOES IT WORK WHEN TAKING POVERTY RATE INTO ACCOUNT CONTROLLING
# THE OUTCOMES OF INTERNATIONAL COOPERATION?

povaidmulti <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate)
summary(povaidmulti)

# WITH THIS REGRESSION, POVERTY RATE BECOMES STATISTICALLY NON SIGNIFICANT HOWEVER IT STILL SHOWS
# A NEGATIVE INFLUENCE OVER THE TOTAL AMOUNT OF AID RECEIVED BY THE COUNTRIES

plot(povaidmulti)

# CHECKING IF THE REGION PLAYS A ROLL IN THE OUTCOMES OF TOTAL AID
plot(pov.aid$cregion, pov.aid$aid_total)
aid.regionreg1 <- lm(data = pov.aid, aid_total ~ cregion)
summary(aid.regionreg1)

# with log
aid.regionreg1log <- lm(data = pov.aid, log(aid_total) ~ cregion)
summary(aid.regionreg1log)

#aidcapta
aidcaptaregion <- lm(data = pov.aid, aid_capta ~ cregion)
summary(aidcaptaregion) # NOT USE; NOT IMPORTANT

# IN RELATION TO THE AMAZON BASIN, ONLY  THE BAY OF BENGAL HAS A SLIGHT STATISTICAL SIGNIFICANCE
# SHOWING IT PLAYS A ROLE IN INCREASING THE VALUE OF AID.

# ADDING TO THE REGRESSION THE GDP GROWTH, THE MEDIAN INCOME AND THE REGION OF COUNTRIES
full.pov.aid.reg <- lm(data = pov.aid, aid_total ~ pov_rate + gdp_size + gdp_growth + cregion)
summary(full.pov.aid.reg)

# CHECKING AN INVERTED REGRESSION WHERE POVERTY RATES CONTROLL THE INFLUENCE OF GDP IN AID RECEIVING
inverse.pov.aid.reg <- lm(data = pov.aid, aid_total ~ gdp_size + pov_rate + gdp_growth + median_inc + cregion)
summary(inverse.pov.aid.reg)

# AGAIN GDP SHOWS TO BE MORE SIGNIFICANT THAN POVERTY RATE

#FULL MODEL TO BE ANALYSED

plot(log(pov.aid$pop_total), log(pov.aid$aid_total))
cor(log(pov.aid$pop_total), log(pov.aid$aid_total), method = "pearson") #.79 high correlation
cor(log(pov.aid$pop_total), log(pov.aid$gdp_size), method = "pearson") #.82 high correlation
# i will therefore take pop_total out of the model,let us check for pop_poor
plot(log(pov.aid$pop_poor), log(pov.aid$aid_total))
cor(log(pov.aid$pop_poor), log(pov.aid$aid_total), method = "pearson") #less correlated .43
cor(log(pov.aid$pop_poor), log(pov.aid$gdp_size), method = "pearson") #even less .19, include pop poor

########################################################################################
#FULL MODELS

###### MODEL ONE POVERTY RATE BEING CONTROLLED BY THE REST

fullmodel1 <- lm(data = pov.aid, log(aid_total) ~ pov_rate + log(gdp_size) + gdp_growth + median_inc + log(pop_poor) + cregion)
summary(fullmodel1)                 
# it explains 69% of the statistics (59 if R squared are adjusted).. controlled by the others,
# pov_rate presents to have a slightly negative impact on the total amount of aid received.
# this can be interpreted like this: since the poverty rate depends on the poverty rates of the
# previous years, then donor countries and organizations might interpret it as the aid is not
# being effectively used, and therefore, high rates of poverty, despite denouncing the need for
# aid will have a negative effect, although marginal on total aid. GDP size, however, not only
# is statistically significant to total aid, but represents a fair increase in total aid.

# checking if the model fits
plot(fullmodel1)
# graph 1 shows the residuals vs fitted, it can be seen that the model confirms the homoskedacist
# need as the residual values are reasonably distributed around zero and with similar amplitude.
# graph two shows that most of the data are not far away from the diagonal line, except for the
# two extreme values. the sclae location show the standardized residuals within the fitted values
#of the model. The last graph shows the Cook's distance, that does show most data fit well in the
# model, and there is one piece that gets closer to being an outlier

shapiro.test(fullmodel1$residuals) # this shows the residuals have normal distribution as its
# p value is higher than 0.05

plot(density(resid(fullmodel1))) # plot de densidade dos residuos, ok
qqnorm(resid(fullmodel1))
qqline(resid(fullmodel1))   # this is the same as with plot(lm)

library(sjPlot)
pred1 <- plot_model(fullmodel1, type = "pred", terms = c("pov_rate")) # PREDICTED VALUES OF TOTAL AID poverty
pred2 <- plot_model(fullmodel1, type = "pred", terms = c("gdp_size [exp]"))# PREDICTED VALUES OF TOTAL AID gdp size
plot_model(fullmodel1, type = "pred", terms = c("gdp_size"))
plot_model(fullmodel1, type = "pred", terms = c("pov_rate [exp]"))
plot_model(fullmodel1, type = "pred", terms = c("pop_poor"))
plot_model(fullmodel1, type = "pred", terms = c("pov_rate", "gdp_size"))
plot_model(fullmodel1, type = "pred", terms = c("gdp_size [exp]", "pov_rate"))
pred3 <- plot_model(fullmodel1, type = "pred", terms = c("pov_rate", "cregion"))
plot_model(fullmodel1, type = "pred", terms = c("gdp_size", "cregion"))
pred4 <-plot_model(fullmodel1, type = "pred", terms = c("gdp_size [exp]", "cregion"))

pred1


# creating the magnitude of coeficients graph
par(mfrow=c(1,1))
betas <- coefficients(fullmodel1)
IC <- confint(fullmodel1, level=0.95)
y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude of Coeficients",
     ylab="", axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2),
                                                             max(y.axis+.2)), cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1),
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T,
     cex.axis=1, mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5,
     cex.axis=1, mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")

# PREDICTORS TOGETHER
par(mfrow=c(2,2))
pred1
pred2
pred3
pred4

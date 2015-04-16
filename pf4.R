library(ggplot2)
library(data.table)

fish <- fread("./data/Fish2.csv")

dim(fish)
names(fish)
summary(fish)
str(fish)

## fapctor
##fish[, c(1:4) := lapply(.SD, as.factor), .SD = c(1:4)]
#fish[, time := relevel(ordered((time), ref = "12")]
fish[, popn := relevel(as.factor(popn), ref = "refer")]
fish[, conc := relevel(as.factor(conc), ref = "low")]

fish[, .N ,by = list(popn, conc, time)][order(popn, conc, time)]
fish[, mean(lncu) ,by = list(popn, conc, time)][order(popn, conc, time)]

## graphics
ggplot(fish, aes(lncu)) + geom_histogram()
mean(fish$lncu)

pairs(fish)
pairs(~ copper + lncu + time, data = fish)

str(fish)

## slide 1
ggplot(fish, aes(popn, lncu)) + geom_boxplot() + facet_wrap( ~conc + time, ncol = 2) + ggtitle("boxplot of lncu vs popn on different facets of conc and time")

## slide 3
comment <- function() {
a = 'Exposed fish population has been exposed to elevated copper concentrations for several decades. Reference fish population has not been expose to copper concentrations.'
a = 'copper concentrations were significantly less in the exposed fish population than the reference fish population .'
a = 'With the same experient expose duration (12/24 hours), fish exposed in higher concentration of radioactive copper isotope contains significantly more copper in their tissue than fish exposed in lower copper concentration'
a = 'for reference fish population, the group with longer experient duration (24 hours) has slight higher copper concentration in their tissue than the grouop with shorter experient duration (12 hours).'
a = 'For exposed fish population, the group experimented in high copper concentration with longer experient duration (24 hours) has slightly more copper in their tissue than the group experimented in high copper concentration but shorter duration (12hours), however,  the group experimented in lower copper concentration with longer experient duration has slightly less copper in their tissue than the group experimented in lower copper concentration but shorter duration.'
}


## model
lr <- lm(lncu ~ time + popn + conc, data = fish)
summary(lr)

## leverage
lev <- hat(model.matrix(lr))
plot(lev)

?model.matrix

plot(model)

## slide 3
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(lr)


plot(lr, which = 1)
plot(lr, which = 2)
plot(lr, which = 3)


## slide 4
comment <- function() {
a <- 'Model: lncu = -1.5202 + 0.0066 * time -0.5368 * popnexposed + 0.5260 * conchigh
popnexposed = {0 for reference population, 1 for exposed population}
conchigh = {0 for low copper concentration experiment, 1 for high copper concentration experiment}
lncu is log(concentration)
time is not significant with high p value of 0.196. popn and conc are significant.
R^2 is 0.567, the predictors somewhat explains the response variable.
'
a <- 'Residual vs. Fitted. The mean residual does not change much with the fitted values. However, the spread of residuals varying at different fitted values shows some non-constant variance, which indicates Heteroskedasticity. There are several big asymmetric positive residuals, which shows a little non-normality/skewness.'
a <- 'Scale-Location. Lowess fit of squre root of standardized residuals has a slightly downward slope, which clearly indicates some Heteroscedasticity in raw data.'
a <- 'Normal Q-Q. The divergence of standardized residuals from theoratical quantiles shows a strong indication of non-normaility.'
a <- 'biggest Residual coming from low conc and exposed population. the next 3 biggest are from low conc refer population. cannot see any obvious reason.'
}

names(lr)

fish$residuals <- lr$residuals
fish$fitted <- lr$fitted.values
## slid 2 maybe
ggplot(fish, aes(residuals)) + geom_histogram()
a <- c('right skewness')

fish[abs(residuals) > 0.7]

ggplot(fish, aes(copper)) + geom_histogram()



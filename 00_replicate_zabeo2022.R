# replicate methodology of Zabeo et al. 2022
#
# Ordered weighted average based grouping of nanomaterials with Arsinh
# and dose response similarity models
#
# immobilization of Daphnia magna


library(tidyverse)
library(readxl)
library(ToxicR)

rm(list=ls())

set.seed(seed=45056)

d <- read_excel(path="C:/Users/vom8/OneDrive - CDC/+My_Documents/MyLargeWorkspace Backup/Self-Study/Replications/Zabeo 2022/data.xlsx",
                sheet=1,
                col_names=T)

# Arsinh transform size, then scale

# (functional, not multiplicative) inverse hyperbolic sine
# https://mathworld.wolfram.com/InverseHyperbolicSine.html

arsinh <- function(z){
  arcsinh <- log(z + sqrt(1 + z^2))
  return(arcsinh)
}

d$size_transform <- arsinh(d$'Size (nm)')

scale01 <- function(x){
  scaled <- (x - min(x))/(max(x) - min(x))
  return(scaled)
}

d$size_transform_scale01 <- scale01(d$size_transform)

#recreate fig2
d$name <- as.factor(d$name)

p <- ggplot(data=d, aes(x=log10(`Dose (mg/L)`), y=log10(`Response (%)`))) +
  geom_point()

p + facet_grid(rows=vars(name)) #close enough

# figure 4
sizes <- distinct(d, name, size_transform_scale01)
sizes <- column_to_rownames(sizes, var="name")
distances <- dist(sizes[1]) #these are scaled and transformed
                            #figure4 is on original scale

# values from their figure for santos80 vs. sovova50
(1.57-0)/(2.31-0) #0.6796537 - close enough to my 0.6780986



# fit D-R curves
# they use PROAST, I'll try ToxicR
subset <- filter(d, name=="Kim_40") %>% select(`Dose (mg/L)`, `Response (%)`)
subset$log10dose <- log10(subset$`Dose (mg/L)`)
subset$log10response <- log10(`Dose (mg/L)`)

fit <- ma_continuous_fit(D=subset$`Dose (mg/L)`, 
                         Y=subset$`Response (%)`,
                         fit_type = "MLE")
fit
plot(fit)
cleveland_plot(fit)
MAdensity_plot(fit)

fit2 <- single_continuous_fit(D=subset$`Dose (mg/L)`, 
                              Y=subset$`Response (%)`,
                              fit_type = "mle",
                              model_type="exp-5",
                              distribution="normal") #normal-ncv and lognormal do not fit
plot(fit2) #visually similar to the paper, but paper used exp4


# plots are in log-log; were the curves fit to original data, log-log, trans/scale?
test <- data.frame(x=seq(from=0, to=25, by=0.5))
test$y <- 0.0216 * (4756.92105 - (4756.92105-1)*exp(-0.2357*test$x)) #Kim_40 model

plot(test$x, test$y)

p1 <- ggplot(data=filter(d, name=="Kim_40"), aes(x=`Dose (mg/L)`, y=`Response (%)`)) + 
  geom_point()
p2 <- ggplot(data=test, aes(x=x, y=y)) + geom_point()
  p1+geom_point(data=test, aes(x=x, y=y, color="red"))
# so fits are on original scale

  
### try an exp5 for comparison
subset <- filter(d, name=="Seo_40") %>% select(`Dose (mg/L)`, `Response (%)`)

fit3 <- single_continuous_fit(D=subset$`Dose (mg/L)`, 
                              Y=subset$`Response (%)`,
                              fit_type = "mle",
                              model_type="exp-5",
                              distribution="lognormal") #normal doesn't fit
plot(fit3)



#try PROAST
library(proast70.0)
subset <- filter(d, name=="Kim_40") %>% select(`Dose (mg/L)`, `Response (%)`)
f.proast(as.data.frame(subset)) #E4-CED fit is pretty close to the paper's
#a=0.0216   exact match to 4 decimals
#b=0.2349   0.2357 in paper
#c=4760     4756.92105 in paper
# PROAST plot does use log10 dose

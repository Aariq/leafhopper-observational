rm(list=ls()) # Clear R environment for this analysis
set.seed(25)

#------------------------------------------------------------
# Simulate data that we will analyze
#------------------------------------------------------------

# Simulation parameters that we will try to estimate
N = 500 # number of plants

# Mean daily growth rate of plants and sd
# there is variance from day to day
plant.r.mean = 1.015
plant.r.sd = 0.008

# Random effect for growth rate of plants
# Some plants just have better growth than others
random.effect.sd = 0.005
plant.id.effects = rnorm(N,0,random.effect.sd) #The actual adjustment for each plant

# Conduct the simulation
time = 60 # days

# Generate some random daily temperatures
daily.temperature = as.numeric(scale(diffinv(rnorm(time))))


#This is the effect of daily temperature on plant growth
temperature.effect.B1 = 0.005
temperature.effect.B2 = -0.001
effect.est = temperature.effect.B1*daily.temperature + temperature.effect.B2*daily.temperature^2
plot(effect.est~daily.temperature)

growth.matrix = matrix(NA,nrow = time, ncol = N)

# Random initial plant sizes
initial.plant.sizes = runif(N, 0.8, 1)
growth.matrix[1,] = initial.plant.sizes

# Simulate growth dynamics for each plant
for (i in 1:N){

    for (t in 2:time){
        temp.today = daily.temperature[t]

        #daily growth rate (with random effect for plant added in)
        r.today = rnorm(1,plant.r.mean + plant.id.effects[i], plant.r.sd)

        #adjustment for weather effect today
        r.today = r.today + (temperature.effect.B1*temp.today + temperature.effect.B2*temp.today^2)

        # Size today depends on size yesterday multiplied by...
        growth.matrix[t,i] = growth.matrix[t-1,i]*r.today
    }
}

# Plot the growth trajectories for each plant
matplot(growth.matrix, type = "l", ylab = "Plant size", xlab = "Day of Season")

#------------------------------------------------------------
# Analyze the simulated data
#------------------------------------------------------------

# Raw data is size of each plant on each day of season.
# Data is also in a matrix format (each column is a different plant, each row is a point in time)
# This isn't ideal for analysis.
# We want to first calculate daily growth rates for each plant.
# Then we want to restructure the data into a format that we can analyze with lmer
growth.df = data.frame(growth.matrix)
colnames(growth.df) = paste("Plant",seq(1,N))

# Calculate daily growth rates for each plant
# This is the data we will analyze
r.df = growth.df[1:(time-1),]
for (t in 1:(time-1)){
    r.df[t,] = growth.df[t+1,]/growth.df[t,]
}

# Plot the daily growth rates for each plant
matplot(r.df, type = "l", ylab = "Plant size", xlab = "Day of Season")

r.df$time = 1:(time-1) # Add a column for time
r.df$temperature = daily.temperature[1:(time-1)] # Add a column for daily temp

# Restructure the data using the "melt" function
library(reshape)
mdata <- melt(r.df, id=c("time", "temperature"))
colnames(mdata)[3] = "plantid"
colnames(mdata)[4] = "r"
head(mdata) #Looks good!

#------------------------------------------------------------
# The actual statistical analyses
#------------------------------------------------------------

# Basic linear model (no random effects or weather effects)
plant.lm1 = lm(r~1, data = mdata)
summary(plant.lm1)
coef(plant.lm1)
# Estimated daily growth rate is 1.013
# This is actually pretty close to our true parameter!

# Weather effects
plant.lm2 = lm(r~temperature + I(temperature^2), data = mdata)
summary(plant.lm2)
coef(plant.lm2)
# Estimated daily growth rate is 1.014
# This is actually closer to our true parameter!
# Weather effects are 0.00387 (not bad) and -0.0004939 (not bad, but a bit off)

# Random plant ID effects and fixed Weather effects
library(lme4)
plant.lmer = lmer(r~temperature + I(temperature^2) + (1|plantid), data = mdata)
tidy(plant.lmer)
# Estimated daily growth rate is 1.014
# This is actually closer to our true parameter!
# Weather effects are 0.00387 (not bad) and -0.0004939 (not bad, but a bit off)
# Random effect for plant id is 2.564e-05 (this is lower than the true estimate)



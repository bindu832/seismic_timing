# 583 Project ----
# 9 Apr 2019



# Set working directory ----
# setwd("/Users/bindu/583_project")





# Load libraries ----
library(data.table)
library(mgcv)
library("scatterplot3d")
library(splines)




# Load data ----
source("seismictimingsfull.R")

st <- copy(data.table(seismictimingsfull))
names(st)





# Visualize the data ----
par(mfrow=c(1,2))
with(st, scatterplot3d(x, y, z, mar = c(5, 3, 0, 3) + 0.1))
with(st, scatterplot3d(x, y, z, angle = 90, mar = c(5, 3, 0, 3) + 0.1))


# See effect of one variable on z at a time
st1 <- copy(st)
st1$y <- NULL

st2 <- copy(st)
st2$x <- NULL

st1[, z := mean(z), by = x]
st1 <- unique(st1)

st2[, z := mean(z), by = y]
st2 <- unique(st2)

plot(st1) # z vs x
plot(st2) # z vs y










#### Models -----


# Multiple Linear Regression ----
seismic.lm <- lm(z ~ x + y, data=st)
summary(seismic.lm)

par(mar = c(2.2, 2.2, 2.2, 2.2))
par(mfrow = c(2,2))
plot(seismic.lm)
AIC(seismic.lm)
# Adjusted R square is very low, linear regression is not a good fit.







# Bivariate spline ---- 
seismic <- lm(z ~ bs(x, knots=seq(0.126, 7.999, length=5)) 
              +bs(y, knots=seq(0.001, 8.3, length=5)), data = st)
summary(seismic) # Better but still not good
plot(seismic)
AIC(seismic)


# Bivariate spline considering interaction between x and y ---- 
seismic2 <- lm(z ~ bs(x, knots=seq(0.126, 7.999, length=5)) 
              :bs(y, knots=seq(0.001, 8.3, length=5)), data = st)

summary(seismic2)
plot(seismic2)
AIC(seismic2)










# GAM with default Gaussian family ----
# The default smoothing functions s() fit penalized cubic regression splines.
st.am <- gam(z ~ s(x) + s(y)
              , data = st)
st.am


par(mar = c(2.5, 2.5))
par(mfrow = c(1,2))
plot(st.am, residuals = TRUE, cex = 2)

par(mfrow=c(1,2))
vis.gam(st.am, plot.type="contour") # alternative: type="contour" 
vis.gam(st.am, theta = 210, phi = 40)

# qqnorm(resid(st.am)) 
# qqline(resid(st.am))

par(mfrow = c(2,2))
gam.check(st.am)

summary(st.am)
AIC(st.am)










# GAM with Gamma family ----
st.gam <- gam(z ~ s(x) + s(y)
              , data = st
              , family = Gamma(link = "log"))
st.gam

par(mfrow = c(1,2))
plot(st.gam, residuals = TRUE, cex = 2)

par(mfrow=c(1,2))
vis.gam(st.gam, plot.type="contour") # alternative: type="contour" 
vis.gam(st.gam, theta = 210, phi = 40)



par(mfrow = c(2,2))
gam.check(st.gam)

summary(st.gam)
AIC(st.gam)








# GAM with thin plate splines ----
st.tpsl <- gam(z ~ s(x, y, k = 50), data = st
               , family = Gamma(link = "log")) 
st.tpsl

par(mfrow=c(1,1))
plot(st.tpsl, residuals = TRUE, cex = 2)


par(mfrow=c(1,2))
vis.gam(st.tpsl, plot.type="contour", color = "heat") # alternative: type="contour" 
vis.gam(st.tpsl, theta = 210, phi = 40)

par(mar = c(2.2, 2.2, 2.2, 2.2))
par(mfrow=c(2,2))
gam.check(st.tpsl)
summary(st.tpsl)
AIC(st.tpsl)









# GAM with Tensor-Product Splines -----
st.tesl <- gam(z ~ te(x, y), data = st
               , family = Gamma(link = "log")) 
st.tesl

par(mfrow=c(1,1))
plot(st.tesl, residuals = TRUE, cex = 4)

par(mfrow=c(1,2))
vis.gam(st.tesl, plot.type="contour") # alternative: type="contour" 
vis.gam(st.tesl, theta = 210, phi = 40)

par(mar = c(2.2, 2.2, 2.2, 2.2))
par(mfrow=c(2,2))
gam.check(st.tesl)
summary(st.tesl)
AIC(st.tesl)








# Clear the environment ----
# rm(list = ls())
# gc()
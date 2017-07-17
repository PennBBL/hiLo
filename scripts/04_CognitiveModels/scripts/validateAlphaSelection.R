# AFGR May 4 2017
# This script is going to be used to run simulate data with different noise
# and corellation structures. The alpha tuning function will then be used to
# observe any patterns in data with known structure.

# First thing we need to do is load our library(s)
source('/home/adrose/hiLo/scripts/04_CognitiveModels/functions/functions.R')
install_load('foreach', 'doParallel', 'glmnet', 'bootstrap', 'psych')

# Cretae the simulated data here

Phi <- matrix(runif(25,-0.1,0.1),5,5)

for (i in 1:5) {
    for (j in 1:5) {
        Phi[i,j] <- Phi[j,i]
    }}

diag(Phi) <- 1

fx <- matrix(runif(500,-0.1,0.1),100,5)
fx[,1] <- 0.9

x1 <- sim.structure(fx=fx,Phi=Phi,n=5000)$observed
x1AlphaVals <- returnOptAlpha(x1[,2:100], x1[,1], nCor=30)

fx <- matrix(runif(500,-0.2,0.2),100,5)
fx[,1] <- 0.8

x2 <- sim.structure(fx=fx,Phi=Phi,n=5000)$observed
x2AlphaVals <- returnOptAlpha(x2[,2:100], x2[,1], nCor=30)

fx <- matrix(runif(500,-0.25,0.25),100,5)
fx[,1] <- 0.7

x3 <- sim.structure(fx=fx,Phi=Phi,n=5000)$observed
x3AlphaVals <- returnOptAlpha(x3[,2:100], x3[,1], nCor=30)

fx <- matrix(runif(500,-0.3,0.3),100,5)
fx[,1] <- 0.6

x4 <- sim.structure(fx=fx,Phi=Phi,n=5000)$observed
x4AlphaVals <- returnOptAlpha(x4[,2:100], x4[,1], nCor=30)

fx <- matrix(runif(500,-0.35,0.35),100,5)
fx[,1] <- 0.5

x5 <- sim.structure(fx=fx,Phi=Phi,n=5000)$observed
x5AlphaVals <- returnOptAlpha(x5[,2:100], x5[,1], nCor=30)

pdf('simulatedDataAlphaHist.pdf')
hist(x1AlphaVals)
hist(x2AlphaVals)
hist(x3AlphaVals)
hist(x4AlphaVals)
hist(x5AlphaVals)
dev.off()

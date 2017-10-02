install_load('psych', 'sas7bdat', 'mi', 'methods', 'doParallel')

x <- read.csv("/home/tymoore/MGI2_CNB.csv")

attach(x)

x <- data.frame(
test_sessions.bblid,
CPF.IFAC_TOT,
ac_FMEM,
CPW.IWRD_TOT,
ac_VMEM,
ac_SPA,
ac_ABF_WP,
SVT,
ac_SMEM,
ac_EMO,
ac_ATT,
LNB2.LNB_MCR,
ac_LAN,
CPF.IFAC_RTC,
t_FMEM,
CPW.IWRD_RTC,
t_VMEM,
t_SPA,
t_ABF_WP,
SVTCRT,
t_SMEM,
t_EMO,
t_ATT,
LNB2.LNB_MRTC,
t_LAN)

detach(x)

# Now impute missing data 
toImpute <- x[,2:25]
options(mc.cores=4)
toImpute <- mi(toImpute, parallel=T, seed=16)
output <- complete(toImpute, 4)
output.1 <- output[[1]][,1:24]
output.2 <- output[[2]][,1:24]
output.3 <- output[[3]][,1:24]
output.4 <- output[[4]][,1:24]
output <- (output.1 + output.2 + output.3 + output.4)/4
x[,2:25] <- output

x[,2:25] <- scale(x[,2:25])

eff <- scale(x[,2:13] - x[,14:25])

x <- data.frame(x,eff)

colnames(x)[26:37] <- c(
"CPF_EFFICIENCY",
"FMEM_EFFICIENCY",
"IWRD_EFFICIENCY",
"VMEM_EFFICIENCY",
"SPA_EFFICIENCY",
"ABF_WP_EFFICIENCY",
"SVT_EFFICIENCY",
"SMEM_EFFICIENCY",
"EMO_EFFICIENCY",
"ATT_EFFICIENCY",
"LNB2_EFFICIENCY",
"LAN_EFFICIENCY")

x$Overall_Accuracy_no_pvrt <- scale(rowMeans(x[,2:13]))
x$Overall_Efficiency <- scale(rowMeans(x[,26:37]))

write.csv(x,"/home/adrose/dataPrepForHiLoPaper/data/mgiData/MGI_FScores.csv",na="", quote=F, row.names=F)

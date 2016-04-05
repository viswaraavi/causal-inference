# Load the libraries 
library(vars)
library(urca)
library(pcalg)
# Read the input data 
data1 <- read.csv("C:/Users/viswa/Desktop/data.csv",header=TRUE)
head(data1)
# Build a VAR model 
var.model <- VAR(data1,type ="const",lag.max=10,ic="SC" )

# Extract the residuals from the VAR model 
res <- resid(var.model)

# Check for stationarity using the Augmented Dickey-Fuller test 
s1 <- ur.df(res[,1],lags=1)
summary(s1)
s2 <- ur.df(res[,2],lags=1)
summary(s1)
s3 <- ur.df(res[,3],lags=1)
summary(s3)
# Check whether the variables follow a Gaussian distribution

ks.test(as.numeric(res[,1]),"pnorm")
ks.test(as.numeric(res[,2]),"pnorm")
ks.test(as.numeric(res[,3]),"pnorm")
qqnorm(as.numeric(res[,1]))
qqnorm(as.numeric(res[,2]))
qqnorm(as.numeric(res[,3]))

# Write the residuals to a csv file to build causal graphs using Tetrad software
write.csv(res,file="res.csv")
# PC algorithm
suffStat=list(C=cor(res), n=1000)
pc_fit <- pc(suffStat, indepTest=gaussCItest, alpha=0.05, labels=colnames(res), skel.method="original")
plot(pc_fit, main="PC Output")

# LiNGAM algorithm
lingam_fit <- LINGAM(res)
show(lingam_fit)

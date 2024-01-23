

library("openxlsx")
library("parallel")
library("Matrix")
options(mc.cores=detectCores())
source("functions.R")

DATA= read.xlsx("/Users",detectDates=TRUE) 
# restrict dataset
DATA=na.omit(DATA)
DATE = DATA[,1]
Y = DATA[,-1]
k = ncol(Y)
NAMES = colnames(Y)

### STATIC CONNECTEDNESS APPROACH
nlag = 2 # VAR(4)
nfore = 10 # 10-step ahead forecast
lasso_full = LassoVAR(Y, p=nlag)
CV_full = GFEVD(lasso_full$B, lasso_full$Q, n.ahead=nfore)$GFEVD
rownames(CV_full)=colnames(CV_full)=NAMES
print(DCA(CV_full)$TABLE)
write.table(DCA(CV_full)$TABLE,file="")
write.table(DCA(CV_full)$TABLE,file="",sep=",",quote=F)

### DYNAMIC CONNECTEDNESS APPROACH
t = nrow(Y)
space = 48 + nlag 
t0 = t-space

net = to = from = matrix(NA, ncol=k, nrow=t0)
gfevd = ct = npso = array(NA, c(k, k, t0))
total = matrix(NA, ncol=1, nrow=t0)
colnames(gfevd)=rownames(gfevd)=colnames(ct)=rownames(ct)=colnames(Y)
for (i in 1:t0){
  lasso_var = LassoVAR(Y[i:(space+i-1),], p=nlag)
  gfevd[,,i] = GFEVD(lasso_var$B, lasso_var$Q, n.ahead=nfore)$GFEVD
  vd = DCA(gfevd[,,i])
  ct[,,i] = vd$CT
  to[i,] = vd$TO
  from[i,] = vd$FROM
  net[i,] = vd$NET
  #npso[,,i] = vd$NPSO
  total[i,] = vd$TCI
  if (i%%100==0) print(paste0(round(100*i/t0,2),"%"))
}

### DYNAMIC TOTAL CONNECTEDNESS
date = DATE[-c(1:space)]
par(mfrow = c(1,1), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
plot(date,total, type="l",xaxs="i",col="grey20", las=1, main="",ylab="",ylim=c(floor(min(total)),ceiling(max(total))),yaxs="i",xlab="",tck=0.01)
grid(NA,NULL,lty=1)
polygon(c(date,rev(date)),c(c(rep(0,nrow(total))),rev(total)),col="grey20", border="grey20")
box()


total.c =  total
# scale the total connectedness and averaged lambda into 0 and 1
total.c = (total.c - min(total.c))/(max(total.c) - min(total.c))


write.table(total.c,file='/Users',sep=",")
write.table(from,file='/Users',sep=",")
write.table(to,file='/Users',sep=",")







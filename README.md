** Oil prices and systemic financial risk: A complex network analysis **

Introduction
This project provides R implementations of all the models in our paper. In addition, it contains some explanations of the models for the reader to refer to without wasting too much time on model training or retraining.

Requirement
R Studio Version 1.4.1717

Data and code

In this study, WTI crude oil futures prices were collected from the FRED.com website (https://www.FRED.com/). The rest of the data was obtained from csmar (https://www.csmar.com/). All the datasets are stored in the directory /data/, details of which are given below:

Data: the data used in this paper is included in the data folder.
Total return.xlsx contains logarithmic returns for 59 financial institutions and WTI with 6 macro state variables. These data can be used to calculate the CoVaR values for the 59 financial institutions, and using the CoVaR values, the high-dimensional risk spillover network can be further constructed.
Corporate Accounting Variables.xlsx contains financial variables for 59 financial institutions, which are used to calculate the results in Table 2.
Macroeconomic Variables.xlsx contains a number of macroeconomic indicators used to calculate the macroeconomic impact of systemic financial risk.

Code: in the code folder includes the R language programme code used in this paper.
CoVaR.R is used to calculate CoVaR values for 59 financial institutions;
function.R and TCI.R are the codes for the Lasso-VAR model, which are used to construct the high-dimensional risk spillover network;
figure heatmap.R and figure1-3.R are used to construct line graphs and heatmaps in the text;
VAR-plot-total.R is used to depict the vector autoregressive model in the text.

Each code will be presented separately next for the reader's reference.

Name of file: CoVaR.R
Description: Estimates the Value at Risks (VaRs) of 59 financial institutions using moving window estimation based on seven macro state variables. The estimated CoVaR using linear quantile (green line) is also shows in this code.
Data file : 59_firms_returns_and_macro.csv, Total return.xlsx
Input : log returns of 59 financial institutions and 6 macro state variables
Output : estimated CoVaRs for 59 financial institutions

### R codes
rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
setwd("/Users")

library(dplyr)
library(readr)
library(reshape2)
library(lubridate)

library(SparseM)
libraries=c("quantreg")
library(Matrix)

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#Load and aggregate data weekly 
daily_returns<-read.csv("/Users", header = TRUE,sep = ",")

returns_weekly <- daily_returns %>% 
  melt(id.vars = "Date") %>% 
  rename("returns" = "value")%>%
  mutate(week = floor_date(as.Date(Date, format="%Y-%m-%d"), unit="week")) %>% select(Date, week, everything())  %>% 
  group_by(week, variable) %>% 
  summarise(mean(returns)) %>% 
  tidyr::spread(key = "variable", value = "mean(returns)")%>% 
  ungroup() 

#state variables -- shift 1 lag 

state_var_returns<-read.csv("/Users", header = TRUE,sep = ",")

state_var_weekly <-  state_var_returns %>% 
  melt(id="Date")%>% 
  mutate(week = floor_date(as.Date(Date, format = "%Y-%m-%d"), unit = "week" )) %>% select(Date, week, everything())%>% 
  group_by(week, variable) %>% 
  summarise(mean(value)) %>%
  tidyr::spread(key = "variable", value = "mean(value)") %>% ungroup()

write.csv(returns_weekly, file = "")
write.csv(state_var_weekly, file = "weekly.csv")


####The generated weekly.csv was used to further calculate the VaR values of 59 financial institutions.

rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
setwd("/Users")

library(SparseM)
libraries=c("quantreg")
library(Matrix)

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data       = read.csv("/Users", header = TRUE)

dt <- as.Date(data[,1], format = "%Y-%m-%d")


# read the macro state variables 
data_m     = as.matrix(data[, 62:67]) 

# read the log returns of 51 firms
data_y     = as.matrix(data[, 2:60]) 

# qantile level
tau        = 0.05

nncol      = ncol(data_y)
nnrow      = nrow(data_y)
lengthfull = nnrow

# window size is 50
winsize    = 48
VaR        = matrix(0, ncol = nncol, nrow = (lengthfull - winsize))

# start the moving window VaR prediction, store the predict values
for (j in 1:nncol) {
  for (i in 1:(lengthfull - winsize)) {
    ycut   = data_y[i:(i + winsize), j]
    xcut   = data_m[i:(i + winsize), ]
    xxcut  = matrix(0, nrow(xcut), ncol(xcut))
    # standardize macro state variables
    for (k in 1:ncol(xcut)) {
      xxcut[, k] = (xcut[, k] - min(xcut[, k]))/(max(xcut[, k]) - min(xcut[, k]))
    }
    fit       = rq(ycut ~ xxcut, tau)
    pre       = predict(fit, quantiles = tau)
    VaR[i, j] = pre[length(pre)]
  }
}
VaR         = round(VaR, digits = 9)
colnames(VaR) <- colnames(data)[2:60]
write.csv(VaR, file = "VaR.csv")


#### further calculated CoVaR values for 59 financial institutions using the generated VaR.csv.


rm(list = ls(all = TRUE))
graphics.off()

# set the working directory
setwd("/Users")

library(SparseM)
libraries  = c("quantreg")
library(Matrix)

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data1       = read.csv("/Users", header = TRUE)

data2      = read.csv("/Users", header = TRUE)

dt <- as.Date(data1[,1], format = "%Y-%m-%d")


# read the macro state variables 
data_m     = as.matrix(data1[, 62:67]) 
# read the log returns of 51 firms
data_y     = as.matrix(data2[, 1:60]) 

# qantile level
tau        = 0.05

nncol      = ncol(data_m)
nnrow      = nrow(data_m)
lengthfull = nnrow

# window size is 50
winsize    = 48

# extracting the VaR of IDBI Bank
VaR_ref <- -subset(data2, select = c(60)) 
# Removing ref bank from VaR data frame
VaR_withoutref <- data2[,-60] 
# Removing last window from the macro variables dataframe
data_Mnew <- data_m[1:(nnrow-48),]
# Combining the macro variables and VaR ref dataframes
Combined_data <-cbind(data_Mnew, VaR_ref) 
Combined_data1 = as.matrix(Combined_data[, 1:7])

# standardize only the macro variables
cbdcut  = Combined_data1
for (k in 1:ncol(data_m)) {
  cbdcut[, k] = (cbdcut[, k] - min(cbdcut[, k]))/(max(cbdcut[, k]) - min(cbdcut[, k]))
}

CV_q    = matrix(0, ncol = ncol(VaR_withoutref), nrow = (lengthfull-winsize))

# Calculating at the 0.05 quantile
for(l in 1:ncol(VaR_withoutref)){
  Var_temp   = VaR_withoutref[,l]
  CoVaR_qth  = rq(Var_temp ~ cbdcut, tau)
  CV_q[,l]   = predict(CoVaR_qth, quantiles = tau)
}

colnames(CV_q) <- colnames(VaR_withoutref)

df_date = as.data.frame(dt[winsize+1:nnrow])
df_CoVaR = cbind(as.data.frame(df_date[1:(nnrow-winsize),1]), as.data.frame(CV_q ))
write.csv(df_CoVaR, file = "CoVaR.csv")


Name of file: code and data
Description:This part of the R code is a Lasso-VAR model for constructing high-dimensional risk spillovers and generating TCIs.Note that the code generates the connectivity matrices in Table 1 of the paper, and needs to be used with the Gephi software to construct the network in Figure 4 of the paper.
code file : function.R and TCI.R
Input : CoVaR values for 59 financial institutions, CoVaR.csv
Output : estimated CoVaRs for 59 financial institutions
##R codes

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
write.table(DCA(CV_full)$TABLE,file=" The connectedness matrix .csv",sep=",",quote=F)

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
total.c =  total
# scale the total connectedness and averaged lambda into 0 and 1
total.c = (total.c - min(total.c))/(max(total.c) - min(total.c))

write.table(total.c,file='TCI.csv',sep=",")
write.table(from,file='Incoming,csv',sep=",")
write.table(to,file='Outgoing.csv',sep=",")


Name of file: code and data
Description:This part of the R code is used to plot Figure 5.
code file : figure heatmap.
Input : Pagerank values of 59 financial institutions.
Output :Figure 5

##R codes


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("pheatmap","reshape2","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set the working directory setwd('C:/...')

setwd("/Users")
data = read.csv("/Users")
Date<-data[,1]
con<-as.matrix(data)[,-1]

# apply a hard thresholding to make the major connections more clearly
con1 = ifelse(abs(con) >= mean(con[order(con, decreasing = T)[1:590]]), con,mean(con[order(con, decreasing = T)[1:590]]) )
data1<-cbind(Date,con1)
write.csv(data1,file = "")


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("pheatmap","reshape2","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set the working directory setwd('C:/...')
setwd("/Users")
data = read.csv("")
data1<-data[,-1]
data_m <- melt(data1, id.vars="Date")
head(data_m)
mytheme=theme_classic()+
  theme(axis.text.x = element_text( hjust = 1, vjust = 1,angle = 0,
                                    size=rel(1.0)),
        axis.text.y = element_text( hjust = 0.5, vjust = 0.5,
                                    size=rel(0.8)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.title=element_text(size=rel(1),hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## draw heatmap
ggplot(data_m, aes(x=Date, y=variable, fill=value))+geom_tile()+
  ggtitle("evc")+mytheme+scale_fill_gradient(low = "skyblue", high = "red")


Name of file: code and data
Description:This part of the R code is used to plot Figures 1-3.Note that since the line graphs all use similar code, only the running code for Figure 1 is shown; when plotting Figures 2 and 3, the Incoming and Outgoing results need to be averaged over different financial sectors in advance, and then the code is run.
code file : figure1-3.
Input :TCI.csv,Incoming.csv,Outgoing.csv,WTI.csv
Output :figure1-figure3

##R codes

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("ggplot2","tseries","patchwork","scam")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# set the working directory setwd('C:/...')
setwd=("/Users")

f            = read.csv("/Users/")
dt           = as.Date(f[, 1], format = "%Y-%m-%d")

f1            = read.csv("/Users/")
dt1           = as.Date(f1[, 1], format = "%Y-%m-%d")

# total links of four groups
total.c   = as.vector(f[, 10])

# WTI
wti  = as.vector(f1[, 2])

##ggplot2,WTI,TCI
p1<-ggplot(f1,aes(x = dt1, y =wti)) +
  geom_line(color = "red",size=0.5) +
  labs(x = "WTI", y = "Price") +theme_light()+geom_hline(yintercept = 0, col = "black",size=0.3)+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p2<-ggplot(f, aes(x = dt, y =total.c)) +
  geom_line(color = "red",size=0.5) +
  labs(x = "TCI", y = "Index") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p1/p2


Name of file: code and data
Description:This part of the R code is a VAR model to measure the impact of TCI on macroeconomic variables. Note that the data used for this model is Macroeconomic Variables.xlsx, which needs to be seasonally adjusted before executing the code run; the plotting package is ggplot2; since similar code is used for each macroeconomic variable test, only the code run for EPU outlook is shown; the robustness test is also done using this part of the code, which requires the selection of the lag order P of the SC based on the results of the instruction lagselect$selection.
code file : VAR-plot-total.
Input : Macroeconomic Variables.csv
Output : Figure 6 to Figure 8.

##R codes

####ggplot2

########################EPU outlook#################################

rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("ggplot2","vars","patchwork")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

library("vars")
library("ggplot2")


df1<- read.csv("/Users")

lagselect <- VARselect(df1, lag.max = 5, type = "both")
lagselect$selection

var1 <- VAR(df1, p = 2, type = "const", season = NULL, exog = NULL)

df2<- read.csv("/Users/")

lagselect <- VARselect(df2, lag.max = 10, type = "const")
lagselect$selection

var2 <- VAR(df2, p = 6, type = "const", season = NULL, exog = NULL)


###outlook

irf1 <- irf(var2, impulse = "TCI", response = "outlook", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf2 <- irf(var2, impulse = "TCI", response = "outlook", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

outlook <- data.frame(x=c(1:37),y=unlist(irf1$irf), ymin1=unlist(irf1$Lower), ymax1=unlist(irf1$Upper),
                      ymin2=unlist(irf2$Lower), ymax2=unlist(irf2$Upper))

p1<-ggplot(outlook, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.005,0.008)+theme_light()+
  labs(title='Economic Outlook',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

###EPU

irf3 <- irf(var1, impulse = "TCI", response = "EPU", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf4 <- irf(var1, impulse = "TCI", response = "EPU", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

EPU <- data.frame(x=c(1:37),y=unlist(irf3$irf), ymin1=unlist(irf3$Lower), ymax1=unlist(irf3$Upper),
                  ymin2=unlist(irf4$Lower), ymax2=unlist(irf4$Upper))

p2<-ggplot(EPU, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.05,0.06)+theme_light()+
  labs(title='EPU',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

########## 4*8
p1+p2

![image](https://github.com/wks1234567/Oil-prices-and-systemic-financial-risk-A-complex-network-analysis/assets/87639837/7c80a236-6065-49ba-9265-d6aa87a1a18c)

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
write.csv(state_var_weekly, file = "")






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
write.csv(VaR, file = "")




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
write.csv(df_CoVaR, file = "")























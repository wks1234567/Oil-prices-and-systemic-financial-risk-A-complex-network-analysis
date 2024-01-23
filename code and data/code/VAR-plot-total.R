####ggplot2
#########################################################

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

##########################Consumption################################

df1<- read.csv("/Users")

lagselect <- VARselect(df1, lag.max = 5, type = "both")
lagselect$selection

var1 <- VAR(df1, p = 2, type = "const", season = NULL, exog = NULL)

df2<- read.csv("/Users/")

lagselect <- VARselect(df2, lag.max = 5, type = "const")
lagselect$selection

var2 <- VAR(df2, p = 3, type = "const", season = NULL, exog = NULL)


###CPI

irf1 <- irf(var1, impulse = "TCI1", response = "CPI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf2 <- irf(var1, impulse = "TCI1", response = "CPI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

CPI <- data.frame(x=c(1:37),y=unlist(irf1$irf), ymin1=unlist(irf1$Lower), ymax1=unlist(irf1$Upper),
                  ymin2=unlist(irf2$Lower), ymax2=unlist(irf2$Upper))

p1<-ggplot(CPI, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
 ylim(-0.15,0.05)+theme_light()+
  labs(title='CPI',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

###PPI

irf3 <- irf(var1, impulse = "TCI1", response = "PPI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf4 <- irf(var1, impulse = "TCI1", response = "PPI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

PPI <- data.frame(x=c(1:37),y=unlist(irf3$irf), ymin1=unlist(irf3$Lower), ymax1=unlist(irf3$Upper),
                  ymin2=unlist(irf4$Lower), ymax2=unlist(irf4$Upper))

p2<-ggplot(PPI, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.5,0.4)+theme_light()+
  labs(title='PPI',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

###CPI-food

irf5 <- irf(var1, impulse = "TCI1", response = "food", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf6 <- irf(var1, impulse = "TCI1", response = "food", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

food <- data.frame(x=c(1:37),y=unlist(irf5$irf), ymin1=unlist(irf5$Lower), ymax1=unlist(irf5$Upper),
                  ymin2=unlist(irf6$Lower), ymax2=unlist(irf6$Upper))

p3<-ggplot(food, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.5,0.3)+theme_light()+
  labs(title='Food',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

###CPI-clothes

irf7 <- irf(var2, impulse = "TCI2", response = "clothes", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf8 <- irf(var2, impulse = "TCI2", response = "clothes", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

clothes <- data.frame(x=c(1:37),y=unlist(irf7$irf), ymin1=unlist(irf7$Lower), ymax1=unlist(irf7$Upper),
                 ymin2=unlist(irf8$Lower), ymax2=unlist(irf8$Upper))

p4<-ggplot(clothes, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.1,0.1)+theme_light()+
  labs(title='Clothes',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

###CPI-traffic

irf9 <- irf(var2, impulse = "TCI2", response = "traffic", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf10 <- irf(var2, impulse = "TCI2", response = "traffic", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

traffic <- data.frame(x=c(1:37),y=unlist(irf9$irf), ymin1=unlist(irf9$Lower), ymax1=unlist(irf9$Upper),
                       ymin2=unlist(irf10$Lower), ymax2=unlist(irf10$Upper))

p5<-ggplot(traffic, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.5,0.2)+theme_light()+
  labs(title='Traffic',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


###CPI-live

irf11 <- irf(var2, impulse = "TCI2", response = "live", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf12 <- irf(var2, impulse = "TCI2", response = "live", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

live <- data.frame(x=c(1:37),y=unlist(irf11$irf), ymin1=unlist(irf11$Lower), ymax1=unlist(irf11$Upper),
                          ymin2=unlist(irf12$Lower), ymax2=unlist(irf12$Upper))

p6<-ggplot(live, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.1,0.05)+theme_light()+
  labs(title='Live',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


#######################
(p2+p1+p3)/(p4+p5+p6)

(p2+p1)/(p3+p4)/(p5+p6)

################### economic activity###############################
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



df3<- read.csv("/Users")

lagselect <- VARselect(df3, lag.max = 5, type = "both")
lagselect$selection

var3 <- VAR(df3, p = 2, type = "const", season = NULL, exog = NULL)

df4<- read.csv("/Users")

lagselect <- VARselect(df4, lag.max = 5, type = "both")
lagselect$selection

var4 <- VAR(df4, p = 2, type = "const", season = NULL, exog = NULL)


df5<- read.csv("/Users")

lagselect <- VARselect(df5, lag.max = 5, type = "both")
lagselect$selection

var5 <- VAR(df5, p = 4, type = "const", season = NULL, exog = NULL)


###Investment 

irf13 <- irf(var3, impulse = "TCI1", response = "investment", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf14 <- irf(var3, impulse = "TCI1", response = "investment", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

investment<- data.frame(x=c(1:37),y=unlist(irf13$irf), ymin1=unlist(irf13$Lower), ymax1=unlist(irf13$Upper),
                        ymin2=unlist(irf14$Lower), ymax2=unlist(irf14$Upper))

p1<-ggplot(investment, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.06,0.1)+theme_light()+
  labs(title='Investment',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


###consumption 

irf15 <- irf(var4, impulse = "TCI2", response = "consumption", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf16 <- irf(var4, impulse = "TCI2", response = "consumption", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

consumption<- data.frame(x=c(1:37),y=unlist(irf15$irf), ymin1=unlist(irf15$Lower), ymax1=unlist(irf15$Upper),
                        ymin2=unlist(irf16$Lower), ymax2=unlist(irf16$Upper))

p2<-ggplot(consumption, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.015,0.02)+theme_light()+
  labs(title='Consumption',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


###PI

irf17 <- irf(var4, impulse = "TCI2", response = "PI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.95)

irf18 <- irf(var4, impulse = "TCI2", response = "PI", boot = T, cumulative = FALSE, n.ahead = 36,runs=1000,ci=0.68)

PI<- data.frame(x=c(1:37),y=unlist(irf17$irf), ymin1=unlist(irf17$Lower), ymax1=unlist(irf17$Upper),
                        ymin2=unlist(irf18$Lower), ymax2=unlist(irf18$Upper))

p3<-ggplot(PI, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.5,0.5)+theme_light()+
  labs(title='PI',x = "Months", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


###GDP 

irf19 <- irf(var5, impulse = "TCI", response = "GDP", boot = T, cumulative = FALSE, n.ahead = 12,runs=1000,ci=0.95)

irf20 <- irf(var5, impulse = "TCI", response = "GDP", boot = T, cumulative = FALSE, n.ahead = 12,runs=1000,ci=0.68)

GDP<- data.frame(x=c(1:13),y=unlist(irf19$irf), ymin1=unlist(irf19$Lower), ymax1=unlist(irf19$Upper),
                        ymin2=unlist(irf20$Lower), ymax2=unlist(irf20$Upper))

p4<-ggplot(GDP, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.015,0.015)+theme_light()+
  labs(title='GDP',x = "Quarters", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")


###unemployment 

irf21 <- irf(var5, impulse = "TCI", response = "unemployment", boot = T, cumulative = FALSE, n.ahead = 12,runs=1000,ci=0.95)

irf22 <- irf(var5, impulse = "TCI", response = "unemployment", boot = T, cumulative = FALSE, n.ahead = 12,runs=1000,ci=0.68)

unemployment<- data.frame(x=c(1:13),y=unlist(irf21$irf), ymin1=unlist(irf21$Lower), ymax1=unlist(irf21$Upper),
                        ymin2=unlist(irf22$Lower), ymax2=unlist(irf22$Upper))

p5<-ggplot(unemployment, aes(x=x, y=y)) + geom_line(color="red",size=0.7)+geom_hline(yintercept = 0, col = "black")+
  ylim(-0.15,0.1)+theme_light()+
  labs(title='Unemployment',x = "Quarters", y = " ")+theme(plot.title = element_text(color = 'black',size=12, hjust=0.5))+
  geom_ribbon(aes(ymin=ymin1, ymax=ymax1,x=x), linetype=2, alpha=0.3,fill="dodgerblue3")+
  geom_ribbon(aes(ymin=ymin2, ymax=ymax2,x=x), linetype=2, alpha=0.4,fill="dodgerblue3")

######8*8

(p1+p2)/(p4+p5)



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






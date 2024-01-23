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

##
p1/p2


# total incoming links of four groups
total.in.b   = as.vector(f[, 2])
total.in.ins = as.vector(f[, 3])
total.in.d   = as.vector(f[, 4])
total.in.o   = as.vector(f[, 5])

##ggplot2,incoming

p3<-ggplot(f, aes(x = dt, y =total.in.b)) +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Bank") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p4<-ggplot(f, aes(x = dt, y =total.in.ins)) +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Trust and others") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p5<-ggplot(f, aes(x = dt, y =total.in.d )) +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Broker") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p6<-ggplot(f, mapping = aes(x = dt, y =total.in.o)) +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Insurance") + theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

##
(p3+p4)/(p5+p6)


# total outgoing links of four groups
total.out.b   = as.vector(f[, 6])
total.out.ins = as.vector(f[, 7])
total.out.d   = as.vector(f[, 8])
total.out.o   = as.vector(f[, 9])

##ggplot2,outgoing
p7<-ggplot(f, aes(x = dt, y =total.out.b)) +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Bank") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p8<-ggplot(f, aes(x = dt, y =total.out.ins))  +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Trust and others") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p9<-ggplot(f, aes(x = dt, y =total.out.d ))  +
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Broker") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

p10<-ggplot(f, aes(x = dt, y =total.out.o ))+
  geom_smooth(method = "scam",size=0.5,formula = y ~ s(x, k = 400), se=T,color="red")+ 
  labs(y = "Index", x = "Insurance") +theme_light()+
  theme(axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 10))+
  annotate("rect",xmin = as.Date("2014-04-15", "%Y-%m-%d"),xmax = as.Date("2015-06-20","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2018-10-01", "%Y-%m-%d"),xmax = as.Date("2019-04-05","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")+
  annotate("rect",xmin = as.Date("2019-12-26", "%Y-%m-%d"),xmax = as.Date("2021-07-06","%Y-%m-%d"),ymin = -Inf,ymax = Inf,alpha=.2,fill="dodgerblue3")

###
(p7+p8)/(p9+p10)


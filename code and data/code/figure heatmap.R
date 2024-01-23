


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










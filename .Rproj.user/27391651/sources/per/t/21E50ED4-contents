# ------------------------------------------------------------------------
# R Code [S1E04] "Les prix baissent en Suisse - C'est top n'est-ce pas?"
# ------------------------------------------------------------------------
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
#
# Feel free to copy, adapt, and use this code for your own purposes at 
# your own risk.
#
# Daniel Kaufmann, 2020 (daniel.kaufmann@unine.ch)
# ------------------------------------------------------------------------
library(tsbox)
library(ggplot2)
library(forecast)
library(xts)
library(ggpubr)
library(lubridate)
library(xlsx)
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series
  
  series <- ts_index(series, baseY)*100
  
  Index <- sapply(seq_len(nrow(series)), function(i){
    weighted.mean(as.matrix(series[i,]), weights, na.rm = TRUE)
  })
  
  Index <- xts(Index, order.by = as.Date(index(series)))
  
  return(Index)
}

startDate <- "2000-01-01"
endDate   <- "2020-04-01"   # Has to be las observation of CPI

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
download.file(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12827309/master", destfile = "./S01E04_Prix/IPC.xlsx", mode="wb")

# Import the data
Date   <- seq(as.Date("1982-12-01"), as.Date(endDate), by = "month")
Prix  <- read.xlsx("./S01E04_Prix/IPC_Manuelx.xlsx", sheetName = "Main", as.data.frame = TRUE, startRow = 1)
Comp  <- read.xlsx("./S01E04_Prix/IPC_Manuelx.xlsx", sheetName = "Components", as.data.frame = TRUE, startRow = 1)
# Note that some adjustments have been made manually!

Prix <- xts(t(Prix[1:3,8:dim(Prix)[2]]), order.by = Date)

Type   <- Comp$PosType
Weight <- as.numeric(Comp$Weight)
Index  <- xts(t(Comp[, 8:dim(Comp)[2]]), order.by = Date)
Missing <- Comp$Missing

Weight <- Weight[Type == 4]
Index  <- Index[,Type == 4,]
Weight[is.na(Weight)] <- 0
Missing <- Missing[Type == 4]
Missing[is.na(Missing)] <- 0

Baseline <- ts_span(calcIndex(Index, Weight, "2015-12-01"), "2010-12-01")
Counterf <- ts_span(calcIndex(Index[,Missing == 0], Weight[Missing == 0], "2015-12-01"), "2010-12-01")
plot(ts_c(Baseline, Counterf, Prix[,1]))


#-------------------------------------------------------------------------------------
# Create charts
#-------------------------------------------------------------------------------------
p <- ts_ggplot(
  `IPC (officiel)`           = ts_pcy(ts_span(Prix[,1], "2010-12-01")),
  #`IPC (propre calcul)`     = ts_pcy(Baseline),
  `IPC (sans categories avec prix imputés en Avril 2020)` = ts_pcy(Counterf),
  
  title = "Taux d'inflation (par rapport à l'année précédente, en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E04_Prix/PrixImpute.png", width = 5, height = 4)


p <- ts_ggplot(
  `IPC`               = ts_pcy(ts_span(Prix[,1], "2015-12-01")),
  `Biens domestiques` = ts_pcy(ts_span(Prix[,2], "2015-12-01")),
  `Biens importés`    = ts_pcy(ts_span(Prix[,3], "2015-12-01")),
  
  title = "Taux d'inflation (par rapport à l'année précédente, en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=1,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E04_Prix/Inflation.png", width = 5, height = 4)


# ------------------------------------------------------------------------
# R Code [S1E03] "Les taux d'intérêts: Est-ce qu'ils suivent les 
#                 instructions du directeur?"
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

startDate <- "2000-01-01"
endDate   <- "2020-03-01"

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
# Source: https://data.snb.ch/fr/
#         Manual forecasts for march and April if not available (partly based on early
#         financial market information)

# Import the data
Monet  <- read.xlsx("./S01E03_TauxDirecteur/MarchéMonétaire.xlsx", sheetName = "Report", as.data.frame = TRUE, startRow = 17)
Oblig  <- read.xlsx("./S01E03_TauxDirecteur/Obligations.xlsx", sheetName = "Report", as.data.frame = TRUE, startRow = 17)
Direct <- read.xlsx("./S01E03_TauxDirecteur/TauxDirecteur.xlsx", sheetName = "Report", as.data.frame = TRUE, startRow = 17)
Credit <- read.xlsx("./S01E03_TauxDirecteur/TauxOperationsNouvelles.xlsx", sheetName = "Report", as.data.frame = TRUE, startRow = 24)
Credit2<- read.xlsx("./S01E03_TauxDirecteur/TauxCredits.xlsx", sheetName = "Report", as.data.frame = TRUE, startRow = 17)

Monet  <- xts(Monet[, 2:dim(Monet)[2]], order.by = as.Date(paste(Monet[, 1], "-01-01", sep ="")))
Oblig  <- ts_frequency(xts(Oblig[, 2:dim(Oblig)[2]], order.by =  as.Date(Oblig[, 1])), to = "month", aggregate = "mean", na.rm = TRUE)
Direct <- xts(Direct[, 2:dim(Direct)[2]], order.by =  as.Date(paste(Direct[, 1], "-01-01", sep ="")))
Credit <- xts(Credit[, 2:dim(Credit)[2]], order.by =  as.Date(paste(Credit[, 1], "-01-01", sep ="")))
Credit2 <- xts(Credit2[, 2:dim(Credit2)[2]], order.by =  as.Date(paste(Credit2[, 1], "-01-01", sep ="")))


# Put everything in one xts
Taux <- ts_c(Monet, Oblig, Direct, Credit, Credit2)
Taux <- ts_span(Taux, startDate, endDate)

# Before 2019, there was a target range instead of a policy rate. Take the middle of the
# target range
Taux$Direct[is.na(Taux$Direct)] <- ((Taux$Super+Taux$Infer)/2)[is.na(Taux$Direct)]

#-------------------------------------------------------------------------------------
# Create charts
#-------------------------------------------------------------------------------------
p <- ts_ggplot(
  `Taux directeur`                  = Taux$Direct,
  `Swiss Average Rate Overnight (SARON)`  = ts_span(Taux$SARON, "2019-06-01"),
  `CHF Libor à trois mois`                           = ts_span(Taux$LIB3M, startDate, "2019-05-01"),
  
  title = "Taux directeur et taux sur le marché monétaire (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E03_TauxDirecteur/TauxDirecteur.png", width = 5, height = 4)

# Recession 2002
p <- ts_ggplot(
  `Taux directeur`          = ts_span(Taux$Direct, startDate, "2004-01-01"),
  `Conféderation à 10 ans`  = ts_span(Taux$Conf10, startDate, "2004-01-01"),
  `Entreprises à 8 ans`     = ts_span(Taux$Manuf8, startDate, "2004-01-01"),
  `Prêts hypothécaires`     = ts_span(Taux$Hypo, startDate, "2004-01-01"),
  `Dépôts d'épargnes`       = ts_span(Taux$Epargne, startDate, "2004-01-01"),
  title = "Taux d'intérêts pendant la recession 2001/2002 (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "#E6AB02", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E03_TauxDirecteur/AutresTaux2001.png", width = 5, height = 4)

# Recession 2008
p <- ts_ggplot(
  `Taux directeur`          = ts_span(Taux$Direct, "2006-01-01", "2010-01-01"),
  `Conféderation à 10 ans`  = ts_span(Taux$Conf10, "2006-01-01", "2010-01-01"),
  `Entreprises à 8 ans`     = ts_span(Taux$Manuf8, "2006-01-01", "2010-01-01"),
  `Prêts hypothécaires`     = ts_span(Taux$Hypo, "2006-01-01", "2010-01-01"),
  `Dépôts d'épargnes`       = ts_span(Taux$Epargne, "2006-01-01", "2010-01-01"),
  title = "Taux d'intérêts pendant la recession 2008/2009 (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E03_TauxDirecteur/AutresTaux2008.png", width = 5, height = 4)

# Taux négatifs
p <- ts_ggplot(
  `Taux directeur`          = ts_span(Taux$Direct, "2013-01-01", "2017-01-01"),
  `Conféderation à 10 ans`  = ts_span(Taux$Conf10, "2013-01-01", "2017-01-01"),
  `Entreprises à 8 ans`     = ts_span(Taux$Manuf8, "2013-01-01", "2017-01-01"),
  `Prêts hypothécaires`     = ts_span(Taux$Hypo, "2013-01-01", "2017-01-01"),
  `Dépôts d'épargnes`       = ts_span(Taux$Epargne, "2013-01-01", "2017-01-01"),
  title = "Taux d'intérêts en territoire négatif (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E03_TauxDirecteur/AutresTaux2015.png", width = 5, height = 4)


# Crise Corona
p <- ts_ggplot(
  `Taux directeur`          = ts_span(Taux$Direct, "2016-01-01", endDate),
  `Conféderation à 10 ans`  = ts_span(Taux$Conf10, "2016-01-01", endDate),
  `Entreprises à 8 ans`     = ts_span(Taux$Manuf8, "2016-01-01", endDate),
  `Prêts hypothécaires`     = ts_span(Taux$Hypo, "2016-01-01", endDate),
  `Dépôts d'épargnes`       = ts_span(Taux$Epargne, "2016-01-01", endDate),
  title = "Taux d'intérêts pendant la crise Corona (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E03_TauxDirecteur/AutresTauxCorona.png", width = 5, height = 4)


# ------------------------------------------------------------------------
# R Code "La courbe de fièvre de l'économie Suisse"
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

startDate <- "2007-01-01"

#-------------------------------------------------------------------------------------
# Compute risk premia
#-------------------------------------------------------------------------------------
download.file(url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/7966853/master", destfile = "./PrimeDeRisque/Defaults_OFS.xlsx", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_gov_y.csv", destfile = "./PrimeDeRisque/ObligationsConf.csv", mode="wb")
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_dom_non_gov_rating_sbi_y.csv", destfile = "./PrimeDeRisque/ObligationsEnt.csv", mode="wb")

Gov      <- read.csv("./PrimeDeRisque/ObligationsConf.csv", sep = ";", skip = 4)
NonGov   <- read.csv("./PrimeDeRisque/ObligationsEnt.csv", sep = ";", skip = 4)
Defaults <- read.xlsx("./PrimeDeRisque/Defaults_OFS.xlsx", sheetName = "T6.2.3.1", as.data.frame = TRUE)

# Beginning of default procedures (Ouverture des procédures de faillite)
# I calculate the number of default procedures as a share of active firms in 2017
# https://www.bfs.admin.ch/bfs/de/home/statistiken/industrie-dienstleistungen/unternehmen-beschaeftigte/unternehmensdemografie/bestand-aktiver.html
numberFirms2017 <- 555626
myDate      <- as.Date(paste(as.numeric(as.matrix((Defaults[1,2:length(Defaults[1,])]))), "-01-01", sep = ""))
myDefaults  <- as.numeric(as.matrix((Defaults[2,2:length(Defaults[2,])])))
Faillite <- xts(myDefaults, order.by = myDate)/numberFirms2017*100

# Create date series
nobs    <- dim(Gov)[1]
myDate1 <- dmy(Gov[, 1])
myDate2 <- dmy(NonGov[, 1])

# Create gov. bond yields
Gov1  <- xts(as.numeric(Gov[, 2]), order.by = myDate1)
Gov3  <- xts(as.numeric(Gov[, 3]), order.by = myDate1)
Gov7  <- xts(as.numeric(Gov[, 7]), order.by = myDate1)
Gov10 <- xts(as.numeric(Gov[, 8]), order.by = myDate1)

# Create corporate bond yields AAA - 
BBB1  <- xts(as.numeric(NonGov[, 3]), order.by = myDate2)
BBB3  <- xts(as.numeric(NonGov[, 4]), order.by = myDate2)
BBB7  <- xts(as.numeric(NonGov[, 6]), order.by = myDate2)
BBB10 <- xts(as.numeric(NonGov[, 7]), order.by = myDate2)

AA1  <- xts(as.numeric(NonGov[, 15]), order.by = myDate2)
AA3  <- xts(as.numeric(NonGov[, 16]), order.by = myDate2)
AA7  <- xts(as.numeric(NonGov[, 18]), order.by = myDate2)
AA10 <- xts(as.numeric(NonGov[, 19]), order.by = myDate2)


# Compute implies probability of default
# ir: expected return corporate bond, d: probability of default, i: riskless interest rate
# x:  risk premium
# x = (1+i+x) - (1+i) -> We can calculate the risk premium as the rate of return between
#                        a riskless investment and the corporate bond yield
#
# (1+i) = (1-d)*(1+i+x) + d*0
# d = ((1+i)-(1+i+x))/(1+i+x)
# d = -x/(1+i+x)      -> We can calculat the probability of default as the - the risk premium
#                        divided by the risky rate of return of a corporate bond yield
RPBBB1  <- BBB1  - Gov1
RPBBB3  <- BBB3  - Gov3
RPBBB7  <- BBB7  - Gov7
RPBBB10 <- BBB10 - Gov10

RPAA1   <- AA1  - Gov1
RPAA3   <- AA3  - Gov3
RPAA7   <- AA7  - Gov7
RPAA10  <- AA10 - Gov10

dBBB1   <- 100*RPBBB1/(1+BBB1)
dBBB3   <- 100*RPBBB3/(1+BBB3)
dBBB7   <- 100*RPBBB7/(1+BBB7)
dBBB10  <- 100*RPBBB10/(1+BBB10)

dAA1   <- 100*RPAA1/(1+AA1)
dAA3   <- 100*RPAA3/(1+AA3)
dAA7   <- 100*RPAA7/(1+AA7)
dAA10  <- 100*RPAA10/(1+AA10)

#-------------------------------------------------------------------------------------
# Create charts
#-------------------------------------------------------------------------------------
myLines <- c(as.numeric(as.Date("2008-09-15")), as.numeric(as.Date("2015-01-15")), as.numeric(as.Date("2020-02-01")))

p <- ts_ggplot(
  `Confédération`                     = ts_span(Gov1, startDate),
  `Entrerpises (notation AA-AAA)`     = ts_span(AA1, startDate),
  `Entrerpises (notation BBB-AAA)`    = ts_span(BBB1, startDate),
  
  title = "Rendements des obligations à 1 ans (en %)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
   scale_color_manual(values = c("black",  "#1B9E77", "#D95F02"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=myLines , colour="blue", size = 1, alpha = 0.5) 
  p <- p+  geom_text(x =myLines[1], y = 0, label="Faillite Lehman", colour="blue", angle=90, vjust = -1)
  p <- p+  geom_text(x =myLines[2], y = 2, label="Fin du taux plancher", colour="blue", angle=90, vjust = -1)
  p <- p+  geom_text(x =myLines[3], y = 2, label="Crise Corona", colour="blue", angle=90, vjust = -1)
  p
ggsave(filename = "./PrimeDeRisque/Obligations.png", width = 5, height = 4)

p <- ts_ggplot(
  `Entrerpises (notation AA-AAA)`  = ts_span(RPAA1, startDate),
  `Entrerpises (notation BBB-AAA)` = ts_span(RPBBB1, startDate),
  
  title = "Primes de risque à 1 ans (en pp)"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=myLines , colour="blue", size = 1, alpha = 0.5) 
p <- p+  geom_text(x =myLines[1], y = 1, label="Faillite Lehman", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[2], y = 1, label="Fin du taux plancher", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[3], y = 1, label="Crise Corona", colour="blue", angle=90, vjust = -1)
p
ggsave(filename = "./PrimeDeRisque/PrimesDeRisque.png", width = 5, height = 4)

p <- ts_ggplot(
  `Primes de risques (notation BBB-AAA, en pp)`           = ts_span(RPBBB1, startDate),
  `Ouverture proc. de faillite (en % des ent. en 2017)`  = ts_span(Faillite, startDate),
  title = "Relation avec les procédures de faillite"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  scale_color_manual(values = c("#1B9E77", "#D95F02"))+ 
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) +
  geom_vline(xintercept=myLines , colour="blue", size = 1, alpha = 0.5) 
p <- p+  geom_text(x =myLines[1], y = 1.5, label="Faillite Lehman", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[2], y = 1.5, label="Fin du taux plancher", colour="blue", angle=90, vjust = -1)
p <- p+  geom_text(x =myLines[3], y = 1.5, label="Crise Corona", colour="blue", angle=90, vjust = -1)
p
ggsave(filename = "./PrimeDeRisque/ProcFaillitePrime.png", width = 5, height = 4)



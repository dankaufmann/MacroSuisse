# ------------------------------------------------------------------------
# R Code [S1E02] "Les saints de glace jettent un froid sur la Suisse"
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
getForecastVariance <- function(fcst){
  # Function to extract forecast error variance from a forecast object
  # CI lower = y(t+h|t)-1.96*sig(h)
  # Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
  # Get exact percentile (1.96 yield basically the same)
  
  z957 = qnorm(0.975, 0, 1)
  sigh2 = ((fcst$lower[,"95%"]-fcst$mean)/(-z957))^2
  return(sigh2)
}

startDate <- "1999-01-01"
endDate   <- "2020-04-01"

#-------------------------------------------------------------------------------------
# Download the data
#-------------------------------------------------------------------------------------
# Source: https://kof.ethz.ch/fr/previsions-indicateurs/indicateurs/kof-globalbaro.html
# Baromètre mondial: Les deux indicateurs sont composés des résultats d'enquêtes 
# conjoncturelles men?es dans plus de 50 pays. 
download.file(url = "https://datenservice.kof.ethz.ch/api/v1/public/ts?keys=ch.kof.globalbaro_coincident,ch.kof.globalbaro_leading&mime=xlsx&name=date,globalbaro_coincident,globalbaro_leading", destfile = "./S01E02_BarometreMondial//Barometre.xlsx", mode="wb")

# PIB Suisse du SECO
download.file(url = "https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/VIP%20Quartalssch%C3%A4tzungen/qna_p_csa.xls.download.xls/qna_p_csa.xls", destfile = "./S01E02_BarometreMondial//PIBSuisse.xlsx", mode="wb")

# Télécharger les données pour l'incertitude sur les marché financier
download.file(url = "https://www.six-group.com/exchanges/downloads/indexdata/h_vsmi_30.csv", destfile = "./S01E02_BarometreMondial//VSMI.csv", mode="wb")

# Import the data
Baro <- read.xlsx("./S01E02_BarometreMondial//Barometre.xlsx", sheetName = "Sheet1", as.data.frame = TRUE)
PIB  <- read.xlsx("./S01E02_BarometreMondial//PIBSuisse.xlsx", sheetName = "real_q", as.data.frame = TRUE, startRow = 11)
Vol  <- read.csv("./S01E02_BarometreMondial//VSMI.csv", sep = ";")

# Create time series (use aggregate GDP and the Barom leading indicator)
Baro <- xts(Baro[,3], order.by = as.Date(paste(Baro[,1], "01", "01", sep = "-")))
PIB  <- xts(PIB[,3], order.by = as.Date(paste(PIB[,1], PIB[,2]*3-2, "01", sep = "-")))

nobs   <- dim(Vol)[1]
myDate <- dmy(Vol[, 1])
Vol    <- xts(as.numeric(Vol[, 3]), order.by = myDate)
Vol    <- ts_frequency(Vol, to = "quarter", aggregate = "mean", na.rm = TRUE)
Vol    <- ts_span(Vol, startDate)

# Normalize the Baro so that it has the same mean and volatility as GDP growth
mPIB   <- as.numeric(mean(ts_pc(PIB), na.rm = TRUE))
sdPIB  <- as.numeric(sqrt(var(ts_pc(PIB), na.rm = TRUE)))
mBaro  <- as.numeric(mean(Baro, na.rm = TRUE))
sdBaro <- as.numeric(sqrt(var(Baro, na.rm = TRUE)))
Baro   <- (Baro - mBaro)/sdBaro*sdPIB+mPIB

mVol     <- as.numeric(mean(Vol, na.rm = TRUE))
sdVol    <- as.numeric(sqrt(var(Vol, na.rm = TRUE)))
VolNorm  <- (Vol - mVol)/sdVol*sdPIB+mPIB

# Compute correlation at quarterly frequency (also data set used in forecasting
# model)
BaroQ   <- ts_frequency(Baro, to = "quarter", aggregate = "mean", na.rm = TRUE)
myDataQ <- data.frame(ts_span(ts_c(ts_pc(PIB), BaroQ, lag(BaroQ, 1)), startDate))
colnames(myDataQ) <- c("PIB", "Baro", "Baro.l")
print(cor(myDataQ[1:dim(myDataQ)[1]-1,]))

#-------------------------------------------------------------------------------------
# Create charts
#-------------------------------------------------------------------------------------
p <- ts_ggplot(
  `Croissance du PIB Suisse (en %)`       = ts_span(ts_pc(PIB), startDate),
  `Baromètre mondial (index avancé normalisé)`     = ts_span(Baro, startDate),
  title = "Activité économique Suisse et barométre mondial"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E02_BarometreMondial//PIBetBaro.png", width = 5, height = 4)

p <- ts_ggplot(
  `Croissance du PIB (en %)`       = ts_span(ts_pc(PIB), startDate),
  `Baromètre mondial (index av. norm.)`     = ts_span(Baro, startDate),
  `Incertitude marchés financiers (norm.)`     = ts_span(VolNorm, startDate),
  title = "Données modèle de prévision"
)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
p
ggsave(filename = "./S01E02_BarometreMondial//PIBetBaroVol.png", width = 5, height = 4)



#-------------------------------------------------------------------------------------
# Estimate simple forecasting model for computing the probability of a negative growth rate
#-------------------------------------------------------------------------------------
XVars    <- ts_span(ts_ts(ts_c(BaroQ, Vol)), startDate, endDate)
YVars    <- ts_span(ts_ts(ts_pc(PIB)), startDate, endDate)
Model    <- Arima(YVars, order = c(0, 0, 0), include.constant= TRUE, xreg = XVars[1:(dim(XVars)[1]-2),])
summary(Model)

Forecast <- forecast(Model, xreg = (XVars[(dim(XVars)[1]-1):dim(XVars)[1],]), h = 2, level = c(50, 80, 90, 95))
p <- autoplot(Forecast,fan=TRUE)
p <- p + theme_minimal() + ylab("")+xlab("")+
  ggtitle("Prévision croissance PIB Suisse (en %)")+
  theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(nrow=2,byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
  theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
  theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank())
p <- p + scale_x_continuous(limits=c(2018, 2021)) 
p <- p + scale_y_continuous(limits=c(-2, 1.5)) 
p
ggsave(filename = "./S01E02_BarometreMondial//PIBPrevision.png", width = 5, height = 4)

# Simulate the forecast density assuming that the forecast error is normally distributed
# This implies that y(t+h) ~ N(y(t+h|t), sigh^2), that is, the future value of GDP growth
# is normally distributed with a mean equal to the point forecast and variance equal to the
# forecast error variance.
# Compute forecast error Variance
sigh2 <- getForecastVariance(Forecast)

NSim    <- 5000    # Note that you can test your codes with a small number of simulations to increase speed and calculate the final results with a higher number
fcsth   <- Forecast$mean
SimFcst <- matrix(NA, nrow = 2, ncol = NSim)
H       <- length(Forecast$mean)

SimFcst[1, ] = rnorm(NSim, fcsth[H-1], sqrt(sigh2[H-1]))
SimFcst[2, ] = rnorm(NSim, fcsth[H], sqrt(sigh2[H]))
SimFcst = xts((SimFcst), order.by = as.Date(c("2020-01-01", "2020-04-01")))

# Compute the probability of a negative growth rate
PNeg2020Q1 = mean(SimFcst[1,]<0)
PNeg2020Q2 = mean(SimFcst[2,]<0)
print(c(PNeg2020Q1, PNeg2020Q2))



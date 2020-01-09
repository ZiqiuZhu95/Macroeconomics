library(dplyr)
library(ggplot2)
library(latex2exp)

setwd('D:\\Users\\Ziqiu\\OneDrive\\Documents\\Masters Courses\\EC640 Macroeconomics\\A3')
df <- read.csv('D:\\Users\\Ziqiu\\OneDrive\\Documents\\Masters Courses\\EC640 Macroeconomics\\A3\\A3.csv', header = TRUE)
df[,-1] <- as.data.frame(sapply(df[-1], as.numeric))

df[,1] <- as.Date(as.character(df[,1]), format = "%m/%d/%Y")

png("1A.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = df, aes(x = Date)) + 
    geom_line(aes(y = K1/Y, color = 'dodgerblue1'), size = 1.1)+
    geom_line(aes(y = K2/Y, color = 'grey2'), size = 1.1)+
    geom_line(aes(y = K3/Y, color = 'darkorange1'), size = 1.1)+
    theme_bw() +
    scale_color_identity(guide = 'legend',
                         breaks = c('dodgerblue1','grey2','darkorange1'),
                         labels = c('0.015', '0.025', '0.05'))  +
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Ratio to GDP', color = "Depreciation Rate")
dev.off()
    
png("1B.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = df, aes(x = Date)) + 
    geom_line(aes(y = log(Y), color = 'dodgerblue1'), size = 1.1)+
    geom_line(aes(y = log(K2), color = I('grey2')), size = 1.1)+
    geom_line(aes(y = log(H), color = 'darkorange1'), size = 1.1)+
    theme_bw() +
    scale_color_identity(guide = 'legend',
                         breaks = c('dodgerblue1','grey2','darkorange1'),
                         labels = c('Real GDP', 'Capital Stock', 
                                    'Total Hours Worked')) +
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Logged Values', color = "Series")
dev.off()

###1c

df <- mutate(df, SolowResiduals = log(Y) - 0.34*log(K1) - (1-0.34)*log(H))

q1c <- lm(SolowResiduals ~ Date, data = df)

summary(q1c)
plot(df[['Date']], df[['SolowResiduals']])
abline(lm(SolowResiduals ~ Date, data = df))

# Graph of the solowresiuals
png("1D.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = df, aes(x=Date)) + 
    geom_smooth(aes(y = SolowResiduals), method = 'lm', formula = y ~ x) + 
    geom_line(aes(y = SolowResiduals)) +
    theme_bw() +
    theme(axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26))
dev.off()

# graphing the residuals of the linear trendd in solow residuals
q1e <- data.frame("Residuals" = q1c$residuals, "Date" = df[['Date']])

png("1E.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = q1e, aes(x = Date)) + 
    geom_line(aes(y = Residuals)) +
    theme_bw() +
    theme(axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26))
dev.off()

# Simple AR(1) no constant
q1f <- lm(q1e[['Residuals']][-1]~q1e[['Residuals']][-length(q1e[['Residuals']])]-1)
summary(q1f)
var(q1f$residuals)

q1g <- lm(log(Labor_prod) ~ Date, data = df)


AR1Res <- lm(q1g$residuals[-1]~q1g$residuals[-length(q1g$residuals)] - 1)
summary(AR1Res)

var(AR1Res$residuals)
                  
### Q5K
                           
df2 <- read.csv('D:\\Users\\Ziqiu\\OneDrive\\Documents\\Masters Courses\\EC640 Macroeconomics\\A3\\A35K.csv', 
                header = TRUE)
df2[,-1] <- as.data.frame(sapply(df2[-1], as.numeric))
df2[,1] <- as.Date(as.character(df2[,1]), format = "%m/%d/%Y")

png("Q5K.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = df2, aes(x=Date)) + 
    geom_line(aes(y = y, color = 'grey2'), size = 1.1)+
    geom_line(aes(y = y_fy, color = 'red'), size = 1.1) +
    theme_bw() +
    scale_color_identity(guide = 'legend',
                         breaks = c('grey2','red'),
                         labels = c('Real GDP', 'Real GDP Forecast'))  +
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Logged Cyclical Component', color = "Series")
dev.off()

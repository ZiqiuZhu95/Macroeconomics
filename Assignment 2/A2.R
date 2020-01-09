library(dplyr)
library(ggplot2)

# Question 1----

# Loading Data Frame 
setwd('EC640 macroeconomics/A2')
df <- read.csv("Q1Data.csv", header = TRUE)
df[,-1] <- as.data.frame(sapply(df[-1], as.numeric))
df[,1] <- as.Date(as.character(df[,1]), format = "%m/%d/%Y")
df <- df[-c(1:4),]

# png("Q1A.png", width = 465, height = 225, units='mm', res = 300)
# ggplot(data = df, aes(x = Date)) + 
#    # geom_line(aes(y = Durables_Y2Y, color = 'dodgerblue1'), size = 1.1) +
#     geom_line(aes(y = Nondurables_Y2Y, color = I('grey2')), size = 1.1) +
#     geom_line(aes(y = GDP_Y2Y, color = 'red'), size = 1.1) +
#     theme_bw() +
#     scale_color_manual(labels = c('Durables', 'Nondurables', 'GDP'), 
#                        values = c("dodgerblue1", 'grey2', 'red')) +
#     labs(color = 'Series') + 
#     theme(legend.position="bottom",
#           legend.text = element_text(size=22),
#           axis.text = element_text(size=22),
#           axis.title = element_text(size=26),
#           plot.title=element_text(size = 26),
#           legend.title=element_text(size= 26),
#           # Change legend key size and key width
#           legend.key.size = unit(1.5, "cm"),
#           legend.key.width = unit(2.0,"cm")) +
#     labs(x = 'Year', y = 'Year-to-year Growth Rate')
# dev.off()

### Part B

q<-acf(df['GDP_Y2Y'])
abline(h = 0.5, col = 'red')
q

q <- acf(df['Nondurables_Y2Y'])
abline(h = 0.5, col = 'red')
q

q <- acf(df['Durables_Y2Y'])
abline(h = 0.5, col = 'red')
q
# Question 2 

### Version 2----
df2 <- read.csv('Q2Data2.csv', header = TRUE)
df2 <- df2[complete.cases(df2),]
df2[,5:7] <- as.data.frame(sapply(df2[,5:7],as.numeric))


growth <- function(x)x/lag(x)-1

Q2 <- df2 %>%
    group_by(country) %>%
    filter(1960 %in% year) %>%
    filter(year >= 1960) %>%
    mutate(rgdpcap_Y2Y = growth(rgdpcap) * 100) 

    
AvgGrowth <- filter(Q2, year <= 2000) %>%
    summarise(mean = mean(rgdpcap_Y2Y, na.rm = TRUE))

USA1960 <- filter(Q2, year == 1960 & countrycode == 'USA') %>%
    select(country, rgdpcap)
USA1960 <- as.numeric(USA1960[[2]])

GDPRatio <- mutate(Q2, xaxis = rgdpcap / USA1960) %>%
    filter(year == 1960) %>%
    select(country,xaxis) %>%
    arrange(country)

Q2A <- cbind(as.data.frame(AvgGrowth), as.data.frame(GDPRatio[,2]))

png("Q2A.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = Q2A, aes(x = xaxis)) +
    geom_point(aes(y = mean, size = 1.2)) + 
    geom_smooth(aes(y = mean), method = 'lm', formula = y~x, size = 1.2) +
    theme_bw() +
    labs(x = 'Relative GDP per capita to USA in 1960', y = 'Annual Growth Rate (%)') +
    theme(legend.position = "none",
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26))
dev.off()
cor.test(Q2A[['mean']], Q2A[['xaxis']])
#2b ----

year1960 <-  df2 %>% 
    group_by(country) %>%
    filter(1960 %in% year) %>%
    filter(year == 1960 & countrycode != 'USA') %>%
    select(country,rgdpcap) 
Hpercentile <- filter(year1960, rgdpcap > quantile(year1960[[2]], .50))
Lpercentile <- filter(year1960, rgdpcap <= quantile(year1960[[2]], .5))

Q2B_Highgrowth <- Q2 %>%
    filter(country %in% Hpercentile[[1]]) %>%
    group_by(country) %>%
    filter(year <= 2000) %>%
    summarise(mean = mean(rgdpcap_Y2Y, na.rm = TRUE)) %>%
    mutate(percentile = 'Top 50 Percentile for Output Level in 1960')

Q2B_Highratio <- Q2%>%
    filter(country %in% Hpercentile[[1]]) %>%
    mutate(xaxis = rgdpcap/USA1960) %>%
    filter(year == 1960) %>%
    select(country, xaxis) %>%
    arrange(country)

Q2B_Lowgrowth <- Q2 %>%
    filter(country %in% Lpercentile[[1]]) %>%
    filter(year <= 2000) %>%
    group_by(country) %>%
    summarise(mean = mean(rgdpcap_Y2Y, na.rm = TRUE)) %>%
    mutate(percentile = 'Bottom 50 Percentile for Output Level in 1960')

Q2B_Lowratio <- Q2%>%
    filter(country %in% Lpercentile[[1]]) %>%
    mutate(xaxis = rgdpcap/USA1960) %>%
    filter(year == 1960) %>%
    select(country, xaxis) %>%
    arrange(country)

Q2B <- cbind(as.data.frame(rbind(Q2B_Highgrowth, Q2B_Lowgrowth)),
            as.data.frame(rbind(Q2B_Highratio[,2], Q2B_Lowratio[,2])))

png("Q2B.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = Q2B, aes(x = xaxis)) +
    geom_point(aes(y = mean, size = 1.2)) + 
    geom_smooth(aes(y = mean), method = 'lm', formula = y~x) +
    facet_grid(col = vars(percentile)) + 
    theme_bw() +
    labs(x = 'Relative GDP per capita to USA in 1960', y = 'Annual Growth Rate (%)') +
    theme(legend.position="none",
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          strip.text.x = element_text(size = 18))
dev.off()

test <- filter(Q2B, percentile == 'Top 50 Percentile for Output Level in 1960') 

cor.test(test[['mean']], test[['xaxis']])

###Q2C ---- 

Q2C <- df2 %>%
    group_by(country) %>%
    filter(1975 %in% year) %>%
    filter(year >= 1975) %>%
    mutate(rgdpcap_Y2Y = growth(rgdpcap) * 100)

AvgGrowthC <- filter(Q2C, year <= 2015) %>%
    summarise(bleh= mean(rgdpcap_Y2Y, na.rm = TRUE))

USA1975 <- filter(Q2C, year == 1975 & countrycode == 'USA') %>%
    select(country, rgdpcap)

GDPRatioC <- mutate(Q2C, xaxis = rgdpcap / USA1975[[2]]) %>%
    filter(year == 1975) %>%
    select(country, xaxis) %>%
    arrange(country)

Q2CDF <- cbind(as.data.frame(AvgGrowthC), xaxis = as.data.frame(GDPRatioC)[,2])

png("Q2C.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = Q2CDF, aes(x = xaxis)) +
    geom_point(aes(y = bleh, size = 1.2)) + 
    geom_smooth(aes(y = bleh), method = 'lm', formula = y~x) + 
    theme_bw() +
    labs(x = 'Relative GDP per capita to USA in 1975', y = 'Annual Growth Rate (%)') +
    theme(legend.position = "none",
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26))
dev.off()
cor.test(Q2CDF[['bleh']], Q2CDF[['xaxis']])

### Q2D OPEC countries
OPEC <- c('DZA', 'AGO', 'COD', 'ECU', 'GIN', 'GAB', 'IRN', 'IRQ', 'KWT', 'NGA',
          'QAT', 'SAU', 'ARE', 'VEN')


Q2D <-  df2 %>% 
    group_by(countrycode) %>%
    filter(1970 %in% year) %>%
    filter(year >= 1970 & year <= 2010) %>%
    mutate(rgdpcap_Y2Y = growth(rgdpcap) * 100) 

AvgGrowthD <- summarise(Q2D, bleh= mean(rgdpcap_Y2Y, na.rm = TRUE))

USA1970 <- filter(Q2D, year == 1970 & countrycode == 'USA') %>%
    select(countrycode, rgdpcap)

GDPRatioD <- mutate(Q2D, xaxis = rgdpcap / USA1970[[2]]) %>%
    filter(year == 1970) %>%
    select(countrycode, xaxis) %>%
    arrange(countrycode)

Q2DDF <- cbind(as.data.frame(AvgGrowthD), xaxis = as.data.frame(GDPRatioD)[,2])

OPEC_2D <- filter(Q2DDF, countrycode %in% OPEC) %>%
    mutate(type = 'OPEC')

Others <- filter(Q2DDF, !(countrycode %in% OPEC)) %>%
    mutate(type = 'Not OPEC')

Final_DF <- rbind(as.data.frame(OPEC_2D), as.data.frame(Others))
#Final_DF['type'] <- as.factor(Final_DF['type'])

png("Q2D.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = Final_DF, aes(x = xaxis)) +
    geom_point(aes(y = bleh, size = 1.2)) + 
    geom_smooth(aes(y = bleh), method = 'lm', formula = y~x)+
    facet_grid(col = vars(type)) +
    theme_bw() +
    labs(x = 'Relative GDP per capita to USA in 1970', y = 'Annual Growth Rate (%)') +
    theme(legend.position = "none",
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          strip.text.x = element_text(size = 18))
dev.off()
cor.test(OPEC_2D[['bleh']], OPEC_2D[['xaxis']])
cor.test(Others[['bleh']], Others[['xaxis']])


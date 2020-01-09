### Call Libraries

library(seasonal)
library(dplyr)
library(ggplot2)
library(zoo)
library(mFilter)

# Loading Data Frame ----

df <- read.csv("D:/Users/Ziqiu/OneDrive/Documents/Masters Courses/EC640 Macroeconomics/A1.csv", 
               header = TRUE)
df[,-1] <- as.data.frame(sapply(df[-1], as.numeric))
df[,1] <- as.Date(as.character(df[,1]), format = "%m/%d/%Y")

GDP_y2y <- select(df, Date,C_Y2Y:Y_Y2Y)[-c(1:4),] #remove first 4 rowws

GDP_comp <- select(df, Date:Y)
GDP_comp <- GDP_comp[complete.cases(GDP_comp),]

consumer_price <- select(df, Date, cpi)
consumer_price[,2] <- ts(consumer_price[,2], start = c(1961, 1), end = c(2019, 2),
                         frequency = 4)



plot(consumer_price[,2])
sa_series <- seas(consumer_price[,2], x11 = "")
lines(final(sa_series), col=2)

c <- as.numeric(final(sa_series))
write.csv(matrix(c, nrow=1), file ="myfile.csv", row.names=FALSE)
### added this as a column in my excel file

#  zoom in

# zoom_cp <- ts(consumer_price[,2][1:25], start = c(1961, 1), end = c(1967, 1),
#               frequency = 4)
# zoom_sas <- ts(final(sa_series)[1:25], start = c(1961,1), end = c(1967,1), 
#                frequency = 4)
# plot(zoom_cp)
# lines(zoom_sas, col = 2)

### with GGPLOT instead ### Need to label


# consumer_price <- consumer_price %>% 
#         mutate(s_adjusted = as.numeric(final(sa_series)))
# 
# ggplot(data = consumer_price, aes(x = Date)) +
#     geom_line(aes(y = as.numeric(cpi))) + 
#     geom_line(aes(y = s_adjusted, color = 'red')) #+
#     #coord_cartesian(ylim=c(15,30) , xlim = c(as.Date("1961-01-01"), as.Date("1975-01-01")))

### Question 1.a) ----

### mean and variance

### To compare with sample table
GDP_y2y %>%
    select(C_Y2Y:Y_Y2Y) %>%
    summarise_all(funs(mean))
GDP_y2y %>%
    select(C_Y2Y:Y_Y2Y) %>%
    summarise_all(funs(sd))

### mean and SD

summarise_all(GDP_y2y, funs(mean))
summarise_all(GDP_y2y, funs(sd))
summarise_all(select(GDP_y2y, C_Y2Y:M_Y2Y), funs(sd))/sapply(GDP_y2y['Y_Y2Y'], sd)



#This is a function that creates lags from -4 to 4 of a vector.
create_lags <- function(vctr) {
    zoo_vector <- zoo(vctr)
    lagged_df <- vctr
    #if (GDP_y2y['Y_Y2Y'] == vctr) {
     #   lagged_df <- GDP_y2y['Y_Y2Y']
      #  } else {
       #     lagged_df <- cbind(GDP_y2y['Y_Y2Y'],vctr)
        #}
    for (i in c(-4:-1, 1:4)){
        lags <- stats::lag(zoo_vector, i, na.pad = TRUE)
        lagged_df <- cbind(lagged_df, as.data.frame(lags))
    }
    return(lagged_df[,c(2:5,1,6:9)])
}

### Autocorrelation table

# get the same results as his chart for 1961-2019

#correlates the lags to the vector of GDP to get the first row
#of the correlation table
test_table <- sapply(create_lags(GDP_y2y['Y_Y2Y']), cor, GDP_y2y['Y_Y2Y'], 
                     use = 'pairwise.complete.obs')

#loop on the other variables to complete the rest of the correlation table.

for (i in names(GDP_y2y[,c(-1,-7)])) {
    test_table <- rbind(test_table, 
                        sapply(create_lags(GDP_y2y[i]), cor, GDP_y2y['Y_Y2Y'], 
                               use = 'pairwise.complete.obs'))
}
colnames(test_table) <- c(-4:4)
test_table
# my numbers are off by 0.04 from the numbers posted on the table at consumption

# 1981-2019

Q1A <- df %>% 
    select(Date, C_Y2Y:Tcomp_hour_Y2Y) %>%
    filter(Date >= as.Date("1981-01-01"))# %>%
# select(C_Y2Y:Y_Y2Y)

summarise_all(Q1A, funs(mean), na.rm = TRUE)
summarise_all(Q1A, funs(sd), na.rm = TRUE)
summarise_all(Q1A[,-c(9:12)], funs(sd), na.rm = TRUE)/sapply(Q1A['Y_Y2Y'], sd)
sd(Q1A[,9], na.rm = TRUE)/sapply(Q1A %>%
                                     filter(Date >= as.Date('1985-01-01')) %>%
                                     select(Y_Y2Y), sd)
summarise_all(Q1A[10:12], funs(sd), na.rm = TRUE)/sapply(Q1A %>% 
                                                             filter(Date >= as.Date('1982-01-01')) %>%
                                                             select(Y_Y2Y), sd)


cor_table_1a <- sapply(create_lags(Q1A['Y_Y2Y']), cor, Q1A['Y_Y2Y'],
                       use = 'pairwise.complete.obs')
for (i in names(Q1A[,c(2:6,8:12)])) {
    cor_table_1a <- rbind(cor_table_1a, 
                          sapply(create_lags(Q1A[i]), cor, Q1A['Y_Y2Y'], 
                                 use = 'pairwise.complete.obs'))
}

colnames(cor_table_1a) <- c(-4:4)
cor_table_1a

### Q1b ----


Q1B <- df %>% 
    select(Date, C_Y2Y:Tcomp_hour_Y2Y) %>%
    filter(Date >= as.Date("1996-01-01"))

summarise_all(Q1B, funs(mean), na.rm = TRUE)
summarise_all(Q1B, funs(sd), na.rm = TRUE)
summarise_all(Q1B[,-c(9:12)], funs(sd), na.rm = TRUE)/sapply(Q1B['Y_Y2Y'], sd)
sd(Q1B[,9], na.rm = TRUE)/sapply(Q1B %>%
                                     filter(Date >= as.Date('1996-01-01')) %>%
                                     select(Y_Y2Y), sd)
summarise_all(Q1B[10:12], funs(sd), na.rm = TRUE)/sapply(Q1B %>% 
                                                             filter(Date >= as.Date('1996-01-01')) %>%
                                                             select(Y_Y2Y), sd)

cor_table_1b <- sapply(create_lags(Q1B['Y_Y2Y']), cor, Q1B['Y_Y2Y'],
                       use = 'pairwise.complete.obs')
for (i in names(Q1B[,c(2:6,8:12)])) {
    cor_table_1b <- rbind(cor_table_1b, 
               sapply(create_lags(Q1B[i]), cor, Q1B['Y_Y2Y'],
                      use = 'pairwise.complete.obs'))
}

colnames(cor_table_1b) <- c(-4:4)
cor_table_1b


### Q1c ----

GDP_q2q <-  select(df, Date, C_Q2Q:Tcomp_hour_Q2Q)
Q1C <- GDP_q2q %>% 
    filter(Date >= as.Date("1981-01-01"))

summarise_all(Q1C, funs(mean), na.rm = TRUE)
summarise_all(Q1C, funs(sd), na.rm = TRUE)
summarise_all(Q1C[,-c(9:12)], funs(sd), na.rm = TRUE)/sapply(Q1C['Y_Q2Q'], sd)
sd(Q1C[,9], na.rm = TRUE)/sapply(Q1C %>%
                                     filter(Date >= as.Date('1984-04-01')) %>%
                                     select(Y_Q2Q), sd)
summarise_all(Q1C[10:12], funs(sd), na.rm = TRUE)/sapply(Q1C %>% 
                                                             filter(Date >= as.Date('1981-04-01')) %>%
                                                             select(Y_Q2Q), sd)

cor_table_1b <- sapply(create_lags(Q1B['Y_Y2Y']), cor, Q1B['Y_Y2Y'],
                       use = 'pairwise.complete.obs')
for (i in names(Q1B[,c(2:6,8:12)])) {
    cor_table_1b <- rbind(cor_table_1b, 
                          sapply(create_lags(Q1B[i]), cor, Q1B['Y_Y2Y'],
                                 use = 'pairwise.complete.obs'))
}

colnames(cor_table_1b) <- c(-4:4)
cor_table_1b

cor_table_1c <- sapply(create_lags(Q1C['Y_Q2Q']), cor, Q1C['Y_Q2Q'],
                       use = 'pairwise.complete.obs')
for (i in names(Q1C[,c(2:6, 8:12)])) {
    cor_table_1c  <- rbind(cor_table_1c , 
               sapply(create_lags(Q1C[i]), cor, Q1C['Y_Q2Q'],use = 'pairwise.complete.obs'))
}
colnames(cor_table_1c) <- c(-4:4)
cor_table_1c

### Q1d ----

## Converting time series ----
# Convert everything to a time series so I can pass hp filter for Q2
GDP_comp_TS <- GDP_comp[,-1] %>%
    mutate_all(ts, start = c(1961,1), end=c(2019,2), frequency = 4) 

cpi <- ts(final(sa_series), start = c(1961,1), end = c(2019,2), frequency = 4)

cpi8 <- ts(df %>%
               filter(Date >= '1984-01-01') %>%
               select(cpi_8),
           start = c(1984,1), end = c(2019,2), frequency = 4)

labour_var <- ts(df %>%
                     filter(Date >= '1981-01-01') %>%
                     select(Hrs_worked:Tcomp_hour), start = c(1981,1),
                 end = c(2019,2), frequency = 4)


## Creating dataframes after passing HP filters ----

HP_Series <- hpfilter(GDP_comp_TS['Y'], freq = 1600)
HP_Series <- as.data.frame(cbind(GDP_comp['Date'], 
                                 100* (HP_Series$x - HP_Series$trend)/HP_Series$trend))

for (i in c('C', 'I', 'G', 'X', 'M')) {
    HP_i <- hpfilter(GDP_comp_TS[i], freq = 1600)
    HP_Series <- cbind(HP_Series, 100* (HP_i$x - HP_i$trend)/HP_i$trend)
}
names(HP_Series)[2:7] <- c('Y_Dev', 'C_Dev', 'I_Dev', 'G_Dev', 'X_Dev', 'M_Dev')

HP_81 <- filter(HP_Series, Date >= '1981-01-01')

HP_cpi <- hpfilter(cpi, freq = 1600)

HP_cpi_series <- as.data.frame(cbind(HP_Series['Date'], HP_Series['Y_Dev'], 
                                     as.numeric(100 * (HP_cpi$x - HP_cpi$trend)/HP_cpi$trend)))
names(HP_cpi_series)[3] <- 'CPI_Dev'


HP_cpi8 <- hpfilter(cpi8, freq = 1600)
HP_cpi8_series <- as.data.frame(cbind(HP_Series[,1:2] %>% 
                                          filter(Date >= '1984-01-01'), 
                                      as.numeric(100 * (HP_cpi8$x - HP_cpi8$trend)/HP_cpi8$trend)))
names(HP_cpi8_series)[3] <- 'CPI8_Dev'

HP_hrs_wrk <- hpfilter(labour_var[,1], freq = 1600)
HP_wrk_series <- as.data.frame(cbind(HP_Series[,1:2] %>%
                                         filter(Date >= '1981-01-01'),
                                     as.numeric(100 * (HP_hrs_wrk$x - HP_hrs_wrk$trend)/HP_hrs_wrk$trend)))
names(HP_wrk_series)[3] <- 'Wrk_Dev'

HP_labor <- hpfilter(labour_var[,2], freq = 1600)
HP_labor_series <- as.data.frame(cbind(HP_Series[,1:2] %>%
                                           filter(Date >= '1981-01-01'),
                                       as.numeric(100 * (HP_labor$x - HP_labor$trend)/HP_labor$trend)))
names(HP_labor_series)[3] <- 'Labor_Prod'

HP_hrly_comp <- hpfilter(labour_var[,3], freq = 1600)
HP_hrly_series <- as.data.frame(cbind(HP_Series[,1:2] %>%
                                          filter(Date >= '1981-01-01'),
                                      as.numeric(100 * (HP_hrly_comp$x - HP_hrly_comp$trend)/HP_hrly_comp$trend)))
names(HP_hrly_series)[3] <- 'Hrly_Comp'

# Summary Statistics
summarise_all(HP_81[,-1], funs(mean))
summarise_all(select(HP_81, C_Dev:M_Dev), funs(sd))/sapply(GDP_y2y['Y_Y2Y'], sd)

summarise_all(HP_cpi_series, funs(mean), na.rm =TRUE)
summarise_all(HP_cpi8_series, funs(mean), na.rm =TRUE)
summarise_all(HP_wrk_series, funs(mean), na.rm =TRUE)
summarise_all(HP_labor_series, funs(mean), na.rm =TRUE)
summarise_all(HP_hrly_series, funs(mean), na.rm =TRUE)

sapply(HP_cpi_series['CPI_Dev'],sd)/sapply(HP_Series['Y_Dev'], sd)
sapply(HP_cpi8_series['CPI8_Dev'], sd)/sapply(HP_Series %>% 
                                                 filter(Date >= '1984-01-01') %>%
                                                 select(Y_Dev), sd)
sapply(HP_wrk_series['Wrk_Dev'],sd)/sapply(HP_Series %>% 
                                               filter(Date >= '1981-01-01') %>%
                                               select(Y_Dev), sd)
sapply(HP_labor_series['Labor_Prod'],sd)/sapply(HP_Series %>% 
                                               filter(Date >= '1981-01-01') %>%
                                               select(Y_Dev), sd)
sapply(HP_hrly_series['Hrly_Comp'],sd)/sapply(HP_Series %>% 
                                               filter(Date >= '1981-01-01') %>%
                                               select(Y_Dev), sd)
# Correlation table


cor_table_1d <- sapply(create_lags(HP_81['Y_Dev']), cor, HP_81['Y_Dev'],
                       use = 'pairwise.complete.obs')
for (i in names(HP_Series[,c(-1,-2)])) {
    cor_table_1d <- rbind(cor_table_1d, 
               sapply(create_lags(HP_81[i]), cor, HP_81['Y_Dev'],
                      use = 'pairwise.complete.obs'))
}

cor_table_1d <- rbind(cor_table_1d,
                      sapply(create_lags(HP_cpi_series %>% 
                                             filter(Date >= '1981-01-01') %>%
                                             select(CPI_Dev)), cor, HP_81['Y_Dev'],
                                         use = "pairwise.complete.obs"))
cor_table_1d <- rbind(cor_table_1d,
                      sapply(create_lags(HP_cpi8_series['CPI8_Dev']), cor, 
                             HP_81 %>% 
                                 filter(Date >= '1984-01-01') %>%
                                 select(Y_Dev),
                                         use = "pairwise.complete.obs"))
cor_table_1d <- rbind(cor_table_1d,
                      sapply(create_lags(HP_wrk_series['Wrk_Dev']), cor, HP_81['Y_Dev'],
                                         use = 'pairwise.complete.obs'))
cor_table_1d <- rbind(cor_table_1d,
                      sapply(create_lags(HP_labor_series['Labor_Prod']), cor, HP_81['Y_Dev'],
                                         use = 'pairwise.complete.obs'))
cor_table_1d <- rbind(cor_table_1d,
                      sapply(create_lags(HP_hrly_series['Hrly_Comp']), cor, HP_81['Y_Dev'],
                                         use = 'pairwise.complete.obs'))

colnames(cor_table_1d) <- c(-4,-3,-2,-1,0,1,2,3,4)
cor_table_1d

# Question 2 ----
### deviations for cpi



### Plot of consumption ----

png("CDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_Series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = C_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Real Consumption'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend') 
dev.off()

### Plot of Investment ----

png("IDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_Series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = I_Dev, color = I('grey2')), size = 1.1) +
    scale_color_manual(labels = c('Real GDP', 'Real Investment'), 
                       values = c("dodgerblue1", 'grey2')) +
    theme_bw() +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of Government Spending ----

png("GDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_Series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = G_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Real Government Spending'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of Exports ----

png("XDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_Series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = X_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Real Exports'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of Imports ----

png("MDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_Series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = M_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Real Imports'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of Headline CPI ----

png("CPIDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_cpi_series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = CPI_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Headline CPI'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of CPI excluding the 8 most volatile components ----

png("CPI8Dev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_cpi8_series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = CPI8_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Core CPI'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Plot of Hours Worked----

png("WorkDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_wrk_series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = Wrk_Dev, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Hours Worked'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Labour Productivity ----

png("LaborDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_labor_series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1'), size = 1.1) +
    geom_line(aes(y = Labor_Prod, color = I('grey2')), size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Labor Productivity'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

### Total hourly compensation ----

png("CompensationDev.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = HP_hrly_series, aes(x = Date)) + 
    geom_hline(yintercept = 0, colour = 'red') + 
    geom_line(aes(y = Y_Dev, color = 'dodgerblue1') , size = 1.1) +
    geom_line(aes(y = Hrly_Comp, color = I('grey2')) , size = 1.1) +
    theme_bw() +
    scale_color_manual(labels = c('Real GDP', 'Total Hourly Compensation'), 
                       values = c("dodgerblue1", 'grey2')) +
    labs(color = 'Series') + 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) +
    labs(x = 'Year', y = 'Percentage Deviation from Trend')
dev.off()

# Question 3 Plot ----

png("GDPshare.png", width = 465, height = 225, units='mm', res = 300)
ggplot(data = df, aes(x = Date)) +
    theme_bw() +
    geom_line(aes(y = Q3C, color = 'red2'), size = 1.25) + 
    geom_line(aes(y = Q3I, color = 'green3'), size = 1.25) +
    geom_line(aes(y = Q3G, color = 'royalblue1'), size = 1.25) +
    geom_line(aes(y = Q3X, color = 'magenta1'), size = 1.25) + 
    geom_line(aes(y = Q3M, color = 'darkorange2'), size = 1.25) +  
    labs(color = 'Series',
         y = 'Ratio of GDP (%)',
         x = 'Year') +
    .
+ 
    theme(legend.position="bottom",
          legend.text = element_text(size=22),
          axis.text = element_text(size=22),
          axis.title = element_text(size=26),
          plot.title=element_text(size = 26),
          legend.title=element_text(size= 26),
          # Change legend key size and key width
          legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(2.0,"cm")) 
dev.off()

#Q4 ----

library(gganimate)

q4df <- read.csv("D:/Users/Ziqiu/OneDrive/Documents/Masters Courses/EC640 Macroeconomics/Q4.csv",
                 header = TRUE)
q4df[,-1] <- as.data.frame(sapply(q4df[-1], as.numeric))
q4df[,1] <- as.Date(as.character(q4df[,1]), format = "%m/%d/%Y")

q4df_complete <- q4df[complete.cases(q4df),]

#exploratory data analysis of velocity of money y2y growth and inflation

cor(q4df['vM2_1'], q4df['GBond10y'], 'pairwise.complete.obs')
cor(q4df['vM2_11'], q4df['GBond10y'], 'pairwise.complete.obs')
cor(q4df['vM1_1'], q4df['GBond10y'], 'pairwise.complete.obs')
cor(q4df['vM1_11'], q4df['GBond10y'], 'pairwise.complete.obs')

cor(q4df['vM2_1'], q4df['GBond1t3y'], 'pairwise.complete.obs')
cor(q4df['vM2_11'], q4df['GBond1t3y'], 'pairwise.complete.obs')
cor(q4df['vM1_1'], q4df['GBond1t3y'], 'pairwise.complete.obs')
cor(q4df['vM1_11'], q4df['GBond1t3y'], 'pairwise.complete.obs')

cor(q4df['vM2_1'], q4df['GBond3t5y'], 'pairwise.complete.obs')
cor(q4df['vM2_11'], q4df['GBond3t5y'], 'pairwise.complete.obs')
cor(q4df['vM1_1'], q4df['GBond3t5y'], 'pairwise.complete.obs')
cor(q4df['vM1_11'], q4df['GBond3t5y'], 'pairwise.complete.obs')

cor(q4df['vM2_1'], q4df['GBond5t10y'], 'pairwise.complete.obs')
cor(q4df['vM2_11'], q4df['GBond5t10y'], 'pairwise.complete.obs')
cor(q4df['vM1_1'], q4df['GBond5t10y'], 'pairwise.complete.obs')
cor(q4df['vM1_11'], q4df['GBond5t10y'], 'pairwise.complete.obs')

### M1+ has the highest correlation to government bond yields

q4treasury <- q4df %>% filter(Date >= '1991-01-01')

P <- ggplot(data = q4treasury, aes(x = Date)) +
    geom_line(aes(y = vM1_1, color = 'red2')) +
    geom_line(aes(y = GBond10y, color =  'dodgerblue1')) +
    geom_line(aes(y = GBond5t10y, color = 'turquoise2')) +
    geom_line(aes(y = GBond3t5y, color = 'palegreen3')) + 
    geom_line(aes(y = GBond1t3y, color = 'green4')) +
    theme_bw() +
    labs(color = 'Series',
         y = 'Yield',
         x = 'Year') +
    scale_color_identity(guide = 'legend',
                         breaks = c('red2',
                                    'dodgerblue1',
                                    'turquoise2',
                                    'palegreen3',
                                    'green4'),
                         labels = c('M1+ Velocity Ratio', 
                                    'Bonds Over 10 years',
                                    'Bonds 5-10 years',
                                    'Bonds 3-5 years',
                                    'Bonds 1-3 years')) +
    scale_y_continuous(name = 'Yield %',
                       sec.axis = sec_axis(~ ., name = 'Ratio')) +
    theme(
        legend.position = c(1,1),
        axis.line.y.right = element_line(color = "red2"),
        axis.ticks.y.right = element_line(color = "red2"),
        axis.text.y.right = element_text(color = "red2"),
        axis.title.y.right = element_text(color = "red2")
    )
png("Q4.png", width = 465, height = 225, units='mm', res = 300)

P + theme(legend.position="bottom",
      legend.text = element_text(size=18),
      axis.text = element_text(size=18),
      axis.title = element_text(size=26),
      plot.title=element_text(size = 26),
      legend.title=element_text(size= 26),
          # Change legend key size and key width
      legend.key.size = unit(1.5, "cm"),
      legend.key.width = unit(2.0,"cm")) +
    guides(color = guide_legend(override.aes = list(size = 1.75)))
dev.off()

#looking at other Money supplies
# P <- ggplot(data = q4treasury, aes(x = Date)) +
#     geom_line(aes(y = vM1_1, color = 'blue')) +
#     geom_line(aes(y = vM1_11, color = 'blue')) +
#     geom_line(aes(y = vM2_1, color = 'blue')) +
#     geom_line(aes(y = vM2_11, color = 'blue')) +
#     geom_line(aes(y = GBond5t10y, color = 'red')) +
#     geom_line(aes(y = GBond1t3y, color = 'purple')) +
#     geom_line(aes(y = GBond3t5y, color = 'orange')) + 
#     geom_line(aes(y = GBond10y, color = 'grey')) 

###animate into a gif for better readability.
Pgif <- P + transition_reveal(Date) + 
    theme(legend.text = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18),
          legend.title = element_text(size = 18),
          # Change legend key size and key width
          legend.key.size = unit(0.8, "cm"),
          legend.key.width = unit(1.2, "cm"))
anim_save("Q4gif.gif", Pgif, width = 250, height = 120, units='mm', res = 200)

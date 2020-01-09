# Macroeconomic Analysis

A series of assignments analyzing real GDP seasonally adjusted data from 1961Q1 - 2019Q2 using R, Stata and the latex markup language. Content covered includes: explanatory data analysis, hypothesis testing and solving dynamic stochastic general equilibrium (DSGE) models.

While the course is taught in Stata, all the code with the exception for that related to the DSGE models, are performed in R due to ease of familiariy in data wrangling and better presentation. Notably, dplyr and ggplot2 libraries covered a majority of the code used. Stata was used to solve DSGE models as R had a comparabily weaker library.

## Assignment Content

### Assignment 1

The main focus of this assignment was to gain a deeper understanding of the national accounts' (Real GDP, consumption, government expenditure, investment, imports and exports) historical data, as well as presenting tables and figures in a professional manner. As such, many of the questions involved exploratory data analysis using a highpass filter. All data was extracted from Statistics Canada.

An interesting part of the assignment was in part 4, where we were tasked to make an interesting observation using data extracted from the Bank of Canada. Particularly, I found a high correlation of 0.9 between the velocity of M1+ and Government of Canada's marketable bond yields and provided an explationation based on monetarism’s quantity theory of money. I also made an animated graph using the library gganimate for which the code and output can be found [here.](http://rpubs.com/ZiqZhu/EC640Q4)

## Assignment 2

This assignment was a bit lax, as it was due right before the midterm. We continued to perform exploratory data analysis on growth rates, particularly output growth and  output levels across countries in search of a significant correlation. However, the main task in the assignment was to solve a Solow model and two-period models as an introduction to DSGE models.


## Assignment 3

The final assignment tasked us to solve for solow residuals, focusing on decentralized and RBC models with different utility functions. VARS was also taught in the course, however I did not include that part in the publishedd assignment.
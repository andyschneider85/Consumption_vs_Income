
"""

The following code conducts exploratory data analysis on the relationship between
spending and income for several categories relevant to the fund's portfolio companies. 
It pulls in data on beer consumption, chocolate chocolate consumption, premium beauty 
and personal care consumption, mass beauty and personal care consumption, and 
dispoable personal income (all in per capita terms) for 80 countries from 2000 to 2014. 
It cleans and transforms the data as necessary to plot a simple regression and smoothed Loess 
line for each consumption category against personal disposable income. From these plots we 
can get a quick idea of the category's S-curve showing key inflection points, specifically
at what income level category consumption accelerates (takeoff) and at which level it slows
(saturation). Armed with this knowledge, we can then assess which countries are primed for
growth versus slowdown in our four categories (beer, chocolate, mass beauty and personal 
care, premium beauty and personal care).

"""

# Read in Euromonitor data 
library(xlsx)

food <- read.xlsx('Food.xlsx', sheetIndex = 1, header = TRUE)
beauty <- read.xlsx('Beauty.xlsx', sheetIndex = 1, header = TRUE)
income <- read.xlsx('Income.xlsx', sheetIndex = 1, header = TRUE)

# Separate the different consumption categories and income 
beer <- food[1:80,]
chocolate <- food[81:160,]
prem_beauty <- beauty[1:80,]
mass_beauty <- beauty[81:160,]
income <- income[1:80,]

# Transform data so that we have all values in a single column
beer_melt <- melt(beer, id=c("Categories","Geographies"))
chocolate_melt <- melt(chocolate, id=c("Categories","Geographies"))
prem_beauty_melt <- melt(prem_beauty, id=c("Categories","Geographies"))
mass_beauty_melt <- melt(mass_beauty, id=c("Categories","Geographies"))
income_melt <- melt(income, id=c("Categories","Geographies"))

# Combine transformed consumption data with income data 
beer_income <- cbind(beer_melt$value, income_melt$value)
colnames(beer_income) <- c("per capita beer consumption", "per capita disposable income")

chocolate_income <- cbind(chocolate_melt$value, income_melt$value)
colnames(chocolate_income) <- c("per capita chocolate consumption", "per capita disposable income")
chocolate_income[,1] <- round(chocolate_income[,1], digits = 2)

prem_beauty_income <- cbind(prem_beauty_melt$value, income_melt$value)
colnames(prem_beauty_income) <- c("per capita premium beauty consumption", "per capita disposable income")

mass_beauty_income <- cbind(mass_beauty_melt$value, income_melt$value)
colnames(mass_beauty_income) <- c("per capita mass beauty consumption", "per capita disposable income")

# Get rid of observations with NA values
beer_income_clean <- beer_income[complete.cases(beer_income),]
chocolate_income_clean <- chocolate_income[complete.cases(chocolate_income),]
prem_beauty_income_clean <- prem_beauty_income[complete.cases(prem_beauty_income),]
mass_beauty_income_clean <- mass_beauty_income[complete.cases(mass_beauty_income),]

# Produce enhanced scatterplots emphasizing Loess smoothing lines
library(car)
beer_scatter <- scatterplot(beer_income_clean[,2], beer_income_clean[,1], smoother = loessLine,
            xlab = ("Per Capita Disposable Income (Constant USD)"),
            ylab = ("Per Capita Beer Consumption (Liters)")) 

chocolate_scatter <- scatterplot(chocolate_income_clean[,2], chocolate_income_clean[,1], smoother = loessLine,
                            xlab = ("Per Capita Disposable Income (Constant USD)"),
                            ylab = ("Per Capita Chocolate Consumption (kg)")) 

prem_beauty_scatter <- scatterplot(prem_beauty_income_clean[,2], prem_beauty_income_clean[,1], smoother = loessLine,
                            xlab = ("Per Capita Disposable Income (Constant USD)"),
                            ylab = ("Per Capita Premium Beauty and Personal Care Spend (Constant USD)")) 

mass_beauty_scatter <- scatterplot(mass_beauty_income_clean[,2], mass_beauty_income_clean[,1], smoother = loessLine,
                            xlab = ("Per Capita Disposable Income (Constant USD)"),
                            ylab = ("Per Capita Mass Beauty and Personal Care Spend (Constant USD)")) 

# Export scatterplots as pdfs
dev.copy2pdf(file="beer_scatter.pdf", width=8, height=10)
dev.copy2pdf(file="chocolate_scatter.pdf", width=8, height=10)
dev.copy2pdf(file="prem_beauty_scatter.pdf", width=8, height=10)
dev.copy2pdf(file="mass_beauty_scatter.pdf", width=8, height=10)

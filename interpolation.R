#the script is written by Katalin Buzasi
#katalinbuzasi.weebly.com


# setting working directory: use your own path to your preferred folder on your computer
path="c:/Documents/project/interpolation"
setwd(path)

# If you need to install the packages, delete the # sign and run the code
# the DemoTools package needs to be installed manually
# see how it is done at https://timriffe.github.io/DemoTools/

#install.packages("readxl")
library(readxl)
#install.packages("writexl")
library(writexl)
library(DemoTools)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("reshape2")
library(reshape2)
#install.packages("dplyr")
library(dplyr)

##########################################################################

#                      PART 1: PREPARING AND CONDUCTING
#                      THE INTERPOLATION PROCESS

###########################################################################


# importing the datasets from working directory
men <- read_xlsx("men_census_pop.xlsx")
women <- read_xlsx("women_census_pop.xlsx")


# inspecting the first five rows
View(men[1:5,])
View(women[1:5,])

# inspecting the last five rows
View(men[87:91,])
View(women[87:91,])

# dataframes containing only the population numbers (each column is a year)

men_limited <- men[,2:10]
women_limited <- women[,2:10]
View(men_limited[1:5,])
View(women_limited[1:5,])

# saving age vector
age <- men$age

# exact dates of censuses
datesIn  <- c("1849-11-19", "1859-12-13", "1869-12-01", "1879-12-31", "1889-12-31",
              "1899-12-31", "1909-12-31", "1920-12-31", "1930-12-31")

# the dates that we need the population estimates for
datesOut <- c("1850-07-01","1851-07-01", "1852-07-01", "1853-07-01", "1854-07-01", "1855-07-01", "1856-07-01", "1857-07-01", "1858-07-01", "1859-07-01",
              "1860-07-01","1861-07-01", "1862-07-01", "1863-07-01", "1864-07-01", "1865-07-01", "1866-07-01", "1867-07-01", "1868-07-01", "1869-07-01",
              "1870-07-01","1871-07-01", "1872-07-01", "1873-07-01", "1874-07-01", "1875-07-01", "1876-07-01", "1877-07-01", "1878-07-01", "1879-07-01",
              "1880-07-01","1881-07-01", "1882-07-01", "1883-07-01", "1884-07-01", "1885-07-01", "1886-07-01", "1887-07-01", "1888-07-01", "1889-07-01",
              "1890-07-01","1891-07-01", "1892-07-01", "1893-07-01", "1894-07-01", "1895-07-01", "1896-07-01", "1897-07-01", "1898-07-01", "1899-07-01",
              "1900-07-01","1901-07-01", "1902-07-01", "1903-07-01", "1904-07-01", "1905-07-01", "1906-07-01", "1907-07-01", "1908-07-01", "1909-07-01",
              "1910-07-01","1911-07-01", "1912-07-01", "1913-07-01", "1914-07-01", "1915-07-01", "1916-07-01", "1917-07-01", "1918-07-01", "1919-07-01",
              "1920-07-01","1921-07-01", "1922-07-01", "1923-07-01", "1924-07-01", "1925-07-01", "1926-07-01", "1927-07-01", "1928-07-01", "1929-07-01")


# LINEAR 
# men
men_linear <- interp(men_limited, datesIn, datesOut, "linear", rule = 2)
men_linear <- round(men_linear, 0)
colnames(men_linear) <- seq(from=1850, to=1929, by=1)
men_linear <- cbind(age, men_linear)
men_linear <- as.data.frame(men_linear)

View(men_linear[1:5,])

# women
women_linear <- interp(women_limited, datesIn, datesOut, "linear", rule = 2)
women_linear <- round(women_linear, 0)
colnames(women_linear) <- seq(from=1850, to=1929, by=1)
women_linear <- cbind(age, women_linear)
women_linear <- as.data.frame(women_linear)

# EXPONENTIAL

# men
men_exponential <- interp(men_limited, datesIn, datesOut, "exponential", rule = 2)
men_exponential <- round(men_exponential, 0)
colnames(men_exponential) <- seq(from=1850, to=1929, by=1)
men_exponential <- cbind(age, men_exponential)
men_exponential <- as.data.frame(men_exponential)

# women
women_exponential <- interp(women_limited, datesIn, datesOut, "exponential", rule = 2)
women_exponential <- round(women_exponential, 0)
colnames(women_exponential) <- seq(from=1850, to=1929, by=1)
women_exponential <- cbind(age, women_exponential)
women_exponential <- as.data.frame(women_exponential)

# saving our files in xlsx format
write_xlsx(men_linear, "men_midyear_linear.xlsx", col_names = TRUE)
write_xlsx(women_linear, "women_midyear_linear.xlsx", col_names = TRUE)
write_xlsx(men_exponential, "men_midyear_exponential.xlsx", col_names = TRUE)
write_xlsx(women_exponential, "women_midyear_exponential.xlsx", col_names = TRUE)

##########################################################################

#                      PART 2: AGGREGATING AGE GROUPS

###########################################################################


# men_linear

## creating age groups

men_linear$age_group <- cut(men_linear$age, c(0, 5, 15, 25, 35, 45, 55, 100), right=FALSE)
# inspecting the first 16 rows and the first, second, third, fourth and last
# column of the resulting table
View(men_linear[1:16, c(1,2,3,4,5,82)])
# deleting the first column
men_linear <- men_linear[,-1]

# summary
aggr_men_linear <- men_linear %>% 
  group_by(age_group) %>% 
  summarise(across(everything(), sum))

# women linear
women_linear$age_group <- cut(women_linear$age, c(0, 5, 15, 25, 35, 45, 55, 100), right=FALSE)
women_linear <- women_linear[,-1]

aggr_women_linear <- women_linear %>% 
  group_by(age_group) %>% 
  summarise(across(everything(), sum))

# men exponential
men_exponential$age_group <- cut(men_exponential$age, c(0, 5, 15, 25, 35, 45, 55, 100), right=FALSE)
men_exponential <- men_exponential[,-1]

aggr_men_exponential <- men_exponential %>% 
  group_by(age_group) %>% 
  summarise(across(everything(), sum))

# women exponential
women_exponential$age_group <- cut(women_exponential$age, c(0, 5, 15, 25, 35, 45, 55, 100), right=FALSE)
women_exponential <- women_exponential[,-1]

aggr_women_exponential <- women_exponential %>% 
  group_by(age_group) %>% 
  summarise(across(everything(), sum))

# saving these tables in the working directory in xlsx format

write_xlsx(aggr_men_linear, "aggr_men_linear.xlsx", col_names = TRUE)
write_xlsx(aggr_women_linear, "aggr_women_linear.xlsx", col_names = TRUE)
write_xlsx(aggr_men_exponential, "aggr_men_exponential.xlsx", col_names = TRUE)
write_xlsx(aggr_women_exponential, "aggr_women_exponential.xlsx", col_names = TRUE)


##########################################################################

#                      PART 3: COMPARISON WITH EXISTING DATA
#                        FROM THE YEARBOOKS

###########################################################################

# importing Yearbook population data
JB_pop <- read_excel("JB_pop.xlsx")

# comparing interpolated and yearbook population


# the dates that we need the population estimates for
datesOut_yearend <- c("1850-12-31","1851-12-31", "1852-12-31", "1853-12-31", "1854-12-31", "1855-12-31", "1856-12-31", "1857-12-31", "1858-12-31", "1859-12-31",
              "1860-12-31","1861-12-31", "1862-12-31", "1863-12-31", "1864-12-31", "1865-12-31", "1866-12-31", "1867-12-31", "1868-12-31", "1869-12-31",
              "1870-12-31","1871-12-31", "1872-12-31", "1873-12-31", "1874-12-31", "1875-12-31", "1876-12-31", "1877-12-31", "1878-12-31", "1879-12-31",
              "1880-12-31","1881-12-31", "1882-12-31", "1883-12-31", "1884-12-31", "1885-12-31", "1886-12-31", "1887-12-31", "1888-12-31", "1889-12-31",
              "1890-12-31","1891-12-31", "1892-12-31", "1893-12-31", "1894-12-31", "1895-12-31", "1896-12-31", "1897-12-31", "1898-12-31", "1899-12-31",
              "1900-12-31","1901-12-31", "1902-12-31", "1903-12-31", "1904-12-31", "1905-12-31", "1906-12-31", "1907-12-31", "1908-12-31", "1909-12-31",
              "1910-12-31","1911-12-31", "1912-12-31", "1913-12-31", "1914-12-31", "1915-12-31", "1916-12-31", "1917-12-31", "1918-12-31", "1919-12-31",
              "1920-12-31","1921-12-31", "1922-12-31", "1923-12-31", "1924-12-31", "1925-12-31", "1926-12-31", "1927-12-31", "1928-12-31", "1929-12-31")

# LINEAR 
# men
men_linear_yearend <- interp(men_limited, datesIn, datesOut_yearend, "linear", rule = 2)
men_linear_yearend <- round(men_linear_yearend, 0)
colnames(men_linear_yearend) <- seq(from=1850, to=1929, by=1)
men_linear_yearend <- cbind(age, men_linear_yearend)
men_linear_yearend <- as.data.frame(men_linear_yearend)

# women
women_linear_yearend <- interp(women_limited, datesIn, datesOut_yearend, "linear", rule = 2)
women_linear_yearend <- round(women_linear_yearend, 0)
colnames(women_linear_yearend) <- seq(from=1850, to=1929, by=1)
women_linear_yearend <- cbind(age, women_linear_yearend)
women_linear_yearend <- as.data.frame(women_linear_yearend)

# EXPONENTIAL

# men
men_exponential_yearend <- interp(men_limited, datesIn, datesOut_yearend, "exponential", rule = 2)
men_exponential_yearend <- round(men_exponential_yearend, 0)
colnames(men_exponential_yearend) <- seq(from=1850, to=1929, by=1)
men_exponential_yearend <- cbind(age, men_exponential_yearend)
men_exponential_yearend <- as.data.frame(men_exponential_yearend)

# women
women_exponential_yearend <- interp(women_limited, datesIn, datesOut_yearend, "exponential", rule = 2)
women_exponential_yearend <- round(women_exponential_yearend, 0)
colnames(women_exponential_yearend) <- seq(from=1850, to=1929, by=1)
women_exponential_yearend <- cbind(age, women_exponential_yearend)
women_exponential_yearend <- as.data.frame(women_exponential_yearend)



# aggregating age-specific population interpolations by year
men_lin_yearend <- colSums(men_linear_yearend[,-1])
women_lin_yearend <- colSums(women_linear_yearend[,-1])
men_exp_yearend <- colSums(men_exponential_yearend[,-1])
women_exp_yearend <- colSums(women_exponential_yearend[,-1])

# wide table
## compiling the data into a single dataset
comparison_gender <- JB_pop[-1,]
comparison_gender$men_lin <- men_lin_yearend
comparison_gender$women_lin <- women_lin_yearend
comparison_gender$men_exp <- men_exp_yearend
comparison_gender$women_exp <- women_exp_yearend

## calculating the difference between the actual and interpolated data
comparison_gender$diff_men_lin <- comparison_gender$men-comparison_gender$men_lin
comparison_gender$diff_women_lin <- comparison_gender$women-comparison_gender$women_lin
comparison_gender$diff_men_exp <- comparison_gender$men-comparison_gender$men_exp
comparison_gender$diff_women_exp <- comparison_gender$women-comparison_gender$women_exp

## creating long tables with necessary information only

### plotting differences

diff <- subset(comparison_gender, select=c("year", "diff_men_lin", "diff_women_lin", "diff_men_exp", "diff_women_exp"))

diff <- melt(diff, id.vars = "year",
       measure.vars = c("diff_men_lin", "diff_women_lin", "diff_men_exp", "diff_women_exp"),
        variable.name = "type",
        value.name = "pop")

figure9 <- ggplot(diff, aes(x=year, y=pop, group=type, color=type)) +
  geom_line(size=1) +
  geom_vline(xintercept = c(1849, 1859, 1869, 1879, 1889, 1899, 1909, 1920, 1930), linetype="dotted", color = "black", size=0.75) +
  ggtitle("Differences between actual and interpolated population", subtitle = "dotted vertical lines refer to census dates") +
  ylab("count")+
  xlab("")
figure9
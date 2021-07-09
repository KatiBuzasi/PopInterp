# PopInterp
Interpolating population from historical censuses


The main goal of this repository is to provide a step-by-step guide/tutorial on how to conduct interpolation on age-structured population data. This repository is expected to be useful for historians, historical demographers and historical epidemiologists who often need between-census population data to calculate mortality and other health-related rates.

The tutorial is mainly based on the "interp" package in the "DemoTools" R library (Riffe et al. 2019, https://rdrr.io/github/timriffe/DemoTools/man/interp.html) and census data between 1849 and 1930 from Amsterdam, the Netherlands (http://www.volkstellingen.nl/nl/index.html). 

The tutorial has been made as part of the research project titled "Lifting the burden of disease. The modernisation of health in the Netherlands: Amsterdam 1854-1940" (https://www.ru.nl/rich/our-research/research-groups/radboud-group-historical-demography-family-history/current-research-projects/current-projects/lifting-burden-disease/).

The repository consists of the following files:
1. Basic datasets in xlsx format ("JB_pop.xlsx", "men_census_pop.xlsx", "women_census_pop.xlsx")
2. R script ("interpolation.R")
3. Step-by-step tutorial in pdf form ("population interpolation tutorial_Buzasi.pdf")
4. The results of the interpolation exercise in .xlsx format: men_midyear_linear.xlsx, men_midyear_exponential.xlsx, women_midyear_linear.xlsx, women_midyear_exponential.xlsx 


References

Riffe, T., Aburto, J., Alexander, M., Fennell, S., Kashnitsky, I., Pascariu, M., and Gerland, P. (2019). DemoTools: An R package of tools for aggregate demographic analysis.
URL: https://github.com/timriffe/DemoTools/

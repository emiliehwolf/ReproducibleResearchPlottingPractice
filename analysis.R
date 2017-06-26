########################################################################
## analysis.R
## Author: Emilie H. Wolf
## Date: Sunday, June 25, 2017
## Description: This is the optional assignment in the first week of
## Reproducible Research. The goal is to create plots that answer
## two questions about medical payments in different US states.
## 
## Please ensure this R script is in the same folder as the payments.csv
## file and that your working directory is set to that folder.
########################################################################

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

## Read in dataset and reclassify 2 variables as factors
payments <- read.csv("payments.csv", stringsAsFactors = FALSE)
payments$Provider.State <- factor(payments$Provider.State)
payments$DRG.Definition <- factor(payments$DRG.Definition)


## QUESTION 1
## What is the relationship between mean covered charges (Average.Covered.Charges) 
## and mean total payments (Average.Total.Payments) in New York?

NY <- subset(payments, Provider.State == "NY", select = c(Average.Covered.Charges,Average.Total.Payments))

plot0 <- ggplot(data = NY, aes(x = Average.Covered.Charges, y = Average.Total.Payments)) + 
        geom_point() + 
        geom_smooth(method = 'lm', se = FALSE) +
        scale_x_log10(name = "Mean Covered Charges\n(log scale in $)", breaks=seq(5000,135000,10000)) + 
        scale_y_log10(name = "Mean Total Payments\n(log scale in $)", breaks=seq(5000,40000,10000)) +
        theme(axis.text.x = element_text(angle=-60)) +
        labs(title="In New York, Higher Mean Covered Charges Are Associated with Higher Mean Total Payments")

ggsave(filename = "plot0.pdf", plot = plot0, width = 10, height = 7.5, units = "in")


## QUESTION 2
## How does the relationship between mean covered charges (Average.Covered.Charges)
## and mean total payments (Average.Total.Payments) vary by medical condition
## (DRG.Definition) and the state in which care was received (Provider.State)?

## Colorblind palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot1 <- ggplot(data = payments, aes(x = Average.Covered.Charges, y = Average.Total.Payments)) +
        geom_point(size = .2, aes(color = DRG.Definition)) +
        geom_smooth(aes(color = DRG.Definition), method = 'lm', se = FALSE) +
        scale_color_manual(values=cbPalette) +
        scale_x_log10(name = "Mean Covered Charges\n(log scale in $)") + 
        scale_y_log10(name = "Mean Total Payments\n(log scale in $)") +
        labs(title="Mean Covered Charges vs Mean Total Payments\nby State and Medical Condition") +
        theme(legend.position = "bottom") +
        guides(color = guide_legend("Medical\nConditions", ncol=1)) +
        facet_wrap( ~ Provider.State, ncol = 2)

ggsave(filename = "plot1.pdf", plot = plot1, width = 7.5, height = 10, units = "in")

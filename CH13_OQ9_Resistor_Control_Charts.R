# Assignment 05 - Chapter 13, Problem 9OQ
# Author: Dan Klemfuss
# Textbook: Operations and Supply Chain Management (14th Ed) McGraw-Hill Education

################################################################################
#' PROLBLEM STATEMENT:
#' Resistors for electronic circuits are manufactured on a high-speed automated 
#' machine. The machine is set up to produce a large run of resistors of 
#' 1,000 ohms each. To set up the machine and create a control chart to be used 
#' throughout the run, 15 samples were taken with four resistors in each sample. 
#' The complete list of samples and their measured values are as follows: 
#' 
#'         <Note: Data captured as CSV file in ./data folder>
#'         
#' Develop an Xbar-chart and R-chart and plot the values. From the charts, what 
#' comments can you make about the process? (Use the three-sigma control limits
#' as in Exhibit 13.7)
#' 
#'       <Note: Exhnibit 13.7 captured as CSV file in ./data folder>
#'       
################################################################################

library(dplyr)
library(data.table)
library(plotly)

## 1. Specify Measured Resistor data, and exhibit 13.7 from CSV's:
resistor.data.file <- "./data/Ch13_OQ9_data.csv"
ex.13.7.file <- "./data/Exhibit_13.7.csv"

## 2. Ingest data: 
resistor_data <- fread(resistor.data.file)
xbar_rbar_data <- fread(ex.13.7.file)

## 3. Clean Resistor data to make each Sample # a factor of the measured values: 
resistor_data <- melt(resistor_data, id.vars='Sample_Number', 
                      variable.name="Reading_Number",
                      value.name="Resistance_Ohms")

## 4. Determine the mean (xbar) and Range (range) for each sample: 
resistor_summary <- resistor_data %>% group_by(Sample_Number) %>% summarize(
  xbar = mean(Resistance_Ohms, na.rm=T),
  range = max(Resistance_Ohms) - min(Resistance_Ohms),
  n = dplyr::n()
)

## 5. Calculate the x-double-bar (x2bar) and r-bar values: 
x2bar <- mean(resistor_summary$xbar)
rbar <- mean(resistor_summary$range)

## 5. Calculate X bar and R bar chart control limits using Exhibit 13.7:
#' For X bar chart (Ref. Exhibit 13.7): 
#' Upper Control Limit (UCL)_xbar = x2bar + A2*Rbar
#' Lower Control Limit (LCL)_xbar = x2bar - A2*Rbar

#' For R bar chart (Ref. Exhibit 13.7):
#' Upper Control Limit (UCL)_rbar = D4*Rbar
#' Lower Control Limit (LCL)_rbar = D3*Rbar

#' Use the appropriate A2, D3, and D4 values using the number of observations (n)
#' previously determined in resistor_summary (Note: The code below accounts for 
#' variations in observations in sample size, which is not needed for this 
#' problem since each sample has 4 observations, but is more robust for future use)

chart_data <- merge(resistor_summary, xbar_rbar_data,
        by.x='n',by.y='Number_Observations_in_Each_Sample_n') %>% mutate(
          x2bar = x2bar,
          rbar = rbar,
          UCL_xbar = x2bar + Factor_for_Xbar_Chart_A2*rbar,
          LCL_xbar = x2bar - Factor_for_Xbar_Chart_A2*rbar,
          UCL_rbar = Upper_Control_Limit_D4*rbar,
          LCL_rbar = Lower_Control_Limit_D3*rbar,
        )

# Plot the Xbar chart: 
fig1 <- plot_ly(chart_data, x=~Sample_Number, y=~xbar, 
                type='scatter', mode='lines', name='xbar') %>%
  add_trace(y=~UCL_xbar, name="UCL", mode='lines') %>%
  add_trace(y=~LCL_xbar, name="LCL", mode='lines')

fig1 # Plot figure in user's selected graphics renderer


# Plot the R chart:
fig2 <- plot_ly(chart_data, x=~Sample_Number, y=~range, 
                type='scatter', mode='lines', name='range') %>%
  add_trace(y=~UCL_rbar, name="UCL", mode='lines') %>%
  add_trace(y=~LCL_rbar, name="LCL", mode='lines')

fig2 # Plot figure in user's selected graphics renderer









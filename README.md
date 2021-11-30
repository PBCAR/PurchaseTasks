[![DOI](https://zenodo.org/badge/305256955.svg)](https://zenodo.org/badge/latestdoi/305256955)

# PurchaseTasks Repository 
This repository contains the script for processing purchase task data. This READ ME document introduces users to purchase tasks, and walks users through the process of running this script who have little working knowledge of R.

# Introduction to Purchase Tasks

These demand instruments are used to measure the reinforcer pathology - the extent to which a value for a commodity is effected by increased cost. Greater demand (i.e. little sensitivity to price changes) is often associated with substance-related problems and use disorders (see Bickel et al. (2011). The Behavioral Economics and Neuroeconomics of Reinforcer Pathologies: Implications for Etiology and Treatment of Addiction). There are 5 different indices generated by the purchase task:

**Breakpoint** - The first increment of cost with zero consumption

**Intensity** - Consumption at the first price point

**Omax** - The maximum expenditure

**Pmax** - The price associated with the maximum total expenditure

**Elasticity** - Measures sensitivity of consumption to increases in cost


# Overview of Processing

Purchase tasks often use branching logic in administering the questions, such that no further prices are presented after a zero response within an array. Individuals who contradict themselves at the last item of the array are considered missing and are removed, since it cannot be assumed that all subsequent responses would be zeroes.

## Quality control removes individuals who:

i) Do not exhibit a decelerating trend (referred to as a trend violation). Does not include those with a starting value of 0

ii) Exhibit a bounce ratio of of 10% or higher (inconsistencies in values given)

iii) Exhibit 2 or more reversals. A reversal is 2 or more consecutive 0's prior to a positive value


**Winsorization at the price level** occurs for values with a z score +/- 3.99, whereby the outlying value is changed to one unit above the nearest non-outlying value. For the price level, a unit is 1 standard amount consumed such as 1 drink, 1 cigarette, or 1 gram of cannabis.

**Winsorizing at the index level** occurs for values with a z score +/- 3.99. Outliers are recoded as 0.001 greater than the next highest non-outlying value, thus retaining order.

The winsorizing type used for both price and index level values is preserve order winsorization, as opposed to other techniques such as replacing all outliers with the corresponding +/- 3.99 regular value rounded up or with a value that is a unit higher than the highest non-outlying value (see {beezdemand} package documentation).

# Purchase Task Template

Processed data provides two sets of these indices:

i) Winsorized variables (at both the price and index level), as denoted by the suffix `_W`

ii) Non-winsorized variables (no outlier management)


## Packages:

This script utilizes the {[beezdemand](https://github.com/brentkaplan/beezdemand)} package to process the behavioural economic data. It also utilizes {dplyr} and {psych} packages

## Changes Required:

There are 7 changes required by the user of this script. These are outlined at the top of the script, with examples:

a) Set your working directory - This is the file location of your data to be analyzed. To set your working directory, go to:

      Session > Set Working Directory > Choose Directory
 
b) Input the name of your data file (this script is set up to only import .csv files)

c) Input the name of your ID variable

d) Copy and Paste all the names of the purchase task items

e) Assign the price associated with each purchase task item

f) Input the maximum allowed value identified in the purchase task

g) Input the total N individuals in the data set


## The Script:

The rest of the script provides prompts for the user, specifically pointing out when they should look at the output in the console for any possible changes/ information needed. At the end, a summary of the data is provided (also in the console). The final part of the script exports the processed data and summary tables to the working directory that was set at the beginning of the document (4 .csv files total):

i) "purchase.task.csv" - This merges the processed data with the original inputted data (all N)

ii) "PT.variables.csv" - This provides the descriptive statistics of the purchase task variables (both winsorized and non-winsorized)

iii) "price.level.variables.csv" - This provides the descriptive statistics of the price-level variables (the initial items of the purchase task both winsorized and non-winsorized)

iv) "Appendix.csv" - This provides a table of all the outlying values that were changed by ID (through winsorizing)

# Purchase Task Report

This script uses R-markdown to produce a PDF report of the processed data. It is an alternative to the output produced by the `Purchase Task Template.R`. The benefits of using this script to produce a report is that all the information about the data processing is provided and therefore documented.

It requires the user to:

a) Save the data set and the script in the same file location

b) Name of the .csv file

c) The name of the ID variable

d) The names of the purchase task item variables

e) The price of each purchase task item

f) Input the maximum allowed value identified in the purchase task

g) Input the total N individuals in the data set

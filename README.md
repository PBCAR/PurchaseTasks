# PurchaseTasks Repository 
This repository contains the script for processing APT, CPT, and MPT data for PBCAR. It also contains the script for creating heat maps of purchase task variables. This READ ME document walks users through the process of running this script who have little working knowledge of R.


# Purchase Task Template

### Packages:

This script utilizes the 'beezdemand' package to process the behavioural economic data. It also utilizes 'dplyr' and 'psych' packages

### Changes Required:

There are 5 changes required by the user of this script. These are outlined at the top of the script, with examples:

i) Set your working directory - This is the file location of your data to be analyzed. To set your working directory, go to:

      Session > Set Working Directory > Choose Directory
  
ii) Input the name of your data file (this script is set up to only import .csv files)
  
iii) Select the type of purchase task: APT, CPT, or MPT

iv) Copy and Paste the ID variable name, along with the name of the purchase task items ONLY

v) Input the total N of participants in the data set

### The Script:

The rest of the script provides prompts for the user, specifically pointing out when they should look at the output in the console for any possible changes/ information needed. At the end, a summary of the data is provided (also in the console). The final part of the script exports the processed data and summary tables to the working directory that was set at the beginning of the document (4 .csv files total):

i) "purchase.task.csv" - This merges the processed data with the original inputted data (all N)

ii) "PT.variables.csv" - This provides the descriptive statistics of the purchase task variables (both winsorized and non-winsorized)

iii) "price.level.variables.csv" - This provides the descriptive statistics of the price-level variables (the initial items of the purchase task)

iv) "Appendix A.csv" - This provides a table of all the outlying values that were changed by ID


# Heat Map Template

This heatmap script utilizes 'pairwise complete' observations, meaning only individuals with missing data for the 2 variables being compared will be removed.

### Packages:

This script uses 'corrplot', 'Hmisc', 'ggplot2', and 'reshape2' packages

### Changes Required:

There are 3 changes required, with a further 3 option changes to the script total. These are outlined at the top of the script with examples:

i) Set your working directory - This is the file location of your data to be analyzed. To set your working directory, go to:

      Session > Set Working Directory > Choose Directory

ii) Input the name of your data file (this script is set up to only import .csv files)

iii) Select the names of variables within your data file to include in the heatmap

iv) Change the variable names (optional)

v) Reorder the variables (optional)

vi) Change the number of decimal points displayed (default is 2)

### The Script:

i) The heatmap itself

ii) 'heatmap.pvalues.csv' - A file of the p.values for each variable pair

iii) 'corr.pair.numbers.csv' - A file of the number of pairwise complete observations

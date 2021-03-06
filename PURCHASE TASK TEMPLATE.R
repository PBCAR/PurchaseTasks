#################################################################################################
##############################     PURCHASE TASK CODE TEMPLATE    ###############################
#################################################################################################

##### ----------  REQUIRED CHANGES BY USER:
#################################################################################################

### a) CHANGE file directory: SESSION > SET WORKING DIRECTORY > CHOOSE DIRECTORY

setwd("~/Desktop/PURCHASE TASK FOR RELEASE/")

### b) NAME of .csv file:

pt.name <- "PBCAR_PT.csv"

### c) NAME of ID variable:

id.name <- "ID"

### d) COPY & PASTE all purchase task item names here:

purchase.task.names <- c("apt000","apt025","apt050","apt1","apt150",
                         "apt2","apt250","apt3","apt4","apt5","apt6",
                         "apt7","apt8","apt9","apt10","apt11","apt12",
                         "apt13","apt14","apt15","apt16","apt18","apt20",	
                         "apt22","apt24","apt26","apt28","apt30","apt35","apt40")

### e) ASSIGN the price associated with each purchase task item:

prices <- c("0","0.25","0.50","1","1.50","2","2.50","3","4","5","6",
            "7","8","9","10","11","12","13","14","15","16","18","20",
            "22","24","26","28","30","35","40")

### f) IDENTIFY the maximum allowed value in the purchase task:

max.val <- 99

### g) IDENTIFY total N individuals in data set:

tot.n <- 730

##### ----------  OPTIONAL CHANGES:
#################################################################################################

### The k-values to test:
k.span <- c(2,3,4)

### The Bounce criteria (default 10%):
bounce.crit <- 0.1

### The Winsorizing type: 'preserve_order', '1_higher_sd', or '1_higher_max_non_outlier'
wins.type <- 'preserve_order'

#################################################################################################
##### STEP 0: DATA INPUT AND FORMATTING PRIOR TO CLEANING AND PROCESSING
#################################################################################################
library(dplyr)
library(psych)
library(beezdemand)

purchase.task.df <- read.csv(pt.name)
purchase.task.df <- purchase.task.df[c(id.name,purchase.task.names)]

# RENAMES the columns in the data frame to "id" (required), plus the price of each purchase task item

item.names <- c("id",prices)
colnames(purchase.task.df) <- item.names

#################################################################################################
##### STEP 1: IMPUTE ALL PERTINENT ZEROS
#################################################################################################
# If the purchase task uses branching logic such that no further prices are presented after a
# zero response is given within an array (i.e. The task does not immediately stop to
# avoid revealing the contingency), then the first step is to impute all pertinent zeros,
# and to re-code any values which exceed the maximum allowed value (defined by user).
#################################################################################################
##### ----- CHANGES NA values to 0 if the last non-missing value was 0:

for (id_num in purchase.task.df$id){
  if (purchase.task.df[purchase.task.df[,"id"]==id_num,][max(which(!is.na(purchase.task.df[purchase.task.df[,"id"]==id_num,])))] == 0){
    purchase.task.df[purchase.task.df[,"id"]==id_num,][is.na(purchase.task.df[purchase.task.df[,"id"]==id_num,])] <- 0
  }
}

###############################################################################################
### ----- CHANGES values which exceed the maximum value to the maximum allowed value (set by user):

purchase.task.df[,2:ncol(purchase.task.df)][purchase.task.df[,2:ncol(purchase.task.df)] > max.val] <- max.val

#################################################################################################
##### STEP 2: REVIEW MISSING DATA
#################################################################################################
# Missing data are reviewed next. Individuals who contradict themselves at the last item of
# the array are considered missing. A valid imputation is not possible when the last non-missing
# value is non-zero, because it is not clear whether all subsequent responses would be zeros.  
#################################################################################################
###### ----- IDENTIFIES IDs with NA values:

missing.id <- {}
for (id_num in purchase.task.df$id){
  if (sum(is.na(purchase.task.df[purchase.task.df[,"id"]==id_num,])) > 0){
    missing.id <- append(missing.id, id_num)
  }
}

print(missing.id)

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################

# REMOVES the IDs with missing data
purchase.task.df2 <- purchase.task.df[!purchase.task.df[,"id"] %in% missing.id,]

#################################################################################################
##### STEP 3: VIOLATION OF TREND, BOUNCE RATIO CRITERION, AND REVERSAL ALLOWANCE
#################################################################################################
# Data quality/attention/effort is reviewed next, excluding individuals who:
# i) do not exhibit a decelerating trend (trend violation - however 0 demand is acceptable)
# ii) exhibit a bounce ratio of 10% (or other 'bounce.crit' value chosen by user)
# iii)  exhibit 2 or more reversals (reversal = 2 or more consecutive 0s prior to a positive value)
#################################################################################################

##### ----- i) CHECKS for trend violation:
#################################################################################################
# IDENTIFIES and REMOVES IDs with a trend violation

remove.id.trend = {}
for (id_num in purchase.task.df2$id){
  if ( (purchase.task.df2[purchase.task.df2$id == id_num,prices[1]]>0) & 
       (purchase.task.df2[purchase.task.df2$id == id_num,prices[1]] <= purchase.task.df2[purchase.task.df2$id == id_num,prices[length(prices)]]) ){
    purchase.task.df2 <- purchase.task.df2[!purchase.task.df2[,"id"] %in% c(id_num),]
    remove.id.trend <- append(remove.id.trend,id_num)
  }
}

print(remove.id.trend)

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################

##### ----- ii) CALCULATES Bounce Ratio:
#################################################################################################
# IDENTIFIES and REMOVES IDs violating the bounce criterion

remove.id.bounce <- {}
for (id_num in purchase.task.df2$id){
  num.bounces <- 0
  for (j in seq(1,length(prices)-1,1)){
    if (purchase.task.df2[purchase.task.df2$id == id_num,prices[j]] < purchase.task.df2[purchase.task.df2$id == id_num,prices[j+1]]){
      num.bounces <- num.bounces + 1
    }
  }
  if (num.bounces/(length(prices)-1) > 0.1){
    purchase.task.df2 <- purchase.task.df2[!purchase.task.df2[,"id"] %in% c(id_num),]
    remove.id.bounce <- append(remove.id.bounce,id_num)
    cat("ID",id_num,"has bounce ratio:", num.bounces/(length(prices)-1),"and is being removed.\n")
  }
}

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################

# RESHAPE data from wide to long to CHECK for reversals
# The {beezdemand} package requires column names to = "id", "x", "y"

PT.long <- reshape(as.data.frame(purchase.task.df2), idvar = "id", 
                   varying = prices,
                   v.names = "y", timevar = "x", sep = "", direction = "long")

# Reordering the long data by id
PT.long <- PT.long[order(PT.long$id),]
# Reassigning x values in the long format using 'prices' object
PT.long$x <- prices

##### ----- iii) CHECKS FOR REVERSALS:
#################################################################################################
# IDENTIFIES and REMOVES IDs with 2 or more reversals

# 'ncons0' Is the number of consecutive 0s prior to a positive value that is used to flag a reversal

check.unsys <- CheckUnsystematic(dat = PT.long, deltaq = -0.01, bounce = bounce.crit, 
                                 reversals = 1.5, ncons0 = 2)

# IDENTIFIES IDs with 2 or more reversals
check.unsys[check.unsys$ReversalsPass=="Fail",]

# LISTS & REMOVES the IDs of those who failed
fail.list <- check.unsys$ReversalsPass=="Fail"
good.id.list <- check.unsys$id[!fail.list]

PT.long2 <- PT.long[!is.na(match(PT.long$id,good.id.list)),]

#################################################################################################
##### STEP 4: OUTLIER MANAGEMENT AT THE PRICE LEVEL
#################################################################################################
# Outlier management is next, starting at the price level. There are 3 winsorizing types,
# which are chosen by the user. One iteration of winsorizing is implemented at this level.
# Price-level outliers are identified in the `Appendix.csv` file.
#################################################################################################

# RESHAPE the data back from long to wide format to winsorize the data
PT.wide <- reshape(as.data.frame(PT.long2), idvar = "id", v.names = "y", timevar = "x", direction = "wide")

colnames(PT.wide) <- item.names

# CREATE z-scores in a separate data frame
wide.zs <- scale(PT.wide, center = TRUE, scale = TRUE)

# CREATE a new data frame for the winsorized data, so original values can later be referred to
PT.wide2 <- PT.wide

##### ----------  WINSORIZING TYPE - OPTION 1:
#################################################################################################
# 1: Values with a SD over 3.99 are replaced with their corresponding 3.99 regular value rounded up

if (wins.type=="1_higher_sd"){
  for (price in prices){
    PT.wide2[wide.zs[,price]> 3.99,price] <- ceiling(3.99*sd(PT.wide2[,price])+
                                                       mean(PT.wide2[,price]))
    PT.wide2[wide.zs[,price]< -3.99,price] <- floor(-3.99*sd(PT.wide2[,price])+
                                                      mean(PT.wide2[,price]))
    print(price)
  }
}

##### ----------  WINSORIZING TYPE - OPTION 2:
#################################################################################################
# 2: All outliers are replaced with 1 higher than highest (or 1 lower than the lowest) non-outlying value

if (wins.type=="1_higher_max_non_outlier"){
  for (price in prices){
    PT.wide2[wide.zs[,price]> 3.99,price] <- max(PT.wide2[wide.zs[,price]< 3.99,price]) + 1
    PT.wide2[wide.zs[,price]< -3.99,price] <- min(PT.wide2[wide.zs[,price]> -3.99,price]) - 1
  }
}

##### ----------  WINSORIZING TYPE - OPTION 3:
#################################################################################################
# 3: Order is maintained by replacing outlying values with 1 unit above the next highest non-outlying value

if (wins.type=="preserve_order"){
  for (price in prices){
    above.399 <- unique(wide.zs[wide.zs[,price]> 3.99,price])
    below.neg399 <- unique(wide.zs[wide.zs[,price]< -3.99,price])
    if (length(above.399)>0){
      for (q in seq(1, length(above.399), by=1)){
        if (length(above.399)>1){
          quantity.zs <- above.399[order(above.399)][q]
        } else if (length(above.399)==1) {
          quantity.zs <- above.399[q]
        }
        PT.wide2[wide.zs[,price]==quantity.zs,price] <- max(PT.wide2[wide.zs[,price]< 3.99,price]) + q
      }
    }
    if (length(below.neg399)>0){
      for (q in seq(1, length(below.neg399), by=1)){
        if (length(below.neg399)>1){
          quantity.zs <- below.neg399[rev(order(below.neg399))][q]
        } else if (length(below.neg399)==1) {
          quantity.zs <- below.neg399[q]
        }
        PT.wide2[wide.zs[,price]==quantity.zs,price] <- min(PT.wide2[wide.zs[,price]> -3.99,price]) - q
      }
    }
  }
}

# IDENTIFY which items have been changed for which IDs via winsorization
df.winsor.track <- data.frame(ID=integer(),
                              Price=numeric(),
                              Bef_Winsor=integer(), 
                              After_Winsor=integer())
i = 1
for (id_num in PT.wide$id){
  for (price in prices){
    orig = PT.wide[PT.wide$id == id_num,price]
    new = PT.wide2[PT.wide2$id == id_num,price]
    if (orig != new){
      df.winsor.track[i,1] <- id_num
      df.winsor.track[i,2] <- price
      df.winsor.track[i,3] <- orig
      df.winsor.track[i,4] <- new
      i = i + 1
    }
  }
}

##### ----- WINSORIZED
#################################################################################################
##### RESHAPE winsorized data from wide to long format
# ! # "W" in dataframe stands for winsorized data

PT.W.long <- reshape(as.data.frame(PT.wide2), idvar = "id", 
                     varying = prices,
                     v.names = c("y"), timevar = c("x"), sep = "", direction = "long")

# Reordering PT long data by id
PT.W.long2 <- PT.W.long[order(PT.W.long$id),]
# Reassigning x values in the long format using the 'prices' object
PT.W.long2$x <- prices

##### ----- NON-WINSORIZED
#################################################################################################
#  USE PREVIOUS PT.long2 data

PT.nonW.long2 <- PT.long2

#################################################################################################
##### STEP 5: ELASTICITY (ALPHA) MODELLING TESTS
#################################################################################################
# Elasticity modelling (curve fitting) tests k values in the exponentiated equation and uses
# the parameter that yields the best fit. To calculate elasticity, individuals with multiple
# breakpoints are reassigned to the first breakpoint reached.
#################################################################################################

##### ----- WINSORIZED
#################################################################################################
PT.emp <- GetEmpirical(dat = PT.W.long2)
colnames(PT.emp) <- c("id","Intensity","BP0","BP1","Omax","Pmax")

# DETERMINE which k-value is best for curve fitting by testing a series of values
R2.val.k <- {}

# The k-values tested are in the 'k.span' object, input by the user (default is values 2, 3, and 4)
for (k_value in k.span){
  mean.curve <- FitCurves(dat = PT.long2, equation = "koff", 
                          k = k_value, agg='Mean')
  R2.val.k <- append(R2.val.k, mean.curve$R2)
}

# CHOOSE k-value based on which R^2 is highest for the mean data
# ! # Ties are broken by choosing the lower k-value
k.value.final <- min(k.span[R2.val.k == max(R2.val.k)])

mean.curve <- FitCurves(dat = PT.W.long2, equation = "koff", 
                        k = k.value.final, agg='Mean')

mean.curve$id <- c('mean.curve')
mean.curve.final <- mean.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(mean.curve.final) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")
print(mean.curve.final)

part.curve <- FitCurves(dat = PT.W.long2, equation = "koff", 
                        k = k.value.final, agg=NULL)

spec.curve <- part.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(spec.curve) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

all.out <- merge(PT.emp,spec.curve)
all.out$id <- as.integer(all.out$id)
all.out <- all.out[order(all.out$id),]
all.out$id <- as.character(all.out$id)

PT.final.results <- bind_rows(mean.curve.final,all.out)

# CREATE proper breakpoint variable
PT.final.results$Breakpoint <- PT.final.results$BP0
for (id_num in PT.wide2$id){
  pt.sum <- sum(PT.wide2[PT.wide2$id==id_num,prices], na.rm = FALSE)
  last.amount <- PT.wide2[PT.wide2$id==id_num,length(prices)+1]
  if(is.na(PT.final.results$BP0[PT.final.results$id==id_num]) & (pt.sum==0)){
    PT.final.results$Breakpoint[PT.final.results$id==id_num] <- 0
  } else if (is.na(PT.final.results$BP0[PT.final.results$id==id_num]) & (last.amount>0)){
    PT.final.results$Breakpoint[PT.final.results$id==id_num] <- as.numeric(prices)[length(prices)]+1
  }
}

# REDEFINE breakpoints to the 1st 0 consumption price point reached in instances of reversals
check.unsys.2 <- CheckUnsystematic(dat = PT.long, deltaq = -0.01, bounce = 0.1, reversals = .01, ncons0 = 1)
one.rev.list <- check.unsys.2[check.unsys.2$ReversalsPass=="Fail",]$id
one.rev.list <- one.rev.list[one.rev.list %in% PT.wide2$id]
for (id_num in one.rev.list){
  str(PT.final.results[PT.final.results$id==id_num,]$Breakpoint)
  cons.vals <- PT.wide2[PT.wide2$id==id_num,]
  for (price in prices){
    if (cons.vals[,price]==0){
      cat('The breakpoint for ID',id_num,'has been changed from',
          PT.final.results[PT.final.results$id==id_num,]$Breakpoint,'to',as.numeric(price))
      PT.final.results[PT.final.results$id==id_num,]$Breakpoint <- as.numeric(price)
      break
    }
  }
}

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################

PT.results <- merge(PT.wide2, PT.final.results)
item.names <- c("id",prices,"Q0d", "K", "Alpha", "R2", "EV", "Omax_curve",
                "Pmax_curve","Intensity", "BP0", "BP1", "Omax", "Pmax", "Breakpoint")
colnames(PT.results) <- item.names

##### ----- NON-WINSORIZED
#################################################################################################
PT.nonW.emp <- GetEmpirical(dat = PT.nonW.long2)
colnames(PT.nonW.emp) <- c("id","Intensity","BP0","BP1","Omax","Pmax")

# DETERMINE which k-value is best for curve fitting by testing a series of values
nonW.R2.val.k <- {}

# The k-values tested are in the 'k.span' object, input by the user (default is values 2, 3, and 4)
for (k_value in k.span){
  nonW.mean.curve <- FitCurves(dat = PT.nonW.long2, equation = "koff", 
                          k = k_value, agg='Mean')
  nonW.R2.val.k <- append(nonW.R2.val.k, nonW.mean.curve$R2)
}

# CHOOSE k-value based on which R^2 is highest for the mean data
# ! # Ties are broken by choosing the lower k-value
nonW.k.value.final <- min(k.span[nonW.R2.val.k == max(nonW.R2.val.k)])

nonW.mean.curve <- FitCurves(dat = PT.nonW.long2, equation = "koff", 
                        k = nonW.k.value.final, agg='Mean')

nonW.mean.curve$id <- c('nonW.mean.curve')
nonW.mean.curve.final <- nonW.mean.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(nonW.mean.curve.final) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")
print(nonW.mean.curve.final)

nonW.part.curve <- FitCurves(dat = PT.nonW.long2, equation = "koff", 
                        k = nonW.k.value.final, agg=NULL)

nonW.spec.curve <- nonW.part.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(nonW.spec.curve) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

nonW.all.out <- merge(PT.nonW.emp,nonW.spec.curve)
nonW.all.out$id <- as.integer(nonW.all.out$id)
nonW.all.out <- nonW.all.out[order(nonW.all.out$id),]
nonW.all.out$id <- as.character(nonW.all.out$id)

PT.nonW.final.results <- bind_rows(nonW.mean.curve.final,nonW.all.out)

# CREATE proper breakpoint variable
# PT.wide = non-winsorized
PT.nonW.final.results$Breakpoint <- PT.nonW.final.results$BP0
for (id_num in PT.wide$id){
  pt.nonW.sum <- sum(PT.wide[PT.wide$id==id_num,prices], na.rm = FALSE)
  nonW.last.amount <- PT.wide[PT.wide$id==id_num,length(prices)+1]
  if(is.na(PT.nonW.final.results$BP0[PT.nonW.final.results$id==id_num]) & (pt.nonW.sum==0)){
    PT.nonW.final.results$Breakpoint[PT.nonW.final.results$id==id_num] <- 0
  } else if (is.na(PT.nonW.final.results$BP0[PT.nonW.final.results$id==id_num]) & (nonW.last.amount>0)){
    PT.nonW.final.results$Breakpoint[PT.nonW.final.results$id==id_num] <- as.numeric(prices)[length(prices)]+1
  }
}

# REDEFINE breakpoints to the 1st 0 consumption price point reached in instances of reversals
nonW.check.unsys.2 <- CheckUnsystematic(dat = PT.long, deltaq = -0.01, bounce = 0.1, reversals = .01, ncons0 = 1)
nonW.one.rev.list <- nonW.check.unsys.2[nonW.check.unsys.2$ReversalsPass=="Fail",]$id
nonW.one.rev.list <- nonW.one.rev.list[nonW.one.rev.list %in% PT.wide$id]
for (id_num in nonW.one.rev.list){
  str(PT.nonW.final.results[PT.nonW.final.results$id==id_num,]$Breakpoint)
  nonW.cons.vals <- PT.wide[PT.wide$id==id_num,]
  for (price in prices){
    if (nonW.cons.vals[,price]==0){
      cat('The breakpoint for ID',id_num,'has been changed from',
          PT.nonW.final.results[PT.nonW.final.results$id==id_num,]$Breakpoint,'to',as.numeric(price))
      PT.nonW.final.results[PT.nonW.final.results$id==id_num,]$Breakpoint <- as.numeric(price)
      break
    }
  }
}

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################

PT.nonW.results <- merge(PT.wide, PT.nonW.final.results)
item.names <- c("id",prices,"Q0d", "K", "Alpha", "R2", "EV", "Omax_curve",
                "Pmax_curve","Intensity", "BP0", "BP1", "Omax", "Pmax", "Breakpoint")
colnames(PT.nonW.results) <- item.names

#################################################################################################
##### STEP 6: WINSORIZING INDEX VARIABLES
#################################################################################################
# Index-level winsorizing re-codes outlying values as .001 (delta value) greater than the next
# highest non-outlying value to retain order (winsorization type 3). By using the delta value
# as spacing, the order of winsorization is maintained. Alpha (Elasticity) requires that the
# first two prices have non-zero values in order to calculate the demand curve. Individuals with
# zeros in either of their first two responses are identified and removed from the curve analysis.
#################################################################################################

# CREATES a FUNCTION for winsorizing index variables
winsorize.index <- function(all_out_temp,var_name,delta) {
  all_out <- all_out_temp[!is.na(all_out_temp[,c(var_name)]),]
  alpha_zs <- scale(all_out[,c(var_name)], center = TRUE, scale = TRUE)
  above_399 <- unique(all_out[,c(var_name)][alpha_zs > 3.99])
  below_neg399 <- unique(all_out[,c(var_name)][alpha_zs < -3.99])
  cat('There is/are',length(c(all_out[,c(var_name)][alpha_zs > 3.99],all_out[,c(var_name)][alpha_zs < -3.99])),
      'outlying ',var_name,' value(s): \n')
  alpha_outliers <- append(above_399, below_neg399)
  if (wins.type=="preserve_order"){
    above_399 <- unique(all_out[,c(var_name)][alpha_zs > 3.99])
    below_neg399 <- unique(all_out[,c(var_name)][alpha_zs < -3.99])
    if (length(above_399)>0){
      q <- 1
      for (ab_399 in sort(above_399)){
        cat('For ID(s) ',as.numeric(all_out[all_out[,c(var_name)] == ab_399,c('id')]),'\n the ',var_name,' value was changed from ',
            ab_399,' to ',max(all_out[,c(var_name)][alpha_zs < 3.99]) + q*delta, '\n')
        all_out[,c(var_name)][all_out[,c(var_name)] == ab_399] <- max(all_out[,c(var_name)][alpha_zs < 3.99]) + q*delta
        q <- q + 1
      }
    }
    if (length(below_neg399)>0){
      for (bel_399 in sort(below_neg399,decreasing = TRUE)){
        q <- 1
        cat('For ID(s) ',all_out[all_out[,c(var_name)] == bel_399,c('id')],'\n the ',var_name,' value was changed from',
            bel_399,' to ',min(all_out[,c(var_name)][alpha_zs > -3.99]) - q*delta, '\n')
        all_out[,c(var_name)][all_out[,c(var_name)] == bel_399] <- min(all_out[,c(var_name)][alpha_zs > -3.99]) - q*delta
        q <- q + 1
      }
    }
  }
  for_replace <- all_out[,c(var_name)]
  all_out_temp[,c(var_name)] <- replace(all_out_temp[,c(var_name)], !is.na(all_out_temp[,c(var_name)]), for_replace)
  all_out_temp
}

##### ----- CALCULATING Elasticity requires the first 2 numbers to be non-zero:
non_zero <- (PT.results[,2]==0)|(PT.results[,3]==0)

# IDENTIFY IDs who had a 0 value in one or both of their first 2 responses
cat('Total number of IDs with a zero value in first 2 responses: ',length(PT.results[(non_zero),]$id),'\n ID(s): ',PT.results[non_zero,]$id,sep=' ')

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################


##### ----- WINSORIZED
#################################################################################################

PT.W.index <- PT.results

### REMOVES IDs with ZEROS in first 2 responses:
zero.id.W <- PT.W.index[non_zero,]$id

if(length(zero.id.W>0)){
PT.W.index[(PT.W.index$id %in% zero.id.W),][,c('Q0d','Alpha','R2','EV','Omax','Pmax')] <- NA
}

# TO PRESERVE order, delta needs to not equal 0 (default delta value of 0.001)
#################################################################################################
delta <- 0.001

PT.W.index <- winsorize.index(PT.W.index,'Alpha', delta)
PT.W.index <- winsorize.index(PT.W.index,'Breakpoint', delta)
PT.W.index <- winsorize.index(PT.W.index, 'Intensity', delta)
PT.W.index <- winsorize.index(PT.W.index,'Omax', delta)
PT.W.index <- winsorize.index(PT.W.index,'Pmax', delta)

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################


##### ----- NON-WINSORIZED
#################################################################################################

PT.nonW.index <- PT.nonW.results

### REMOVE IDs with ZEROS in first 2 responses:
zero.id.nonW <- PT.nonW.index[non_zero,]$id

if(length(zero.id.nonW>0)){
PT.nonW.index[(PT.nonW.index$id %in% zero.id.nonW),][,c('Q0d','Alpha','R2','EV','Omax','Pmax')] <- NA
}

################  ----------  OUTPUT FOR PURCHASE TASK REPORTS  ----------  #####################
#################################################################################################
#################################################################################################

cat(nrow(PT.W.index),'/',tot.n,'=',100*nrow(PT.W.index)/tot.n,'% left after removal\n',sep='')
cat(length(prices),' different price values, ',nrow(PT.W.index),' individuals = ',
    nrow(PT.W.index)*length(prices),' data points\n',sep='')
cat('Outliers:\n',nrow(df.winsor.track),' outlying values (',
    100*nrow(df.winsor.track)/(nrow(PT.W.index)*length(prices)),'% of total values)\n',sep='')
cat('K values tested:',k.span,'\n')
cat('K value selected: ',min(PT.W.index$K),'\n',sep='')
cat('R^2 for mean values: ',PT.final.results[PT.final.results$id=='mean.curve',]$R2,'\n',sep='')
cat('Median R^2: ',median(PT.W.index$R2,na.rm=TRUE),
    ' (Interquartile range: ', IQR(PT.W.index$R2,na.rm=TRUE),'; min = ',min(PT.W.index$R2,na.rm=TRUE),
    ', max = ',max(PT.W.index$R2,na.rm=TRUE),')\n',sep='')

##### ^^^ AFTER RUNNING THIS CODE, CHECK CONSOLE
#################################################################################################


################  ----------  SAVING PROCESSED DATA AND OUTPUT  ----------  #####################
#################################################################################################
#################################################################################################

##### ----- DATA OUTPUT WITH ORIGINAL N individuals:
# This merges the output with N = 'tot.n' so that any individuals that were removed for the
# purchase task have NAs in the purchase task output

PT.W.index.final <- merge(purchase.task.df[c("id")],
  PT.W.index[c("id","Alpha","Breakpoint","Intensity","Omax","Pmax")], by = "id", all.x = TRUE)

winso.names <- c("id","Alpha_W","Breakpoint_W","Intensity_W","Omax_W","Pmax_W")
colnames(PT.W.index.final) <- winso.names

PT.nonW <- PT.nonW.index[c("id","Alpha","Breakpoint","Intensity","Omax","Pmax")]

PT.ALL.DATA <- merge(PT.nonW,PT.W.index.final, by = "id", all.y = TRUE)

##### ----- INDEX LEVEL VARIABLES DESCRIPTIVE STATISTICS:

PT.describe <- psych::describe(PT.ALL.DATA[c("Alpha","Intensity", "Omax", "Pmax", "Breakpoint",
                            "Alpha_W", "Intensity_W", "Omax_W", "Pmax_W", "Breakpoint_W")])
PT.describe$vars <- c("Alpha", "Intensity", "Omax", "Pmax", "Breakpoint",
                      "Alpha_W", "Intensity_W", "Omax_W", "Pmax_W", "Breakpoint_W")

PT.describe <- PT.describe[c("vars","n","mean","sd","se","min","max")]

##### ----- PRICE LEVEL VARIABLES (PRICES) DESCRIPTIVE STATISTICS:

price.stats.W <- psych::describe(PT.wide2[c(prices)])
price.stats.W$vars <- purchase.task.names
price.stats.W$vars <- paste0(price.stats.W$vars,"_W")
price.stats.W <- price.stats.W[c("vars","n","mean","sd","se","min","max")]

price.stats.nonW <- psych::describe(PT.wide[c(prices)])
price.stats.nonW$vars <- purchase.task.names
price.stats.nonW <- price.stats.nonW[c("vars","n","mean","sd","se","min","max")]

price.stats <- rbind(price.stats.W,price.stats.nonW)

##### ----- WRITE ALL TO CREATE A REPORT
#################################################################################################
# These files will be located in the working directory (chosen by the user)

write.csv(PT.ALL.DATA, "purchase.task.csv", row.names = FALSE) ### PT DATA (WINSORIZED & NON-WINSORIZED)
write.csv(PT.describe,"PT.variables.csv", row.names = FALSE) ### PT VARIABLES (WINSORIZED & NON-WINSORIZED)
write.csv(price.stats, "price.level.variables.csv", row.names = FALSE) ### (WINSORIZED & NON-WINSORIZED)
write.csv(df.winsor.track, "Appendix.csv", row.names = FALSE)  ### Outlier changes by ID (WINSORIZED)
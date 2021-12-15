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
library(ggplot2)

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
wide.zs <- PT.wide
wide.zs[c(prices)] <- scale(PT.wide[c(prices)], center = TRUE, scale = TRUE)

# CREATE a new data frame for the winsorized data, so original values can later be referred to
PT.wide2 <- PT.wide

##### MODIFIED Price List (if final price array not reached)
price.count <- colSums(PT.wide[prices])
price_df <- data.frame(price.count,prices)
mod.prices <- price_df$prices[price_df$price.count!=0]

##### ----------  WINSORIZING TYPE - OPTION 1:
#################################################################################################
# 1: Values with a z-score over 3.99 are replaced with their corresponding 3.99 regular value rounded up

if (wins.type=="1_higher_sd"){
  for (price in mod.prices){
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
  for (price in mod.prices){
    PT.wide2[wide.zs[,price]> 3.99,price] <- max(PT.wide2[wide.zs[,price]< 3.99,price]) + 1
    PT.wide2[wide.zs[,price]< -3.99,price] <- min(PT.wide2[wide.zs[,price]> -3.99,price]) - 1
  }
}

##### ----------  WINSORIZING TYPE - OPTION 3:
#################################################################################################
# 3: Order is maintained by replacing outlying values with 1 unit above the next highest non-outlying value

if (wins.type=="preserve_order"){
  for (price in mod.prices){
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
df.winsor.track <- data.frame(ID=character(),
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
                        k = k.value.final, agg='Mean', detailed = T)

mean.curve.final <- mean.curve[["dfres"]]
mean.curve.final$id <- c('mean.curve')
mean.curve.final <- mean.curve.final[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(mean.curve.final) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

##### ----- Plot Mean Curve:

### IF the minimum price == 0, then two plots are used by beezdemand,
### resulting in two sets of geom_text data. Thus, if minimum price == 0,
### then set alpha to 0 for the first plot

if(as.numeric(min(mod.prices))==0)
  (
    alpha_val <- c(0,1)
  )

if(as.numeric(min(mod.prices))!=0)
  (
    alpha_val <- c(0)
  )


PlotCurve(mean.curve$adfs[[1]],
          mean.curve$dfres[1,],
          mean.curve$newdats[[1]]) +
  ggtitle(paste0("Mean Curve")) + theme_classic() +
  geom_text(data = mean.curve.final,
            mapping = aes(label = paste0("Elasticity: ", round(Alpha, digits = 4),
                                         "\n Q0: ", round(Q0d, digits = 2),
                                         "\n Pmax: ", round(Pmax_curve, digits = 2),
                                         "\n Omax: ", round(Omax_curve, digits = 2)),
                          x = Inf, y = Inf, hjust = 1, vjust = 1), alpha = alpha_val,
            size = 5, fontface = "bold", show.legend = F) +
  theme(title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(), strip.text = element_blank())

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

part.curve <- FitCurves(dat = PT.W.long2, equation = "koff",
                        k = k.value.final, agg=NULL)

spec.curve <- part.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(spec.curve) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

all.out <- merge(PT.emp,spec.curve)
all.out <- all.out[order(all.out$id),]

PT.final.results <- bind_rows(mean.curve.final,all.out)

# CREATE proper breakpoint variable
PT.final.results$Breakpoint <- PT.final.results$BP0
for (id_num in PT.wide2$id){
  pt.sum <- sum(PT.wide2[PT.wide2$id==id_num,prices], na.rm = FALSE)
  last.amount <- PT.wide2[PT.wide2$id==id_num,length(prices)+1]
  if(is.na(PT.final.results$BP0[PT.final.results$id==id_num]) & (pt.sum==0)){
    PT.final.results$Breakpoint[PT.final.results$id==id_num] <- as.numeric(min(mod.prices))
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
                             k = nonW.k.value.final, agg='Mean', detailed = T)

nonW.mean.curve.final <- nonW.mean.curve[["dfres"]]
nonW.mean.curve.final$id <- c('mean.curve')
nonW.mean.curve.final <- nonW.mean.curve.final[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(nonW.mean.curve.final) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

##### ----- Plot Mean Curve:
PlotCurve(nonW.mean.curve$adfs[[1]],
          nonW.mean.curve$dfres[1,],
          nonW.mean.curve$newdats[[1]]) +
  ggtitle(paste0("Mean Curve")) + theme_classic() +
  geom_text(data = mean.curve.final,
            mapping = aes(label = paste0("Elasticity: ", round(Alpha, digits = 4),
                                         "\n Q0: ", round(Q0d, digits = 2),
                                         "\n Pmax: ", round(Pmax_curve, digits = 2),
                                         "\n Omax: ", round(Omax_curve, digits = 2)),
                          x = Inf, y = Inf, hjust = 1, vjust = 1), alpha = c(0,1),
            size = 5, fontface = "bold", show.legend = F) +
  theme(title = element_text(size = 25, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(), strip.text = element_blank())

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

nonW.part.curve <- FitCurves(dat = PT.nonW.long2, equation = "koff",
                             k = nonW.k.value.final, agg=NULL)

nonW.spec.curve <- nonW.part.curve[,c("id","Q0d","K","Alpha","R2","EV","Omaxd","Pmaxd")]
colnames(nonW.spec.curve) <- c("id","Q0d","K","Alpha","R2","EV","Omax_curve","Pmax_curve")

nonW.all.out <- merge(PT.nonW.emp,nonW.spec.curve)
nonW.all.out <- nonW.all.out[order(nonW.all.out$id),]

PT.nonW.final.results <- bind_rows(nonW.mean.curve.final,nonW.all.out)

# CREATE proper breakpoint variable
# PT.wide = non-winsorized
PT.nonW.final.results$Breakpoint <- PT.nonW.final.results$BP0
for (id_num in PT.wide$id){
  pt.nonW.sum <- sum(PT.wide[PT.wide$id==id_num,prices], na.rm = FALSE)
  nonW.last.amount <- PT.wide[PT.wide$id==id_num,length(prices)+1]
  if(is.na(PT.nonW.final.results$BP0[PT.nonW.final.results$id==id_num]) & (pt.nonW.sum==0)){
    PT.nonW.final.results$Breakpoint[PT.nonW.final.results$id==id_num] <- as.numeric(min(mod.prices))
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
        cat('For ID(s) ',all_out[all_out[,c(var_name)] == ab_399,c('id')],'\n the ',var_name,' value was changed from ',
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

##### ----- PRICE LEVEL VARIABLES (PRICES) DESCRIPTIVE STATISTICS:

price.stats.W <- psych::describe(PT.wide2[c(prices)])
price.stats.W$vars <- purchase.task.names
price.stats.W$vars <- paste0(price.stats.W$vars,"_W")
price.stats.W <- price.stats.W[c("vars","n","mean","sd","se","min","max")]

price.stats.nonW <- psych::describe(PT.wide[c(prices)])
price.stats.nonW$vars <- purchase.task.names
price.stats.nonW <- price.stats.nonW[c("vars","n","mean","sd","se","min","max")]

price.stats <- rbind(price.stats.W,price.stats.nonW)

##### ----- DATA TRANSFORMATIONS
#################################################################################################
# Determine best transformation of (winsorized) variables and save to data set
# since a value of 0 is possible for Breakpoint, Intensity, Omax, and Pmax,
# a small constant (0.1) is added prior to log10 transformation

se <- function(x) sqrt(var(x,na.rm = T)/length(x))

PT.TRFMED <- PT.ALL.DATA

PT.LOG.TRFMED <- log10(PT.ALL.DATA[c("Alpha_W","Breakpoint_W","Intensity_W","Omax_W","Pmax_W")]+0.1)
PT.LOG.TRFMED <- PT.LOG.TRFMED %>%
  rename(Alpha_Log = Alpha_W, Breakpoint_Log = Breakpoint_W,
         Intensity_Log = Intensity_W, Omax_Log = Omax_W, Pmax_Log = Pmax_W)

PT.SQRT.TRFMED <- sqrt(PT.ALL.DATA[c("Alpha_W","Breakpoint_W","Intensity_W","Omax_W","Pmax_W")])
PT.SQRT.TRFMED <- PT.SQRT.TRFMED %>%
  rename(Alpha_Sqrt = Alpha_W, Breakpoint_Sqrt = Breakpoint_W,
         Intensity_Sqrt = Intensity_W, Omax_Sqrt = Omax_W, Pmax_Sqrt = Pmax_W)

PT.TRFMED <- cbind(PT.TRFMED,PT.LOG.TRFMED,PT.SQRT.TRFMED)
PT.ALL.TRANSFORMED <- PT.TRFMED ### TO SAVE AS OUTPUT

PT.TRFMED <- PT.TRFMED %>%
  rename(Alpha_NonWinsorized = Alpha, Alpha_Winsorized = Alpha_W,
         Breakpoint_NonWinsorized = Breakpoint, Breakpoint_Winsorized = Breakpoint_W,
         Intensity_NonWinsorized = Intensity, Intensity_Winsorized = Intensity_W,
         Omax_NonWinsorized = Omax, Omax_Winsorized = Omax_W,
         Pmax_NonWinsorized = Pmax, Pmax_Winsorized = Pmax_W)

PT.TRFMED.LONG <- reshape(as.data.frame(PT.TRFMED), idvar = "id",
                          varying = c("Alpha_NonWinsorized", "Alpha_Winsorized", "Alpha_Log", "Alpha_Sqrt",
                                      "Breakpoint_NonWinsorized", "Breakpoint_Winsorized", "Breakpoint_Log", "Breakpoint_Sqrt",
                                      "Intensity_NonWinsorized", "Intensity_Winsorized", "Intensity_Log", "Intensity_Sqrt",
                                      "Omax_NonWinsorized", "Omax_Winsorized", "Omax_Log", "Omax_Sqrt",
                                      "Pmax_NonWinsorized", "Pmax_Winsorized", "Pmax_Log", "Pmax_Sqrt"),
                          timevar = "Transformation", sep = "_", direction = "long")

PT.TRFMED.LONG$Transformation <- factor(PT.TRFMED.LONG$Transformation,
                                        levels = c("NonWinsorized","Winsorized","Log","Sqrt"))

##### ----- Visualize Non-Winsorized, Winsorized, and Transformed (Log and Sqrt) Purchase Task Variables

trfmed.stats <- PT.TRFMED.LONG %>% group_by(Transformation) %>%
  summarize(Alpha_skew = skew(Alpha), Alpha_kur = kurtosi(Alpha), Alpha_se = se(Alpha),
            Alpha_zmin = min(scale(Alpha), na.rm = T), Alpha_zmax = max(scale(Alpha), na.rm = T),
            Breakpoint_skew = skew(Breakpoint), Breakpoint_kur = kurtosi(Breakpoint), Breakpoint_se = se(Breakpoint),
            Breakpoint_zmin = min(scale(Breakpoint), na.rm = T), Breakpoint_zmax = max(scale(Breakpoint), na.rm = T),
            Intensity_skew = skew(Intensity), Intensity_kur = kurtosi(Intensity), Intensity_se = se(Intensity),
            Intensity_zmin = min(scale(Intensity), na.rm = T), Intensity_zmax = max(scale(Intensity), na.rm = T),
            Omax_skew = skew(Omax), Omax_kur = kurtosi(Omax), Omax_se = se(Omax),
            Omax_zmin = min(scale(Omax), na.rm = T), Omax_zmax = max(scale(Omax), na.rm = T),
            Pmax_skew = skew(Pmax), Pmax_kur = kurtosi(Pmax), Pmax_se = se(Pmax),
            Pmax_zmin = min(scale(Pmax), na.rm = T), Pmax_zmax = max(scale(Pmax), na.rm = T))

trfmed.stats[c(2:26)] <- round(trfmed.stats[c(2:26)], digits = 2)

#################################################################################################
#################################################################################################
##### TRANSFORMATIONS: Chose Desired Transformation for each purchase task variable below
##### in the following section.

##### ----- ALPHA:

### NOTE: If Elasticity (Alpha) is left untransformed, multiplying by a constant
### is recommended as the values are quite small and can cause issues in model estimation.

ggplot(PT.TRFMED.LONG, aes (x = Alpha, fill = Transformation)) + geom_histogram(alpha = 0.5, show.legend = F) +
  ylab("Count") + theme_apa() + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank(), axis.title.x = element_text(size = 20, face = "bold")) +
  geom_text(trfmed.stats, mapping = aes(label = paste0("SE: ", Alpha_se, "\n Skew: ", Alpha_skew, "\n Kurtosis: ", Alpha_kur, "\n Z-Score Min: ", Alpha_zmin, "\n Z-Score Max: ", Alpha_zmax),
                                        group = Transformation), x = Inf, y = Inf, hjust = 1, vjust = 1) + facet_wrap(~Transformation, scales = "free")

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

##### ----- Breakpoint:

ggplot(PT.TRFMED.LONG, aes (x = Breakpoint, fill = Transformation)) + geom_histogram(alpha = 0.5, show.legend = F) +
  ylab("Count") + theme_apa() + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank(), axis.title.x = element_text(size = 20, face = "bold")) +
  geom_text(trfmed.stats, mapping = aes(label = paste0("SE: ", Breakpoint_se, "\n Skew: ", Breakpoint_skew, "\n Kurtosis: ", Breakpoint_kur, "\n Z-Score Min: ", Breakpoint_zmin, "\n Z-Score Max: ", Breakpoint_zmax),
                                        group = Transformation), x = Inf, y = Inf, hjust = 1, vjust = 1) + facet_wrap(~Transformation, scales = "free")

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

##### ----- Intensity:

ggplot(PT.TRFMED.LONG, aes (x = Intensity, fill = Transformation)) + geom_histogram(alpha = 0.5, show.legend = F) +
  ylab("Count") + theme_apa() + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank(), axis.title.x = element_text(size = 20, face = "bold")) +
  geom_text(trfmed.stats, mapping = aes(label = paste0("SE: ", Intensity_se, "\n Skew: ", Intensity_skew, "\n Kurtosis: ", Intensity_kur, "\n Z-Score Min: ", Intensity_zmin, "\n Z-Score Max: ", Intensity_zmax),
                                        group = Transformation), x = Inf, y = Inf, hjust = 1, vjust = 1) + facet_wrap(~Transformation, scales = "free")

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

##### ----- Omax:

ggplot(PT.TRFMED.LONG, aes (x = Omax, fill = Transformation)) + geom_histogram(alpha = 0.5, show.legend = F) +
  ylab("Count") + theme_apa() + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank(), axis.title.x = element_text(size = 20, face = "bold")) +
  geom_text(trfmed.stats, mapping = aes(label = paste0("SE: ", Omax_se, "\n Skew: ", Omax_skew, "\n Kurtosis: ", Omax_kur, "\n Z-Score Min: ", Omax_zmin, "\n Z-Score Max: ", Omax_zmax),
                                        group = Transformation), x = Inf, y = Inf, hjust = 1, vjust = 1) + facet_wrap(~Transformation, scales = "free")

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

##### ----- Pmax:

ggplot(PT.TRFMED.LONG, aes (x = Pmax, fill = Transformation)) + geom_histogram(alpha = 0.5, show.legend = F) +
  ylab("Count") + theme_apa() + theme(strip.text = element_text(size = 13, face = "bold"), strip.background = element_blank(), axis.title.x = element_text(size = 20, face = "bold")) +
  geom_text(trfmed.stats, mapping = aes(label = paste0("SE: ", Pmax_se, "\n Skew: ", Pmax_skew, "\n Kurtosis: ", Pmax_kur, "\n Z-Score Min: ", Pmax_zmin, "\n Z-Score Max: ", Pmax_zmax),
                                        group = Transformation), x = Inf, y = Inf, hjust = 1, vjust = 1) + facet_wrap(~Transformation, scales = "free")

##### ^^^ AFTER RUNNING THIS CODE, CHECK PLOTS PANE
#################################################################################################

##### TRANSFORMATIONS:

### BOTH Winsorized and Non-Winsorized values are retained, with log10 and square root
### transformations of the winsorized index-level variables included in both
### the saved data and the descriptive statistics

PT.describe <- psych::describe(PT.ALL.TRANSFORMED[c("Alpha","Alpha_W","Alpha_Log","Alpha_Sqrt",
                                                    "Breakpoint","Breakpoint_W","Breakpoint_Log","Breakpoint_Sqrt",
                                                    "Intensity","Intensity_W","Intensity_Log","Intensity_Sqrt",
                                                    "Omax","Omax_W","Omax_Log","Omax_Sqrt",
                                                    "Pmax","Pmax_W","Pmax_Log","Pmax_Sqrt")])

PT.describe$vars <- c("Alpha","Alpha_W","Alpha_Log","Alpha_Sqrt",
                      "Breakpoint","Breakpoint_W","Breakpoint_Log","Breakpoint_Sqrt",
                      "Intensity","Intensity_W","Intensity_Log","Intensity_Sqrt",
                      "Omax","Omax_W","Omax_Log","Omax_Sqrt",
                      "Pmax","Pmax_W","Pmax_Log","Pmax_Sqrt")

PT.describe <- PT.describe[c("vars","n","mean","sd","se","min","max")]

##### ----- WRITE ALL TO CREATE A REPORT
#################################################################################################
# These files will be located in the working directory (chosen by the user)

write.csv(PT.ALL.TRANSFORMED, "purchase.task.csv", row.names = FALSE) ### PT DATA (WINSORIZED & NON-WINSORIZED)
write.csv(PT.describe,"PT.variables.csv", row.names = FALSE) ### PT VARIABLES (WINSORIZED & NON-WINSORIZED)
write.csv(price.stats, "price.level.variables.csv", row.names = FALSE) ### (WINSORIZED & NON-WINSORIZED)
write.csv(df.winsor.track, "Appendix.csv", row.names = FALSE)  ### Outlier changes by ID (WINSORIZED)

setwd("~/Dropbox/BigDataCert/R Clustering/round 2")

library("tidyverse")
original <- read.csv("globalterrorismdb_0718dist.csv")

years <- c(2012:2017) 
# the range of years we want to look at

gtd <- filter(original, iyear %in% years)
# file the dataset gtd by the the value years in 'iyears'

gtd <- select(gtd,"nkill","nwound", "propvalue","country_txt","iyear")


kill_na <- sum(is.na(gtd$nkill))
# 3779 NA values


wound_na <- sum(is.na(gtd$nwound))
# 6399 NA values

prop_na <- sum(is.na(gtd$propvalue))
# 49327

# create the ratings
indexes = c()
for (i in 1:nrow(gtd)){
  counter = 1
  if (!is.na(gtd[i,][1])){ #nkills
    counter = counter + gtd[i,][1] * 3
  } 
  
  if (!is.na(gtd[i,][2])){ # nwounded
    counter = counter + gtd[i,][2] * 0.5
  } 
  if (!is.na(gtd[i,][3])){ # propvalue 
    value <- gtd[i,][3]
    if (value >= 0 & value < 1000000){ # condition 1
      counter = counter + 1
    }
    else if (value >= 1000000 & value < 1000000000){ # condition 2
      counter = counter + 2 
      
    }
    else{ # condition 3: otherwise
      counter = counter + 3
    }
    if (value < 0) { # change the negative values in propvalue to 0
      gtd[i,][3] <- 0
    }
  }
  indexes[i] <- counter
}

gtd$ratings <- as.numeric(unlist(indexes))
# add the indexes to gtd as a column called ratings


# We want to look at a summary of the data by country 
gtd_summary <- gtd %>% 
  group_by(country_txt)  %>% 
  summarise(total_rating = sum(ratings), total_propvalue = sum(propvalue, na.rm = TRUE), 
            total_killed = sum(nkill, na.rm = TRUE), total_wounded = sum(nwound, na.rm = TRUE))
# made a new dataframe grouped by the country and show the region_txt column.
# new columns were made given by column_name = column values grouped by the country_txt

write.csv(gtd_summary, "gtd_summary.csv", row.names = TRUE)

##########

#will start on world bank data and driscretize terroism and world bank columns at the same time

world_bank_original <- read.csv("worldbankdata.csv")

world_bank <- filter(world_bank_original, Time %in% years)
wanted_var <- c(1,3,5,6,7,8,9,10,11,13)
world_bank <- world_bank[,wanted_var]
names(world_bank) <- c("time","country_name","life_expectancy","urban_population_of_total", "urban_population_growth", 
                       "school_enrollment_secondary","school_enrollment_primary","school_enrollment_tertiary",
                       "military_expenditure", "GDP_growth_annual")
View(world_bank)


#replacing ".." with Blanks
world_bank[world_bank==".."] <- ""

#converts values from factor (i.e. string) to numeric /character
#world_bank[,"country_name"] <- as.character(world_bank$country_name)
world_bank[,"life_expectancy"] <- as.numeric(world_bank$life_expectancy)
world_bank[,'urban_population_of_total'] <- as.numeric(world_bank$urban_population_of_total)
world_bank[,'urban_population_growth'] <- as.numeric(world_bank$urban_population_growth)
world_bank[,'school_enrollment_secondary'] <- as.numeric(world_bank$school_enrollment_secondary)
world_bank[,'school_enrollment_primary'] <- as.numeric(world_bank$school_enrollment_primary)
world_bank[,'school_enrollment_tertiary'] <- as.numeric(world_bank$school_enrollment_tertiary)
world_bank[,'military_expenditure'] <- as.numeric(world_bank$military_expenditure)
world_bank[,'GDP_growth_annual'] <- as.numeric(world_bank$GDP_growth_annual)



gtd_wb <- merge(gtd, world_bank, by.x=c("country_txt","iyear"),by.y=c("country_name","time"), all.x = TRUE)
View(gtd_wb)

write.csv(gtd_wb,"merged_data.csv", row.names = TRUE)
gtd_wb <- read.csv("merged_data.csv")
# World Bank and Terrorism Data have been merged


other_set <- gtd_wb
View(other_set)

# next begin discretization
library(funModeling)
d_bins<-discretize_get_bins(data = gtd_wb, 
                            input = c("ratings"))

df_discretized<-discretize_df(data = gtd_wb,data_bins = d_bins,stringsAsFactors = T)

# change factor level names
new_levels <-c(1,2,3,4,5)



levels(df_discretized$ratings)[1] <- new_levels[1] # rename first factor index
levels(df_discretized$ratings)[2] <- new_levels[2] # rename second factor index
levels(df_discretized$ratings)[3] <- new_levels[3]
levels(df_discretized$ratings)[4] <- new_levels[4]
levels(df_discretized$ratings)[5] <- new_levels[5]

#change the factor type to numeric
df_discretized[,"ratings"] <- as.numeric(df_discretized$ratings)

# look for significant variables
# check with regression tree and linear model statistic

df_discretized <- read.csv("df_discretized.csv")

# look for significant variables
library(MASS)


df_reg <- lm(ratings ~.-X, data = df_discretized)
step = stepAIC(df_reg, direction = "both")

aci_fit <- lm(ratings ~ country_txt + iyear + nkill + nwound + propvalue + 
                urban_population_of_total + school_enrollment_secondary + 
                GDP_growth_annual, data = df_discretized)
Rsq_adj <- summary(aci_fit)$adj.r.squared
# we see the Rsq_adj gives us a score of 0.27
# Under the step AIC criteria, the our model was reduced to 8 variables
summary(aci_fit)
# we see the that school_enrollment_secondary and propvalue are not significant therefore we can omit them
# not within 95% significance level

sign_fit <- lm(ratings ~ country_txt + iyear + nkill + nwound + 
                 urban_population_of_total + GDP_growth_annual, data = df_discretized)

Rsq_adj <- summary(sign_fit)$adj.r.squared
# there is not significant change in the Rsq adj value therefore this simplier model is preferred

par(mfrow=c(2,1))
plot(sign_fit,1)
plot(sign_fit,2)


library(rpart)

aci_fit_tree <- rpart(ratings ~ country_txt + iyear + nkill + nwound + propvalue + 
                        urban_population_of_total + school_enrollment_secondary + 
                        GDP_growth_annual, data = df_discretized)

plot(aci_fit_tree)
text(aci_fit_tree)

sign_fit_tree <- rpart(ratings ~ country_txt + iyear + nkill + nwound + 
                         urban_population_of_total + GDP_growth_annual, data = df_discretized)
plot(reg_tree)
text(reg_tree)
# we see there are so significant changes


wanted_var <- c(2,3,4,5,7,9,15)
final_update <- df_discretized[,wanted_var]
final_update[,'country_txt'] <- as.character(final_update$country_txt)

write.csv(final_update, "final_update.csv", row.names = FALSE)

final_update <- read.csv("final_update.csv")
# identify that we hve clusters 

d1 = na.omit(final_update) # Clearing the NA values 

d <- dist(d1,method = "euclidean") #distance matrix
h_clust <- hclust(d, method = "ward") #clustering
plot(h_clust) #dendrogram
rect.hclust(h_clust, k=5, border="red") 



#fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
#            palette = "jco", ggtheme = theme_minimal())



library(fpc)
set.seed(1010)
sample_size = nrow(final_update) * 0.80
train_index <- sample(seq_len(nrow(final_update)), size = sample_size, replace = FALSE)

train <- final_update[train_index, ]
test <- final_update[-train_index, ]


hc <- hclust(dist(d1), method = "ave")
plot(hc, hang = -1, labels = d1$ratings[idx])
rect.hclust(hc, k = 3)



original_cluster <- read.csv("final_update.csv", header = T)


# Required libraries

library(cluster)
library(caTools)
library(rpart)
library(treeClust)
library(caret)


# Clustering is a broad set of techniques for finding subgroups of observations
# within a data set. When we cluster observations, We want observations in the 
# same group to be similar and observations in different groups to be dissimilar.
# Because there isn't a response variable, this is an unsupervised method.

# since we don't have any continuous varaibles in our data set, we subset the 
# data into a data frame containing only the two varaibles nkill and nwound. 
# Based on that we can create the model on clustering.

d1_original_cluster = na.omit(original_cluster) # Clearing the NA values 
d2_original_cluster = as.data.frame(d1_original_cluster[,5])  # the class variable 
table(d2_original_cluster)  # contains total 5 categories 
d1_original_cluster = d1_original_cluster[,c(3,4)]  # The two components 

# function to create the optimal value of the number of centres k

wssplot <- function(data, nc=7, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(d1_original_cluster, nc=7) # optimal value of k


set.seed(125) # random seed before the cluster method 
km = kmeans(x=d1_original_cluster, centers = 5)  # The kmeans model 
y_km = km$cluster  # The clusters from the k-means model
d1_original_cluster$new = y_km # adding the cluster into the data frame


# The cluster plot visualization 

clusplot(d1_original_cluster,y_km,lines = 0, color = TRUE,plotchar = FALSE,
         span = TRUE,main = paste('2D representation of the Cluster solution'),
         xlab = 'kill',
         ylab = 'wound')

table(y_km, d2_original_cluster$`d1_original_cluster[, 5]`) # The confusion matrix 

# "78.75% of variability" says that, with your data, almost half of the 
# information about the multivariate data is captured by this plot of components
# 1 and 2., i.e., nkill and nwound.

# pam model
gtd <- read.csv("final_update.csv")

library(cluster)

#split into test/train data
set.seed(1010)
sample_size = round(nrow(gtd) * 0.80)
train_index <- sample(seq_len(nrow(gtd)), size = sample_size, replace = FALSE)

train <- gtd[train_index, ]
test <- gtd[-train_index, ]



D=daisy(gtd, metric='gower')

#After the creation of a distance matrix, we implement a Wardâs hierarchical clustering procedure:

H.fit <- hclust(D, method="ward.D2")
plot(H.fit) # display dendrogram

groups <- cutree(H.fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the5 clusters
rect.hclust(H.fit, k=5, border="red")

clusplot(gtd, groups, color=TRUE, shade=TRUE,
         +          labels=2, lines=0, main= 'ratings')

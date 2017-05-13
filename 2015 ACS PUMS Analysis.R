setwd("~/Development/R_Census/csv_pus")
library(ggplot2)
library(data.table)
library(caret)

# GET DATA

cols <- c("RT", "ST", "PWGTP", "AGEP", "CIT", "COW", "ENG", "SCHL", "SEX", "WAGP", "ESR", "FOD1P", "HISP", "MSP", "NATIVITY","PINCP", "RAC1P", "SOCP","ADJINC", "PERNP", "SCIENGP", "SCIENGRLP")
#first data file, pusa
pusa <- fread(file="ss15pusa.csv", select = cols)
#second data file, pusb (starts at st=29)
pusb <- fread(file="ss15pusb.csv", select = cols)
pus <- rbind(pusa, pusb)

ls.str(pus)

#TRANSFORM VARIABLES
# Factor variables with help from: https://www.kaggle.com/huili0140/d/census/2013-american-community-survey/the-working-moms/notebook

pus$CIT <- factor(pus$CIT)
levels(pus$CIT)
levels(pus$CIT) <- c("Born in the U.S.", "Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas", "Born abroad of American parent(s)", ".U.S. citizen by naturalization", "Not a citizen of the U.S.")
summary(pus$CIT)
prop.table(table(pus$CIT)) * 100

pus$CITX <- ifelse(pus$CIT == "Not a citizen of the U.S.", 0, 1)
prop.table(table(pus$CITX)) * 100
pus$CITX <- factor(pus$CITX)
levels(pus$CITX)

pus$COW <- factor(pus$COW)
summary(pus$COW) # a lot of NAs to deal with later
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")
prop.table(table(pus$COW)) * 100

pus$ESR <- factor(pus$ESR)
levels(pus$ESR)
levels(pus$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Armed forces, at work", "Armed forces, with a job but not at work", "Not in labor force")
summary(pus$ESR) # will filter out NAs later
prop.table(table(pus$ESR)) * 100

#could simplify ESR this way
#pus$ESRX <- ifelse(pus$ESR == "Employed", 1, pus$ESR)
#pus$ESRX <- ifelse(pus$ESRX == "Unemployed", 2, pus$ESRX)
#pus$ESRX <- ifelse(pus$ESRX == "Not in labor force", 3, pus$ESRX)

pus$MSP <- factor(pus$MSP)
levels(pus$MSP) <- c("Married", "Married", "Widowed", "Divorced", "Separated", "Never married")
summary(pus$MSP)
prop.table(table(pus$MSP)) * 100

pus$MSPX <- ifelse(pus$MSP == "Married", 1, 0)
pus$MSPX <- factor(pus$MSPX)
levels(pus$MSPX)

pus$RAC1P <- factor(pus$RAC1P)
levels(pus$RAC1P)
levels(pus$RAC1P) <- c("White", "Black or African American", "American Indian", "Alaska Native", "American Indian and Alaska Native tribes specified or American Indian or Alaska Native", 
                       "Asian", "Native Hawaiian Pacific Islander", "Some Other Race", "Two or More Races")
summary(pus$RAC1P)
prop.table(table(pus$RAC1P)) * 100
levels(pus$RAC1P)

# Could simplify RAC1P this way
#pus$RAC1P <- ifelse(pus$RAC1P == "American Indian", "Other", pus$RAC1P)
#pus$RAC1P <- ifelse(pus$RAC1P == "Alaska Native", "Other", pus$RAC1P)
#pus$RAC1P <- ifelse(pus$RAC1P == "American Indian and Alaska Native tribes specified or American Indian or Alaska Native", Other, pus$RAC1P)
#pus$RAC1P <- ifelse(pus$RAC1P == "Native Hawaiian Pacific Islander", "Other", pus$RAC1P)
#pus$RAC1P <- ifelse(pus$RAC1P == "Some Other Race", "Other", pus$RAC1P)
#pus$RAC1P <- ifelse(pus$RAC1P == "STwo or More Races", "Other", pus$RAC1P)

pus$HISP <- factor(pus$HISP)
prop.table(table(pus$HISP)) * 100
pus$HISP <- ifelse(pus$HISP == 1, 0, 1)
levels(pus$HISP)

pus$SEX <- factor(pus$SEX)
levels(pus$SEX) <- c("Male", "Female")
prop.table(table(pus$SEX)) * 100

pus$SEXX <- ifelse(pus$SEX == "Male", 0, 1)
pus$SEXX <- factor(pus$SEXX)
levels(pus$SEXX)

pus$ST <- factor(pus$ST)
levels(pus$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New_Hampshire", "New Mexico", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode_Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
prop.table(table(pus$ST)) * 100

ordered(unique(pus$SCHL))
pus$SCHL <- ifelse(pus$SCHL <= 17, 17, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 18 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")
prop.table(table(pus$SCHL)) * 100

pus$AGEPX <- ifelse(pus$AGEP <= 15, 15, pus$AGEP)
pus$AGEPX <- ifelse(pus$AGEPX >=16 & pus$AGEPX <= 20, 20, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=21 & pus$AGEPX <= 30, 30, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=31 & pus$AGEPX <= 40, 40, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=41 & pus$AGEPX <= 50, 50, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=51 & pus$AGEPX <= 60, 60, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=61 & pus$AGEPX <= 70, 70, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=71 & pus$AGEPX <= 80, 80, pus$AGEPX)
pus$AGEPX <- ifelse(pus$AGEPX >=81, 90, pus$AGEPX)
pus$AGEPX <- factor(pus$AGEPX)
levels(pus$AGEPX)
levels(pus$AGEPX) <- c("15 and Under", "16-20", "21-30", "31-40", "41-50","51-60","61-70","71-80","80+")
summary(pus$AGEPX)
prop.table(table(pus$AGEPX)) * 100

#agep left unchanged
summary(pus$AGEP)

pus$ENG <- factor(pus$ENG)
levels(pus$ENG) <- c("English only", "Very well", "Well", "Not well", "Not at all")
prop.table(table(pus$ENG)) * 100
summary(pus$ENG)

pus$NATIVITY <- factor(pus$NATIVITY)
summary(pus$NATIVITY)
prop.table(table(pus$NATIVITY)) * 100
pus$NATIVITY <- ifelse(pus$NATIVITY == 1, 1, 0)
prop.table(table(pus$NATIVITY)) * 100

pus$SCIENGP <- ifelse(pus$SCIENGP == 1, 1, 0)
pus$SCIENGRLP <- ifelse(pus$SCIENGRLP == 1, 1, 0)
summary(pus$SCIENGP)
summary(pus$SCIENGRLP)

# MAKING ADDITIONAL VARIABLES

# STEM occupations and IT occupations
x <- scan("Attachment_C_STEM.txt", what = ",", sep = "\n")
y <- strsplit(x, ",")
y <- sapply(y, '[[',1)
stem.grp <- gsub("‐", "", y)
stem.grp <- as.numeric(stem.grp)
x <- scan("IT_occupations.txt", what = ",", sep = "\n")
y <- strsplit(x, ",")
y <- sapply(y, '[[',1)
it.grp <- gsub("‐", "", y)
it.grp <- as.numeric(it.grp)

pus$STEM <- ifelse(pus$SOCP %in% stem.grp, 1, 0)
pus$IT <- ifelse(pus$SOCP %in% it.grp, 1, 0)

# Could make subsets of data this way, in future
#pus.stem <- subset(pus[pus$SOCP %in% stem.grp])
#pus.it <- subset(pus[pus$SOCP %in% it.grp])
#summary(pus.stem)
#summary(pus.it)

# IMPUTING NA VALUES
# Dealing with some of the NAs before subsetting / finalizing data

# Impute NAs to 0 for ENG, NA's are mostly English-only speakers
pus$ENG[is.na(pus$ENG)] <- "English only"
summary(pus$ENG)

# Impute NAs to 0 for college degree
pus$SCIENGP[is.na(pus$SCIENGP)] <- 0
pus$SCIENGRLP[is.na(pus$SCIENGRLP)] <- 0

# BUILDING FINAL DATA SET

#drop wagp = na, per census documentation, all are under 15 years old
pus <- subset(pus, !is.na(WAGP))

# look at complete cases 
table (complete.cases (pus))
summary  (pus [!complete.cases(pus),])

#look at ESR NA values
esr <- subset(pus, is.na(ESR))
summary(esr)
# good additional subset option

#building final out of cases where ESR not NA
final <- pus[complete.cases(pus[,pus$ESR]),]
nrow(final)

# building out final1 where ESR not "Not in labor force"
final1<-final[!(final$ESR=="Not in labor force"),]
summary(final1$ESR)
nrow(final1)

# make binary target
final1.target <- ifelse(final1$WAGP >= 50000, 1, 0)
head(final1.target)
head(final1$WAGP)
final1$target <- final1.target
head(final1.target)
head(final1$target)
# looks good
prop.table(table(final1$target)) * 100
# classes are unbalanced

# VISUALIZING DATA AND ACCESSING IMPORTANCE

require(scales)
# Make use of lattice? It's already loaded

# visualize some factors according to target class to gauge value of discrimination.
# seems to be valuable to look at WAGP rather than the target to get a sense of means

# SCHL / income
ggplot(final1, aes(x=factor(target), fill=factor(SCHL))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# age categories
ggplot(final1, aes(x=factor(target), fill=factor(AGEPX))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# age continous
# ggplot(final1, aes(x=(target), fill=factor(agep))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
# not much there

# CIT - doesn't look like much from this graph
ggplot(final1, aes(x=(target), fill=factor(CIT))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Marriage status - doesn't look like much
ggplot(final1, aes(x=(target), fill=factor(MSP))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# not much with sex either it seems
ggplot(final1, aes(x=(target), fill=factor(SEXX))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# looks like you need to be able to speak english to be 1
ggplot(final1, aes(x=(target), fill=factor(ENG))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# boxplot of continuous wages by target by key factor / pretty big differences
#Used log10 to minimize outliers
# all of these should have some power
ggplot(na.omit(final1), aes(x = factor(target), y = log10(WAGP))) + geom_boxplot(aes(fill = AGEPX), alpha = 0.5) 
ggplot(na.omit(final1), aes(x = factor(target), y = log10(WAGP))) + geom_boxplot(aes(fill = SCHL), alpha = 0.5) 
ggplot(na.omit(final1), aes(x = factor(target), y = log10(WAGP))) + geom_boxplot(aes(fill = SCIENGP), alpha = 0.5) 

# FEATURE IMPORTANCE - SIGNIFICANCE OF CATEGORICAL VARIABLES / HYPOTHESIS TESTING / INDEPENDENCE 

# Examine correlation of categorical variables
chisq.test(final1$target, final1$AGEP)
chisq.test(final1$target, final1$SEX)
chisq.test(final1$target, final1$SCHL)

# How likely are your data, assuming a true null hypothesis
# Null hypothesis = independence 
# Test the hypothesis whether one variable is independent of the target  at .05 significance level.
# As the p-value is less than the .05 significance level, we reject the null hypothesis that the variable is independent of the target.
# However, the p-values are so small, better to say it's not helpful
# p-values from chisq.test() : p-value < 2.2e-16 results were meaningless

# DROP SOME COLUMNS
colnames(final1)
final1 = subset(final1, select = -c(RT, ADJINC, FOD1P, PERNP, PINCP, PWGTP, SEX, SOCP, CIT, MSP, WAGP, AGEP))

#table (complete.cases (pus))
#summary  (pus [!complete.cases(pus),])

# What to Machine Learn
# Liklihood someone makes over some threshold of income
# remove the weights per this reference: https://www.knowbigdata.com/blog/predicting-income-level-analytics-casestudy-r
# one option, accounting for weight in summary stats and visuals. Weight = "size" of the data point
# source material: Occupations in Information Technology https://www.census.gov/content/dam/Census/library/publications/2016/acs/acs-35.pdf
# Categorical variables ENG, SCHL, ESR, RAC1P, AGEPX, ST, CIT, COW, MSP, RAC1P, ST
# Make ordinal variables out of SCHL, ENG 
# Kept AGEP as continous, but need to deal with AGEPX which is factored version of this one. 

# order some variables
final1$SCHL = ordered(final1$SCHL)
final1$ENG = ordered(final1$ENG, levels(final1$ENG) [c(5, 4, 3, 2, 1)])
levels(final1$ENG)
levels(final1$SCHL)

# LOGISTIC REGRESSION MODEL

# split into train test / cv #
set.seed(101)
intrain<- createDataPartition(final1$target,p=0.7,list = FALSE)
train<- final1[intrain,]
test <- final1[-intrain,]

# train / test separation
colnames(train)
colnames(test)
train.variables <- subset(train, select = c(1:16))
colnames(train.variables)
train.target <- subset(train, select = c(17))
colnames(train.target)
test.variables <- subset(test, select = c(1:16))
colnames(test.variables)
test.target <- subset(test, select = c(17))
colnames(test.target)

# logistic regression#
lg <- glm(target ~.,family=binomial(link='logit'),data=train)

lgprediction<- predict(lg,newdata=test.variables,type = 'response')
lgPred<- ifelse(lgprediction>0.5,1,0)
lgAcu<-confusionMatrix(lgPred,test$target)$overall[1]
lgAcu
summary(lg)

# Model was under 80% accuracy in terms of probability.
# Summary evaluation needs more time
# cols to simplify: CITX, RAC1P, ESR ??
# Examining the variables is complex
# The target classes are unbalanced 

# Second Logistic Regression Attempt

set.seed(32323)
intrain2<- createDataPartition(final2$target,p=0.7,list = FALSE)
train2<- final2[intrain2,]
test2 <- final2[-intrain2,]

# Boosting

# Simplify the variables first, as boosting did not finish on initial data set

# first let's remove agepx
final2 = subset(final1, select = -c(AGEPX))
# subset(final2, select = -c(NATIVITY))
# subset(final1, select = -c(ST))

set.seed (32323)
trCtrl = trainControl (method = "cv", number = 3)

boostFit = train (target ~., trControl = trCtrl, method = "gbm", data = final1, verbose = FALSE)
confusionMatrix (final1$target, predict (boostFit, final1))
confusionMatrix (final1$target, predict (boostFit, final1))
summary(boostFit)
boostFit
plot(boostFit)



setwd("~/Development/R_Census/csv_pus")
library(ggplot2)
library(data.table)

# Read in ACS data columns 
cols <- c("RT", "ST", "PWGTP", "AGEP", "CIT", "COW", "ENG", "SCHL", "SEX", "WAGP", "ESR", "FOD1P", "HISP", "MSP", "NATIVITY","PINCP", "RAC1P", "SOCP","ADJINC", "PERNP", "SCIENGP", "SCIENGRLP")
#first data file, pusa
pusa <- fread(file="ss15pusa.csv", select = cols)
#second data file, pusb (starts at st=29)
pusb <- fread(file="ss15pusb.csv", select = cols)
pus <- rbind(pusa, pusb)

ls.str(pus)

# Factor variables (with help from: https://www.kaggle.com/huili0140/d/census/2013-american-community-survey/the-working-moms/notebook)
pus$CIT <- factor(pus$CIT)
levels(pus$CIT) <- c("Born in the U.S.", "Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas", "Born abroad of American parent(s)", ".U.S. citizen by naturalization", "Not a citizen of the U.S.")
pus$CITX <- ifelse(pus$CIT == 5, 0, 1)

pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

pus$ESR <- factor(pus$ESR)
levels(pus$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Armed forces, at work", "Armed forces, with a job but not at work", "Not in labor force")
summary(pus$ESR)
prop.table(table(pus$ESR)) * 100

# still some remaining NA's corresponding to the 16 year olds, probably
# need to eval Armed Forces

#pus$ESRX <- ifelse(pus$ESR == "Employed", 1, pus$ESR)
#pus$ESRX <- ifelse(pus$ESR == "Unemployed", 2, pus$ESR)
#pus$ESRX <- ifelse(pus$ESR == "Not in labor force", 3, pus$ESR)

pus$MSP <- factor(pus$MSP)
levels(pus$MSP) <- c("Married", "Married", "Widowed", "Divorced", "Separated", "Never married")
pus$MSPX <- ifelse(pus$MSP == "Married", 1, 0)

pus$RAC1P <- factor(pus$RAC1P)
levels(pus$RAC1P) <- c("White", "Black or African American", "American Indian", "Alaska Native", "American Indian and Alaska Native tribes specified or American Indian or Alaska Native", 
                       "Asian", "Native Hawaiian Pacific Islander", "Some Other Race", "Two or More Races")

## Oh damn, RAC1P doesn't contain Hispanic!!
pus$HISP <- ifelse(pus$HISP == 1, 0, 1)

pus$SEX <- factor(pus$SEX)
levels(pus$SEX) <- c("Male", "Female")
pus$SEXX <- ifelse(pus$SEX == "Male", 0, 1)

pus$ST <- factor(pus$ST)
levels(pus$ST) <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New_Hampshire", "New Mexico", "New Jersey", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode_Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

pus$SCHL <- ifelse(pus$SCHL <= 17, 17, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 18 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")

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
levels(pus$AGEPX) <- c("Under 15", "16-20", "21-30", "31-40", "41-50","51-60","61-70","71-80","80+")

pus$ENG[is.na(pus$ENG)] <- 0
pus$ENG <- factor(pus$ENG)
levels(pus$ENG) <- c("English only", "Very well", "Well", "Not well", "Not at all")

pus$NATIVITY <- ifelse(pus$NATIVITY == 1, 0, 1)

ls.str(pus)


#Dealing with some NA values before subsetting data
#NA's for example, could be dropped now
#drop wagp = na, per census documentation, all are under 15 years old
pus <- subset(pus, !is.na(WAGP))
# Will still need to drop some columns as we go such as RT and ADJINC, for example
# pus = subset(pus, select = -c(RT, ADJINC))

ls.str(pus)

# create SCOP groups for later slicing and selecting
# STEM occupations people vs IT occupations
# STEM degrees vs non STEM degrees

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

pus.stem <- subset(pus[pus$SOCP %in% stem.grp])
pus.it <- subset(pus[pus$SOCP %in% it.grp])
summary(pus.stem)
summary(pus.it)

# Visualizing the data
# library(survey) not being used currently 

require(scales)
#  bar chart age by school
qplot(factor(AGEPX), data=pus, geom = 'bar', fill=factor(SCHL), xlab = "age")

#  dodge using sex
ggplot(pus, aes(x=factor(SCHL), fill=factor(SEX))) + geom_bar(position="dodge") + scale_y_continuous(labels = comma) + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# looking at IT and Stem differences
qplot(factor(AGEPX), data=pus.it, geom = 'bar', fill=factor(SCHL), main = "IT Edu")
qplot(factor(AGEPX), data=pus.stem, geom = 'bar', fill=factor(SCHL), main = "STEM Edu")
qplot(factor(AGEPX), data=pus, geom = 'bar', fill=factor(SCHL))
 
# boxplot of wages stem/it vs age general
#Used log10 to minimize outliers
ggplot(pus, aes(x = factor(AGEPX), y = log10(WAGP))) + geom_boxplot(aes(fill = AGEPX), alpha = 0.5) 
ggplot(pus.it, aes(x = factor(AGEPX), y = log10(WAGP))) + geom_boxplot(aes(fill = AGEPX), alpha = 0.5) 
ggplot(pus.stem, aes(x = factor(AGEPX), y = log10(WAGP))) + geom_boxplot(aes(fill = AGEPX), alpha = 0.5) 

# education degree and wage
ggplot(na.omit(pus), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEPX), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEPX, colour = AGEPX), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

ggplot(na.omit(pus.it), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEPX), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEPX, colour = AGEPX), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

ggplot(na.omit(pus.stem), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEPX), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEPX, colour = AGEPX), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

# build way to compare total, it, stem
pus$SOCPX <- ifelse(pus$SOCP %in% it.grp, 1, 0)
# reminder this column doesn't exist in the it and stem subgroups yet

# wage vs education, mean of it or not it
ggplot(na.omit(pus), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = SOCPX), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=SOCPX, colour = SOCPX), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

# let's try something else. Not so nice. 
qplot(factor(SCHL), data=pus, geom = 'bar', fill=factor(SOCPX), main = "Edu")

# need to remove ppl not working

# need to make a variable for stem industries?
pus$STEM <- ifelse(pus$SOCP %in% stem.grp, 1, 0)

# don't have a variable for degree holders 

# what would be a useful predictor? race or ethnicity, gender, age, degree, industry?, geography? 
# percent chance of earning 80k and above? whether they earn above or below the mean? 


# Liklihood someone makes over some threshold of income
# One option for machine learning is to remove the weights per this reference: https://www.knowbigdata.com/blog/predicting-income-level-analytics-casestudy-r
# Use PWGTP for person's weight
# one option, accounting for weight in summary stats and visuals. Weight = "size" of the data point
# source material: Occupations in Information Technology https://www.census.gov/content/dam/Census/library/publications/2016/acs/acs-35.pdf
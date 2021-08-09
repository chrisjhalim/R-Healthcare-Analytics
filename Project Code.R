
 #Clear out workspace
  rm(list=ls())
load("/Users/christopherhalim888/OneDrive - UW-Madison/ACTSCI 655/R Homework/H171.RData")
#Subset needed variables
keep <- c("yrsinus","bornusa","sex","intvlang","racethx","MARRY14X","INSCOV14","MDDLAY42","CHECK53","AGE14X", "RTHLTH53", "eduyrdg", "POVCAT14","OCCCAT53","REGION53")
dat <- H171[keep] 
dat <- subset(dat, AGE14X >= 18)
library(psych)
library(questionr)
#Years in US: only asked when the person is not born in the US, so there are many NAs for this variable
#Years in US midpoint variable: “1” for less than a year, “2” for one to four years, “3” for five to nine years, “4” for ten to fourteen years, “5” for over fifteen years
dat <- within(dat, {
  YrsInUSAMid <- NA
  YrsInUSAMid[yrsinus == 1] <- .5
  YrsInUSAMid[yrsinus == 2] <- 2
  YrsInUSAMid[yrsinus == 3] <- 7
  YrsInUSAMid[yrsinus == 4] <- 12
  YrsInUSAMid[yrsinus == 5] <- 15
})

#Years in US midpoint cubed
dat <- within(dat, {
  YrsInUSACub <- NA
  YrsInUSACub[yrsinus == 1] <- .5^3
  YrsInUSACub[yrsinus == 2] <- 2^3
  YrsInUSACub[yrsinus == 3] <- 7^3
  YrsInUSACub[yrsinus == 4] <- 12^3
  YrsInUSACub[yrsinus == 5] <- 15^3
})

#Years in US midpoint squared
dat <- within(dat, {
  YrsInUSASqr <- NA
  YrsInUSASqr[yrsinus == 1] <- .5^2
  YrsInUSASqr[yrsinus == 2] <- 2^2
  YrsInUSASqr[yrsinus == 3] <- 7^2
  YrsInUSASqr[yrsinus == 4] <- 12^2
  YrsInUSASqr[yrsinus == 5] <- 15^2
})
#Born in the USA determines immigrant status
#Born in USA variable; “1” means born in the USA, “2” means born in other country
dat <- within(dat, {
  ForeignBorn <- NA
  ForeignBorn[bornusa == 1] <- "No"
  ForeignBorn[bornusa == 2] <- "Yes"
})

dat$ForeignBorn <- factor(dat$ForeignBorn, levels = c("No","Yes"))

#English proficiency determined by language in which study was conducted – “1” means the study was in English, “2” means the study was not in English
dat <- within(dat, {
  EngProf <- NA
  EngProf[intvlang == 1] <- "Yes"
  EngProf[intvlang != 1] <- "No"
})
dat$EngProf <- factor(dat$EngProf,levels = c("No","Yes"))

#Sex – “1” for male, “2” for female
dat <- within(dat, {
  Sex <- NA 
  Sex[sex == 1] <- "Male"
  Sex[sex == 2] <- "Female"
})
dat$Sex <- factor(dat$Sex, levels = c("Male","Female"))

#Race – “1” for Hispanic, “2” for Non-Hispanic white, “3” for Non-Hispanic Black, and “4” for Non-Hispanic Asian
dat <- within(dat, {
  Race <- NA
  Race[racethx == 1] <- "Hispanic"
  Race[racethx == 2] <- "Non-Hispanic white"
  Race[racethx == 3] <- "Non-Hispanic black"
  Race[racethx == 4] <- "Non-Hispanic Asian"
})
dat$Race <- factor(dat$Race, levels = c("Non-Hispanic white","Non-Hispanic Asian","Non-Hispanic black","Hispanic"))

#Race binomials
#Binomial for Non-Hispanic white
dat <- within(dat, {
  Race1 <- NA
  Race1[Race == "Non-Hispanic white"] <- "Non-Hispanic white"
  Race1[Race != "Non-Hispanic white" & is.na(Race) == FALSE] <- "Not non-Hispanic white"
})
dat$Race1 <- factor(dat$Race1, levels = c("Not non-Hispanic white", "Non-Hispanic white"))

#Binomial for Non-Hispanic black 
dat <- within(dat, {
  Race2 <- NA
  Race2[Race == "Non-Hispanic black"] <- "Non-Hispanic black"
  Race2[Race != "Non-Hispanic black" & is.na(Race) == FALSE] <- "Not non-Hispanic black"
})
dat$Race2 <- factor(dat$Race2, levels = c("Not non-Hispanic black", "Non-Hispanic black"))

#Binomial for Non-Hispanic Asian
dat <- within(dat, {
  Race3 <- NA
  Race3[Race == "Non-Hispanic Asian"] <- "Non-Hispanic Asian"
  Race3[Race != "Non-Hispanic Asian" & is.na(Race) == FALSE] <- "Not non-Hispanic Asian"
})
dat$Race3 <- factor(dat$Race3, levels = c("Not non-Hispanic Asian", "Non-Hispanic Asian"))

#Binomial for Hispanic
dat <- within(dat, {
  Race4 <- NA
  Race4[Race == "Hispanic"] <- "Hispanic"
  Race4[Race != "Hispanic" & is.na(Race) == FALSE] <- "Not Hispanic"
})
dat$Race4 <- factor(dat$Race4, levels = c("Not Hispanic", "Hispanic"))

#Health insurance coverage - only looking at insured vs. uninsured
#Health insurance coverage – “1” for private coverate and “2” for public coverage(never uninsured), “3” for Uninsured (ever uninsured)
dat <- within(dat, {
  HealthInsCov <- NA #initialize variable within empty placeholders
  HealthInsCov[INSCOV14 == 1 | INSCOV14 == 2] <- "Never Uninsured"
  HealthInsCov[INSCOV14 == 3] <- "Ever Uninsured"
})
dat$HealthInsCov <- factor(dat$HealthInsCov, levels = c("Never Uninsured", "Ever Uninsured"))

#Marriage categorical variable
dat <- within(dat, {
  Marriage <- NA #initialize variable within empty placeholders
  Marriage[MARRY14X == 1] <- "Married"
  Marriage[MARRY14X == 3 | MARRY14X == 4] <- "Divorced/Separated"
  Marriage[MARRY14X == 5] <- "Never married"
  Marriage[MARRY14X == 2] <- "Widowed"
  Marriage[MARRY14X == -9 | MARRY14X == -8 | MARRY14X == -7 ] <- NA
})
dat$Marriage <- factor(dat$Marriage, levels = c("Married", "Divorced/Separated","Never married", "Widowed"))

#Binomials for marriage variable 
#Binomial for married 
dat <- within(dat, {
  Marriage1 <- NA #initialize variable within empty placeholders
  Marriage1[MARRY14X == 1] <- "Married"
  Marriage1[MARRY14X != 1] <- "Not Married"
  Marriage1[MARRY14X == -9 | MARRY14X == -8 | MARRY14X == -7 ] <- NA
})
dat$Marriage1 <- factor(dat$Marriage1, levels = c("Not Married", "Married"))

#Binomial for divorced/separated
dat <- within(dat, {
  Marriage2 <- NA #initialize variable within empty placeholders
  Marriage2[MARRY14X == 3 | MARRY14X == 4] <- "Divorced/Separated"
  Marriage2[MARRY14X != 3 & MARRY14X != 4] <- "Not divorced/separated"
  Marriage2[MARRY14X == -9 | MARRY14X == -8 | MARRY14X == -7 ] <- NA
})
dat$Marriage2 <- factor(dat$Marriage2, levels = c("Not divorced/separated", "Divorced/Separated"))

#Binomial for never married
dat <- within(dat, {
  Marriage3 <- NA #initialize variable within empty placeholders
  Marriage3[MARRY14X == 5] <- "Never married"
  Marriage3[MARRY14X != 5] <- "Not never married"
  Marriage3[MARRY14X == -9 | MARRY14X == -8 | MARRY14X == -7 ] <- NA
})
dat$Marriage3 <- factor(dat$Marriage3, levels = c("Not never married", "Never married"))

#Binomial for widowed
dat <- within(dat, {
  Marriage4 <- NA #initialize variable within empty placeholders
  Marriage4[MARRY14X == 2] <- "Widowed"
  Marriage4[MARRY14X != 2] <- "Not widowed"
  Marriage4[MARRY14X == -9 | MARRY14X == -8 | MARRY14X == -7 ] <- NA
})
dat$Marriage4 <- factor(dat$Marriage4, levels = c("Not widowed", "Widowed"))

#Outcome variables
#MDDLAY42 for delayed medical care – “1” for yes, “2” for no
dat <- within(dat, {
  DelayedMedCare <- NA #initialize variable within empty placeholders
  DelayedMedCare[MDDLAY42 == -8 | MDDLAY42 == -7 | MDDLAY42 == -1] <- NA
  DelayedMedCare[MDDLAY42 == 1] <- "Yes"
  DelayedMedCare[MDDLAY42 == 2] <- "No"
})
dat$DelayedMedCare <- factor(dat$DelayedMedCare, levels = c("No", "Yes"))

#LastCheckup for time since last checkup within last year – “1” for within past year, “2” for not within past year
dat <- within(dat, {
  LastCheckup <- NA
  LastCheckup[CHECK53 == -9 | CHECK53 == -8 | CHECK53 == -7 | CHECK53 == -1] <- NA
  LastCheckup[CHECK53 == 1] <- "Within Past Year"
  LastCheckup[CHECK53 > 1] <- "Not Within Past Year"
})
dat$LastCheckup <- factor(dat$LastCheckup, levels = c("Within Past Year", "Not Within Past Year"))

#Age – continuous variable for table 1 stats
dat <- within(dat, {   
  Age <- AGE14X # initialize variable     
  Age[AGE14X == -1] <- NA 
}) 

#Education levels – categorical 
dat <- within(dat, {
  EducNew <- NA  # note need this statement to initialize the variable
  EducNew[eduyrdg == 5 | eduyrdg == 6 | eduyrdg == 7] <- "Some college"
  EducNew[eduyrdg == 3 | eduyrdg == 4] <- "High school diploma"
  EducNew[eduyrdg == 1 | eduyrdg == 2] <- "Less than HS"
  EducNew[eduyrdg == 8 | eduyrdg == 9] <- "Bachelor's degree or higher"  
})
dat$EducNew <- factor(dat$EducNew, levels = c("Bachelor's degree or higher","Some college","High school diploma","Less than HS"))

#Education level – binomials 
#Binomial for some college
dat <- within(dat, {
  EducNew1 <- NA  # note need this statement to initialize the variable
  EducNew1[eduyrdg == 5 | eduyrdg == 6 | eduyrdg == 7] <- "Some college"
  EducNew1[eduyrdg == 8 | eduyrdg == 9 | (eduyrdg < 5 & eduyrdg > 0)] <- "Not some college" 
})
dat$EducNew1 <- factor(dat$EducNew1, levels = c("Not some college","Some college"))

#Binomial for high school diploma
dat <- within(dat, {
  EducNew2 <- NA  # note need this statement to initialize the variable
  EducNew2[eduyrdg == 3 | eduyrdg == 4] <- "High school diploma"
  EducNew2[eduyrdg == 8 | eduyrdg == 9 | eduyrdg == 5 | eduyrdg == 6 | eduyrdg == 7 | (eduyrdg < 3 & eduyrdg > 0)] <- "Not high school diploma"
})
dat$EducNew2 <- factor(dat$EducNew2, levels = c("Not high school diploma","High school diploma"))

#Binomial for less than high school 
dat <- within(dat, {
  EducNew3 <- NA  # note need this statement to initialize the variable
  EducNew3[eduyrdg == 1 | eduyrdg == 2] <- "Less than HS"
  EducNew3[eduyrdg > 2] <- "Not less than HS"
})
dat$EducNew3 <- factor(dat$EducNew3, levels = c("Not less than HS","Less than HS"))

#Binomial for bachelor’s degree or higher 
dat <- within(dat, {
  EducNew4 <- NA  # note need this statement to initialize the variable
  EducNew4[eduyrdg == 8 | eduyrdg == 9] <- "Bachelor's degree or higher"
  EducNew4[eduyrdg < 8 & eduyrdg > 0] <- "Not Bachelor's degree or higher"
})
dat$EducNew4 <- factor(dat$EducNew4, levels = c("Not Bachelor's degree or higher","Bachelor's degree or higher"))

#Health Status (as perceived by participant) – categorical
dat <- within(dat, {
  HealthStat <- NA #initialize variable
  HealthStat[RTHLTH53 == -1] <- NA
  HealthStat[RTHLTH53 == 2] <- "Very good"
  HealthStat[RTHLTH53 == 3] <- "Good"
  HealthStat[RTHLTH53 == 4 | RTHLTH53 == 5] <- "Fair/Poor"
  HealthStat[RTHLTH53 == 1] <- "Excellent"
})

dat$HealthStat <- factor(dat$HealthStat, levels = c("Excellent", "Very good", "Good","Fair/Poor"))

#Health status binomials
#Binomial for “very good” health
dat <- within(dat, {
  HealthStat1 <- NA #initialize variable
  HealthStat1[RTHLTH53 == -1] <- NA
  HealthStat1[RTHLTH53 == 2] <- "Very good"
  HealthStat1[RTHLTH53 > 0 & RTHLTH53 != 2] <- "Not very good"
})
dat$HealthStat1 <- factor(dat$HealthStat1, levels = c("Not very good", "Very good"))

#Binomial for “good” health
dat <- within(dat, {
  HealthStat2 <- NA #initialize variable
  HealthStat2[RTHLTH53 == -1] <- NA
  HealthStat2[RTHLTH53 == 3] <- "Good"
  HealthStat2[RTHLTH53 > 0 & RTHLTH53 != 3] <- "Not good"
})
dat$HealthStat2 <- factor(dat$HealthStat2, levels = c("Not good", "Good"))

#Binomial for “fair/poor” health
dat <- within(dat, {
  HealthStat3 <- NA #initialize variable
  HealthStat3[RTHLTH53 == -1] <- NA
  HealthStat3[RTHLTH53 == 4 | RTHLTH53 == 5] <- "Fair/Poor"
  HealthStat3[RTHLTH53 > 0 & RTHLTH53 != 4] <- "Not fair/poor"
})
dat$HealthStat3 <- factor(dat$HealthStat3, levels = c("Not fair/poor", "Fair/Poor"))

#Binomial for “excellent” health
dat <- within(dat, {
  HealthStat4 <- NA #initialize variable
  HealthStat4[RTHLTH53 == -1] <- NA
  HealthStat4[RTHLTH53 == 1] <- "Excellent"
  HealthStat4[RTHLTH53 > 1] <- "Not excellent"
})
dat$HealthStat4 <- factor(dat$HealthStat4, levels = c("Not excellent", "Excellent"))

#Family income as percentage of poverty line
#Family income - categorical
dat <- within(dat, {
  Income <- NA
  Income[POVCAT14 == 1 | POVCAT14 == 2] <- "Poor/near poor"
  Income[POVCAT14 == 3] <- "Low income"
  Income[POVCAT14 == 4] <- "Middle income"
  Income[POVCAT14 == 5] <- "High income"
})
dat$Income <- factor(dat$Income, levels = c("High income", "Middle income","Low income","Poor/near poor"))

#Family Income binomials
#Binomial for poor/near poor
dat <- within(dat, {
  Income1 <- NA
  Income1[POVCAT14 == 1 | POVCAT14 == 2] <- "Poor/near poor"
  Income1[POVCAT14 > 0 & POVCAT14 != 1 & POVCAT14 != 2] <- "Not poor/near poor"
})
dat$Income1 <- factor(dat$Income1, levels = c("Not poor/near poor", "Poor/near poor"))

#Binomial for low income
dat <- within(dat, {
  Income2 <- NA
  Income2[POVCAT14 == 3] <- "Low income"
  Income2[POVCAT14 != 3 & POVCAT14 > 0] <- "Not low income"
})
dat$Income2 <- factor(dat$Income2, levels = c("Not low income","Low income"))

#Binomial for middle income
dat <- within(dat, {
  Income3 <- NA
  Income3[POVCAT14 == 4] <- "Middle income"
  Income3[POVCAT14 != 4 & POVCAT14 > 0] <- "Not middle income"
})
dat$Income3 <- factor(dat$Income3, levels = c("Not middle income","Middle income"))

#Binomial for high income
dat <- within(dat, {
  Income4 <- NA
  Income4[POVCAT14 == 5] <- "High income"
  Income4[POVCAT14 != 5 & POVCAT14 > 0] <- "Not high income"
})
dat$Income4 <- factor(dat$Income4, levels = c("Not high income","High income"))

#Type of occupation - split into new (1-5) vs. traditional (6-9) types
dat <- within(dat, {
  OccCat <- NA
  OccCat[OCCCAT53 >= 1 & OCCCAT53 <= 5] <- "New"
  OccCat[OCCCAT53 >= 6 & OCCCAT53 <= 9] <- "Traditional"
})
dat$OccCat <- factor(dat$OccCat, levels = c("New","Traditional"))

#Region of residence – “1” for Northeast, “2” for Midwest, “3” for South, “4” for West
dat <- within(dat, {
  Region <- NA
  Region[REGION53 == 1] <- "Northeast"
  Region[REGION53 == 2] <- "Midwest"
  Region[REGION53 == 3] <- "South"
  Region[REGION53 == 4] <- "West"
})

dat$Region <- factor(dat$Region, levels = c("Northeast","Midwest","South","West"))

#Remove negative vals for eduyrdg, marriage, and health stat 
dat <- dat[dat$eduyrdg > 0,]
dat <- dat[dat$MARRY14X > 0,]
dat <- dat[dat$RTHLTH53 > 0,]

#Remove unnecessary variables
dat <- subset(dat, select = -c(yrsinus,bornusa,sex,intvlang,racethx,MARRY14X,INSCOV14,MDDLAY42,CHECK53,AGE14X, RTHLTH53, eduyrdg, POVCAT14,OCCCAT53,REGION53))
#Remove data that has NA for any columns
#Did not include YrsInUSA because this is NA for US natives
#Also did not include the added columns - region & occupation type (these are only used for defining their region type, and including would cause loss of most of data)
dat <- dat[!(is.na(dat$ForeignBorn)) & !(is.na(dat$EngProf)) & !(is.na(dat$Sex)) & !(is.na(dat$Race)) & !(is.na(dat$HealthInsCov)) & !(is.na(dat$DelayedMedCare)) & !(is.na(dat$LastCheckup)) !(is.na(dat$Age)),]

#Determine skill ratio of each region (high/low)
#SR >= .25 is high and < .25 is low
NortheastSR <- sum(dat$ForeignBorn == "Yes" & dat$EducNew4 == "Bachelor's degree or higher" & dat$Region == "Northeast", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$EducNew4 != "Bachelor's degree or higher" & dat$Region == "Northeast", na.rm = TRUE)
MidwestSR <- sum(dat$ForeignBorn == "Yes" & dat$EducNew4 == "Bachelor's degree or higher" & dat$Region == "Midwest", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$EducNew4 != "Bachelor's degree or higher" & dat$Region == "Midwest", na.rm = TRUE)
SouthSR <- sum(dat$ForeignBorn == "Yes" & dat$EducNew4 == "Bachelor's degree or higher" & dat$Region == "South", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$EducNew4 != "Bachelor's degree or higher" & dat$Region == "South", na.rm = TRUE)
WestSR <- sum(dat$ForeignBorn == "Yes" & dat$EducNew4 == "Bachelor's degree or higher" & dat$Region == "West", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$EducNew4 != "Bachelor's degree or higher" & dat$Region == "West", na.rm = TRUE)

#Determine occupation ratio of each region (new/traditional)
#OR >= 2.25 is high and < 2.25 is low
NortheastOccR <- sum(dat$ForeignBorn == "Yes" & dat$OccCat == "New" & dat$Region == "Northeast", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$OccCat == "Traditional" & dat$Region == "Northeast", na.rm = TRUE)
MidwestOccR <- sum(dat$ForeignBorn == "Yes" & dat$OccCat == "New" & dat$Region == "Midwest", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$OccCat == "Traditional" & dat$Region == "Midwest", na.rm = TRUE)
SouthOccR <- sum(dat$ForeignBorn == "Yes" & dat$OccCat == "New" & dat$Region == "South", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$OccCat == "Traditional" & dat$Region == "South", na.rm = TRUE)
WestOccR <- sum(dat$ForeignBorn == "Yes" & dat$OccCat == "New" & dat$Region == "West", na.rm = TRUE)/sum(dat$ForeignBorn == "Yes" & dat$OccCat == "Traditional" & dat$Region == "West", na.rm = TRUE)

#Determine region destination type
#Northeast: High SR, High OR
#Midwest: High SR, High OR
#South: Low SR, Low OR
#West: Low SR, Low OR
#So we will only have two levels: high SR, high OR and low SR, low OR
dat <- within(dat, {
  RegionDestType <- NA
  RegionDestType[Region == "Northeast" | Region == "Midwest"] <- "New, high-skill"
  RegionDestType[Region == "South" | Region == "West"] <- "Traditional, low-skill"
})
factor(dat$RegionDestType,levels = c("New, high-skill","Traditional, low-skill"))

#Add age^2 and age^3 to dat for glm
dat <- within(dat, {
  Age2 <- Age^2
  Age3 <- Age^3
})

#View basic stats
summary(dat)
psych::describe(dat)

#Table 1 US data
USdat <- dat[dat$ForeignBorn == "No",]
USSubUnmetNeed <- sum(USdat$DelayedMedCare == "Yes")/nrow(USdat)
USObUnmetNeed <- sum(USdat$LastCheckup == "Not Within Past Year")/nrow(USdat)
USForeignBorn <- sum(USdat$ForeignBorn == "Yes")/nrow(USdat)
USAgeMean <- mean(USdat$Age)
USAgesd <- sd(USdat$Age)
USFemale <- sum(USdat$Sex == "Female")/nrow(USdat)
USRaceWhite <- sum(USdat$Race == "Non-Hispanic white")/nrow(USdat)
USRaceBlack <- sum(USdat$Race == "Non-Hispanic black")/nrow(USdat)
USRaceHispanic <- sum(USdat$Race == "Hispanic")/nrow(USdat)
USRaceAsian <- sum(USdat$Race == "Non-Hispanic Asian")/nrow(USdat)
USHealthStatPoor <- sum(USdat$HealthStat3 == "Fair/Poor", na.rm=TRUE)/nrow(USdat)
USHealthStatGood <- sum(USdat$HealthStat2 == "Good", na.rm = TRUE)/nrow(USdat)
USHealthStatVGood <- sum(USdat$HealthStat1 == "Very good", na.rm = TRUE)/nrow(USdat)
USHealthStatExcellent <- sum(USdat$HealthStat4 == "Excellent", na.rm = TRUE)/nrow(USdat)
USEducLessThanHS <- sum(USdat$EducNew3 == "Less than HS", na.rm = TRUE)/nrow(USdat)
USEducHS <- sum(USdat$EducNew2 == "High school diploma", na.rm = TRUE)/nrow(USdat)
USEducSomeCollege <- sum(USdat$EducNew1 == "Some college", na.rm = TRUE)/nrow(USdat)
USEducBachelors <- sum(USdat$EducNew4 == "Bachelor's degree or higher", na.rm = TRUE)/nrow(USdat)
USIncPoor <- sum(USdat$Income1 == "Poor/near poor", na.rm = TRUE)/nrow(USdat)
USIncLow <- sum(USdat$Income2 == "Low income", na.rm = TRUE)/nrow(USdat)
USIncMid <- sum(USdat$Income3 == "Middle income", na.rm = TRUE)/nrow(USdat)
USIncHigh <- sum(USdat$Income4 == "High income", na.rm = TRUE)/nrow(USdat)
USUninsured <- sum(USdat$HealthInsCov == "Ever Uninsured", na.rm = TRUE)/nrow(USdat)
USFamMarried <- sum(USdat$Marriage1 == "Married", na.rm = TRUE)/nrow(USdat)
USFamWidowed <- sum(USdat$Marriage4 == "Widowed", na.rm = TRUE)/nrow(USdat)
USFamDivorced <- sum(USdat$Marriage2 == "Divorced/Separated",na.rm = TRUE)/nrow(USdat)
USFamNeverMarried <- sum(USdat$Marriage3 == "Never married", na.rm = TRUE)/nrow(USdat)
USYrsInUSMid <- NA
USEnglishIntv <- NA
USRegionDestTypeNH <- NA
USRegionDestTypeTL <- NA

USTable1 <- data.frame(USSubUnmetNeed,USObUnmetNeed,USForeignBorn,USAgeMean,USAgesd,USFemale,USRaceWhite,USRaceBlack,USRaceHispanic,USRaceAsian,USHealthStatPoor,USHealthStatGood,USHealthStatVGood,USHealthStatExcellent,USEducLessThanHS,USEducHS,USEducSomeCollege,USEducBachelors,USIncPoor,USIncLow,USIncMid,USIncHigh,USUninsured,USFamMarried,USFamWidowed,USFamDivorced,USFamNeverMarried,USYrsInUSMid,USEnglishIntv,USRegionDestTypeNH,USRegionDestTypeTL)
USTable1 <- data.frame(t(USTable1))

#Table 1 Foreign Born/Immigrant data
Immdat <- dat[dat$ForeignBorn == "Yes",]
ImmSubUnmetNeed <- sum(Immdat$DelayedMedCare == "Yes")/nrow(Immdat)
ImmObjUnmetNeed <- sum(Immdat$LastCheckup == "Not Within Past Year")/nrow(Immdat)
ImmForeignBorn <- sum(Immdat$ForeignBorn == "Yes")/nrow(Immdat)
ImmAgeMean <- mean(Immdat$Age)
ImmAgeSD <- sd(Immdat$Age)
ImmFemale <- sum(Immdat$Sex == "Female")/nrow(Immdat)
ImmRaceWhite <- sum(Immdat$Race == "Non-Hispanic white")/nrow(Immdat)
ImmRaceBlack <- sum(Immdat$Race == "Non-Hispanic black")/nrow(Immdat)
ImmRaceHispanic <- sum(Immdat$Race == "Hispanic")/nrow(Immdat)
ImmRaceAsian <- sum(Immdat$Race == "Non-Hispanic Asian")/nrow(Immdat)
ImmHealthStatPoor <- sum(Immdat$HealthStat3 == "Fair/Poor", na.rm=TRUE)/nrow(Immdat)
ImmHealthStatGood <- sum(Immdat$HealthStat2 == "Good", na.rm = TRUE)/nrow(Immdat)
ImmHealthStatVGood <- sum(Immdat$HealthStat1 == "Very good", na.rm = TRUE)/nrow(Immdat)
ImmHealthStatExcellent <- sum(Immdat$HealthStat4 == "Excellent", na.rm = TRUE)/nrow(Immdat)
ImmEducLessThanHS <- sum(Immdat$EducNew3 == "Less than HS", na.rm = TRUE)/nrow(Immdat)
ImmEducHS <- sum(Immdat$EducNew2 == "High school diploma", na.rm = TRUE)/nrow(Immdat)
ImmEducSomeCollege <- sum(Immdat$EducNew1 == "Some college", na.rm = TRUE)/nrow(Immdat)
ImmEducBachelors <- sum(Immdat$EducNew4 == "Bachelor's degree or higher", na.rm = TRUE)/nrow(Immdat)
ImmIncPoor <- sum(Immdat$Income1 == "Poor/near poor", na.rm = TRUE)/nrow(Immdat)
ImmIncLow <- sum(Immdat$Income2 == "Low income", na.rm = TRUE)/nrow(Immdat)
ImmIncMid <- sum(Immdat$Income3 == "Middle income", na.rm = TRUE)/nrow(Immdat)
ImmIncHigh <- sum(Immdat$Income4 == "High income", na.rm = TRUE)/nrow(Immdat)
ImmUninsured <- sum(Immdat$HealthInsCov == "Ever Uninsured", na.rm = TRUE)/nrow(Immdat)
ImmFamMarried <- sum(Immdat$Marriage1 == "Married", na.rm = TRUE)/nrow(Immdat)
ImmFamWidowed <- sum(Immdat$Marriage4 == "Widowed", na.rm = TRUE)/nrow(Immdat)
ImmFamDivorced <- sum(Immdat$Marriage2 == "Divorced/Separated",na.rm = TRUE)/nrow(Immdat)
ImmFamNeverMarried <- sum(Immdat$Marriage3 == "Never married", na.rm = TRUE)/nrow(Immdat)
ImmYrsInUSMid <- mean(Immdat$YrsInUSAMid,na.rm=TRUE)
ImmEnglishIntv <- sum(Immdat$EngProf == "Yes",na.rm=TRUE)/nrow(Immdat) 
ImmRegionDestTypeNH <- sum(Immdat$RegionDestType == "New, high-skill",na.rm=TRUE)/nrow(Immdat)
ImmRegionDestTypeTL <- sum(Immdat$RegionDestType == "Traditional, low-skill",na.rm = TRUE)/nrow(Immdat)

ImmTable1 <- data.frame(ImmSubUnmetNeed,ImmObjUnmetNeed,ImmForeignBorn,ImmAgeMean,ImmAgeSD,ImmFemale,ImmRaceWhite,ImmRaceBlack,ImmRaceHispanic,ImmRaceAsian,ImmHealthStatPoor,ImmHealthStatGood,ImmHealthStatVGood,ImmHealthStatExcellent,ImmEducLessThanHS,ImmEducHS,ImmEducSomeCollege,ImmEducBachelors,ImmIncPoor,ImmIncLow,ImmIncMid,ImmIncHigh,ImmUninsured,ImmFamMarried,ImmFamWidowed,ImmFamDivorced,ImmFamNeverMarried,ImmYrsInUSMid,ImmEnglishIntv,ImmRegionDestTypeNH,ImmRegionDestTypeTL)
ImmTable1 <- data.frame(t(ImmTable1))

#Table 1 total data 
total_bachelors <- sum(dat$EducNew4 == "Bachelor's degree or higher", na.rm = TRUE)/nrow(dat)
total_somecollege <- sum(dat$EducNew1 == "Some college", na.rm = TRUE)/nrow(dat)
total_highschool <- sum(dat$EducNew2 == "High school diploma", na.rm = TRUE)/nrow(dat)
total_lessthanhs <- sum(dat$EducNew3 == "Less than HS", na.rm = TRUE)/nrow(dat)
total_hs_excellent <- sum(dat$HealthStat4 == "Excellent", na.rm = TRUE)/nrow(dat)
total_hs_verygood <- sum(dat$HealthStat1 == "Very good", na.rm = TRUE)/nrow(dat)
total_hs_good <- sum(dat$HealthStat2 == "Good", na.rm = TRUE)/nrow(dat)
total_hs_fairpoor <-  sum(dat$HealthStat3 == "Fair/Poor", na.rm=TRUE)/nrow(dat)
total_inc_high <- sum(dat$Income4 == "High income", na.rm = TRUE)/nrow(dat)
total_inc_poor <- sum(dat$Income1 == "Poor/near poor", na.rm = TRUE)/nrow(dat)
total_inc_low <- sum(dat$Income2 == "Low income", na.rm = TRUE)/nrow(dat)
total_inc_middle <- sum(dat$Income3 == "Middle income", na.rm = TRUE)/nrow(dat)
total_age_mean <- mean(dat$Age)
total_age_sd <- sd(dat$Age)
total_sub_unmet <- sum(dat$DelayedMedCare == "Yes")/nrow(dat)
total_ob_unmet <- sum(dat$LastCheckup == "Not Within Past Year")/nrow(dat)
total_foreign_born <- sum(dat$ForeignBorn == "Yes")/nrow(dat)
total_female <- sum(dat$Sex == "Female")/nrow(dat)
total_white <- sum(dat$Race == "Non-Hispanic white")/nrow(dat)
total_black <- sum(dat$Race == "Non-Hispanic black")/nrow(dat)
total_hispanic <- sum(dat$Race == "Hispanic")/nrow(dat)
total_asian <- sum(dat$Race == "Non-Hispanic Asian")/nrow(dat)
total_uninsured <- sum(dat$HealthInsCov == "Ever Uninsured", na.rm = TRUE)/nrow(dat)
total_married <- sum(dat$Marriage1 == "Married", na.rm = TRUE)/nrow(dat)
total_widowed <- sum(dat$Marriage4 == "Widowed", na.rm = TRUE)/nrow(dat)
total_divorced <- sum(dat$Marriage2 == "Divorced/Separated",na.rm = TRUE)/nrow(dat)
total_nevermarried <-  sum(dat$Marriage3 == "Never married", na.rm = TRUE)/nrow(dat)
total_yrsinusmid <- NA
total_englishintv <- NA
total_regiondesttypeNH <- NA
total_regiondesttypeTL <- NA

TotTable1 <- data.frame(total_sub_unmet,total_ob_unmet,total_foreign_born,total_age_mean,total_age_sd,total_female,total_white,total_black,total_hispanic,total_asian,total_hs_fairpoor,total_hs_good,total_hs_verygood,total_hs_excellent,total_lessthanhs,total_highschool,total_somecollege,total_bachelors,total_inc_poor,total_inc_low,total_inc_middle,total_inc_high,total_uninsured,total_married,total_widowed,total_divorced,total_nevermarried,total_yrsinusmid,total_englishintv,total_regiondesttypeNH,total_regiondesttypeTL)
TotTable1 <- data.frame(t(TotTable1))

#Combine tables to create full Table 1
Table1 <- cbind(TotTable1,USTable1,ImmTable1)
row.names(Table1) <- c("UnmetNeed_Subjective","UnmetNeed_Objective","Foreign_Born","Age_Mean","Age_sd","Female","Race_White","Race_Black","Race_Hispanic","Race_Asian","HealthStat_FairPoor","HealthStat_Good","HealthStat_VeryGood","HealthStat_Excellent","Education_LessThanHS","Education_HS","Education_SomeCollege","Education_Bachelors","Inc_Poor","Inc_Low","Inc_Middle","Inc_High","Ever_Uninsured","MarriageStat_Married","MarriageStat_Widowed","MarriageStat_Divorced","MarriageStat_NeverMarried","YrsInUSMidpoint","EnglishProficiency","RegionDestType_NH","RegionDestType_TL")

#Calculate model odds ratio and confidence intervals for Table 2
#Table 2, Panel A, Model 1
tab2_panA_mod1 <- glm(DelayedMedCare ~ ForeignBorn, family = binomial(link = "logit"), data = dat)
summary(tab2_panA_mod1)
odds.ratio(tab2_panA_mod1)

#Table 2, Panel A, Model 2
tab2_panA_mod2 <- glm(DelayedMedCare ~ ForeignBorn + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race, family = binomial(link = "logit"), data = dat)
summary(tab2_panA_mod2)
odds.ratio(tab2_panA_mod2)

#Table 2, Panel A, Model 3
tab2_panA_mod3 <- glm(DelayedMedCare ~ ForeignBorn + EducNew + Income + HealthInsCov + Marriage + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race, family = binomial(link = "logit"), data = dat)
summary(tab2_panA_mod3)
odds.ratio(tab2_panA_mod3)

#Table 2, Panel B, Model 1 
tab2_panB_mod1 <- glm(DelayedMedCare ~ EngProf + RegionDestType + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab2_panB_mod1)
odds.ratio(tab2_panB_mod1)

#Table 2, Panel B, Model 2 
tab2_panB_mod2 <- glm(DelayedMedCare ~ EngProf + RegionDestType + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab2_panB_mod2)
odds.ratio(tab2_panB_mod2)

#Table 2, Panel B, Model 3
tab2_panB_mod3 <- glm(DelayedMedCare ~ EngProf + RegionDestType + EducNew + Income + HealthInsCov + Marriage + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab2_panB_mod3)
odds.ratio(tab2_panB_mod3)

#Calculte odds ratios and confidence intervals for Table 3
#Table 3, Panel A, Model 1
tab3_panA_mod1 <- glm(LastCheckup ~ ForeignBorn, family = binomial(link = "logit"), data = dat)
summary(tab3_panA_mod1)
odds.ratio(tab3_panA_mod1)

#Table 3, Panel A, Model 2
tab3_panA_mod2 <- glm(LastCheckup ~ ForeignBorn + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race, family = binomial(link = "logit"), data = dat)
summary(tab3_panA_mod2)
odds.ratio(tab3_panA_mod2)

#Table 3, Panel A, Model 3
tab3_panA_mod3 <- glm(LastCheckup ~ ForeignBorn + EducNew + Income + HealthInsCov + Marriage + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race, family = binomial(link = "logit"), data = dat)
summary(tab3_panA_mod3)
odds.ratio(tab3_panA_mod3)

#Table 3, Panel B, Model 1
tab3_panB_mod1 <- glm(LastCheckup ~ EngProf + RegionDestType + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab3_panB_mod1)
odds.ratio(tab3_panB_mod1)

#Table 3, Panel B, Model 2
tab3_panB_mod2 <- glm(LastCheckup ~ EngProf + RegionDestType + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab3_panB_mod2)
odds.ratio(tab3_panB_mod2)

#Table 3, Panel B, Model 3
tab3_panB_mod3 <- glm(LastCheckup ~ EngProf + RegionDestType + EducNew + Income + HealthInsCov + Marriage + Age + Age2 + Age3 + Sex + Race + HealthStat + Age:Sex + Age:Race + Sex:Race + YrsInUSAMid + YrsInUSASqr + YrsInUSACub, family = binomial(link = "logit"), data = Immdat)
summary(tab3_panB_mod3)
odds.ratio(tab3_panB_mod3)










#Install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("plyr")
install.packages("dplyr")
install.packages("patchwork")
install.packages("survey")
install.packages("mitools")
install.packages("lme4")
install.packages("car")
install.packages("moments")

#Packages
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(patchwork)
library(survey)
library(mitools)
library(lme4)
library(car)
library(moments)

#Merging public and restricted dataset
merged.df <- merge(NLAAS_Data_Public, NLAAS_Data_Restricted, by.x="CPESCASE", by.y="CPESCASE")

#Renaming merged dataset for easier use
df <- merged.df

#Renaming race/ancestry variables with correct labels
df["RANCEST"][df["RANCEST"] == "1"] <- "Vietnamese"
df["RANCEST"][df["RANCEST"] == "2"] <- "Filipino"
df["RANCEST"][df["RANCEST"] == "3"] <- "Chinese"
df["RANCEST"][df["RANCEST"] == "4"] <- "All Other Asian"
df["RANCEST"][df["RANCEST"] == "5"] <- "Cuban"
df["RANCEST"][df["RANCEST"] == "6"] <- "Puerto Rican"
df["RANCEST"][df["RANCEST"] == "7"] <- "Mexican"
df["RANCEST"][df["RANCEST"] == "8"] <- "All Other Hispanic"
df["RANCEST"][df["RANCEST"] == "9"] <- "Afro-Caribbean"
df["RANCEST"][df["RANCEST"] == "10"] <- "African American"
df["RANCEST"][df["RANCEST"] == "11"] <- "Non-Latino Whites"
df["RANCEST"][df["RANCEST"] == "12"] <- "All Other"

#ordering based on numbers
level_order <- c('Vietnamese', 
                 'Filipino', 
                 'Chinese', 
                 'All Other Asian',
                 'Cuban',
                 'Puerto Rican',
                 'Mexican',
                 'All Other Hispanic',
                 'Afro-Caribbean',
                 'African American',
                 'Non-Latino Whites',
                 'All Other')

#N number of individuals per race/ancestry category
ggplot(data=df, aes(x=factor(RANCEST, level=level_order))) + geom_bar()

sum(df$RANCEST == "Vietnamese")
sum(df$RANCEST == "Filipino")
sum(df$RANCEST == "Chinese")
sum(df$RANCEST == "All Other Asian")
sum(df$RANCEST == "Cuban")
sum(df$RANCEST == "Puerto Rican")
sum(df$RANCEST == "Mexican")
sum(df$RANCEST == "All Other Hispanic")

#creating a composite self-rated health score
df$overallhealth <- df$SC8_1 + df$SC8_2

#creating a composite disability score
df$DISABILITYSCORE <- df$CARESCORE + df$COGSCORE + df$MOVESCORE + df$OUTROLESCORE + df$SOCIALSCORE

# iwyear is the year they participated in the study, age is their age, and DM1_6A is their age when they arrived here
# adding year of arrival to US to dataframe
df <- mutate(df, yeararrival = iwyear-Age+DM1_6A)

#subsetting Cubans from dataset
cubans<-subset(df, RANCEST=="Cuban")

#Number of Cubans born in US
sum(cubans$DM1_6 == "1")

#Number of Cubans born in another country (assuming this means they were born in Cuba)
sum(cubans$DM1_6 == "2")

#Creating migration wave bins (Method 1: Binning based on natural lulls in the data combined with historical events)
cubans$migrationwave1 <- cut(cubans$yeararrival,
                             breaks = c(1953, 1965, 1975, 1977, 1987, 1991, 2002),
                             include.lowest = T,
                             right = F)

#Renaming bins
cubans$migrationwave1name <- revalue(cubans$migrationwave1, c ("[1953,1965)" = "Early Cuban Exiles",
                                                               "[1965,1975)" = "Freedom Flights",
                                                               "[1975,1977)" = "No Wave (Early)",
                                                               "[1977,1987)" = "Pre- and Post- Mariel",
                                                               "[1987,1991)" = "No Wave (Late)",
                                                               "[1991,2002]" = "Special Period"))


custom_colors <- c("Early Cuban Exiles" = "#007c88", 
                   "Freedom Flights" = "#89d1d1", 
                   "Pre- and Post- Mariel" = "#855b9d", 
                   "Special Period" = "#dbc9ef")

#Recoding 1st and 2nd gen into migration wave category

cubans$DM1_7 <-as.character(cubans$DM1_7)
cubans$DM1_6 <-as.character(cubans$DM1_6)
cubans$migrationwave1name<-as.character(cubans$migrationwave1name)

cubans$migrationwave1name[cubans$DM1_7 == '2' & cubans$DM1_6 == '1'] <- "1gen"
cubans$migrationwave1name[cubans$DM1_7 == '1' & cubans$DM1_6 == '1'] <- "1gen"
cubans$migrationwave1name[cubans$DM1_7 == '3' & cubans$DM1_6 == '1'] <- "2gen"

count(cubans$migrationwave1name == "1gen")
count(cubans$migrationwave1name == "2gen")

#Number of Cubans that immigrated to US per year plot, colored by migration waves of interest
ggplot(cubans, aes(x=factor(yeararrival), fill=migrationwave1name)) + 
  geom_bar() + 
  scale_fill_manual(values = custom_colors) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45))

#Counts in each bin
ggplot(cubans, aes(x=factor(migrationwave1name))) + 
  geom_bar() + theme_minimal()

count(cubans$migrationwave1name == "Early Cuban Exiles")
count(cubans$migrationwave1name == "Freedom Flights")
count(cubans$migrationwave1name == "No Wave (Early)")
count(cubans$migrationwave1name == "Pre- and Post- Mariel")
count(cubans$migrationwave1name == "No Wave (Late)")
count(cubans$migrationwave1name == "Special Period")
count(cubans$migrationwave1name == "1gen")
count(cubans$migrationwave1name == "2gen")

#Subset of only migration waves of interest
cubanwaves<- subset(cubans, migrationwave1name == 'Early Cuban Exiles' | 
                      migrationwave1name =='Freedom Flights' | 
                      migrationwave1name =='Pre- and Post- Mariel' | 
                      migrationwave1name =='Special Period')

#incorporating sampling weights (this happens again later in the script when I create new variables that have to be reincrporated into the survey design)
survey_design <- svydesign(ids = ~1, weights = ~NLAASWGT, data = cubanwaves)

#Plot of counts
ggplot(cubanwaves, aes(x=migrationwave1name, fill = migrationwave1name)) + 
  geom_bar()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal()

#Number of Cubans that immigrated to US per year plot, colored by migration wave for dataset with just individuals of interest
ggplot(cubanwaves, aes(x=factor(yeararrival), fill = migrationwave1name)) + 
  geom_bar()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(axis.text.x = element_text(angle = 45)) + theme(legend.position = "none")

#gender distribution across migration wave
cubanwaves$SEX <-as.factor(cubanwaves$SEX)
cubanwaves$SEX <- revalue(cubanwaves$SEX, c ("1" = "Male","2" = "Female"))

table(cubanwaves$migrationwave1name, cubanwaves$SEX)

#regional distribution across migration wave
cubanwaves$REGION <-as.factor(cubanwaves$REGION)
cubanwaves$REGION <- revalue(cubanwaves$REGION, c ("1" = "Northeast","2" = "Midwest", "3"= "South", "4"="West"))

table(cubanwaves$migrationwave1name, cubanwaves$REGION)

#average age, se, and sd across migration waves, weighted
weighted_age<-svyby(~Age, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_age

weighted_age_sd<-svyby(~Age, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_age_sd_2 <- sqrt(weighted_age_sd$Age)
weighted_age_sd_2

#plots for age across migration wave, weighted

ggplot(cubanwaves, aes(x=migrationwave1name, y=Age, fill = migrationwave1name)) + 
  geom_violin()+
  geom_point(data=weighted_age, aes(y=Age), shape=20, size=5, color = "#3ab681") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + ylim(0,100)

ggplot(cubanwaves, aes(x=migrationwave1name, y=Age, fill = migrationwave1name)) + 
  geom_boxplot()+
  geom_point(data=weighted_age, aes(y=Age), shape=20, size=5, color = "#3ab681") +
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + ylim(0,100)

#average WHODAS CARESCORE score by migration wave, weighted
weighted_CARESCORE<-svyby(~CARESCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_CARESCORE

weighted_CARESCORE_sd<-svyby(~CARESCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_CARESCORE_sd_2 <- sqrt(weighted_CARESCORE_sd$CARESCORE)
weighted_CARESCORE_sd_2

CARE.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=CARESCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,100)+
  geom_point(data=weighted_CARESCORE, aes(y=CARESCORE), shape=20, size=5, color = "#3ab681")

CARE.plot.weighted

#average WHODAS COGSCORE score by migration wave, weighted
weighted_COGSCORE<-svyby(~COGSCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_COGSCORE

weighted_COGSCORE_sd<-svyby(~COGSCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_COGSCORE_sd_2 <- sqrt(weighted_COGSCORE_sd$COGSCORE)
weighted_COGSCORE_sd_2

COG.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=COGSCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,100)+
  geom_point(data=weighted_COGSCORE, aes(y=COGSCORE), shape=20, size=5, color = "#3ab681")

COG.plot.weighted

#average WHODAS MOVESCORE score by migration wave, weighted
weighted_MOVESCORE<-svyby(~MOVESCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_MOVESCORE

weighted_MOVESCORE_sd<-svyby(~MOVESCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_MOVESCORE_sd_2 <- sqrt(weighted_MOVESCORE_sd$MOVESCORE)
weighted_MOVESCORE_sd_2

MOVE.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=MOVESCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,100)+
  geom_point(data=weighted_MOVESCORE, aes(y=MOVESCORE), shape=20, size=5, color = "#3ab681")

MOVE.plot.weighted

#average WHODAS OUTROLESCORE score by migration wave, weighted
weighted_OUTROLESCORE<-svyby(~OUTROLESCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_OUTROLESCORE

weighted_OUTROLESCORE_sd<-svyby(~OUTROLESCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_OUTROLESCORE_sd_2 <- sqrt(weighted_OUTROLESCORE_sd$OUTROLESCORE)
weighted_OUTROLESCORE_sd_2

OUTROLE.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=OUTROLESCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,100)+
  geom_point(data=weighted_OUTROLESCORE, aes(y=OUTROLESCORE), shape=20, size=5, color = "#3ab681")

OUTROLE.plot.weighted

#average WHODAS SOCIALSCORE score by migration wave, weighted
weighted_SOCIALSCORE<-svyby(~SOCIALSCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_SOCIALSCORE

weighted_SOCIALSCORE_sd<-svyby(~SOCIALSCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_SOCIALSCORE_sd_2 <- sqrt(weighted_SOCIALSCORE_sd$SOCIALSCORE)
weighted_SOCIALSCORE_sd_2

SOCIAL.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=SOCIALSCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,100)+
  geom_point(data=weighted_SOCIALSCORE, aes(y=SOCIALSCORE), shape=20, size=5, color = "#3ab681")

SOCIAL.plot.weighted

#average DISABILITYSCORE by migration wave, weighted
weighted_DISABILITYSCORE<-svyby(~DISABILITYSCORE, ~migrationwave1name, design = survey_design, FUN = svymean, na.rm = TRUE)

weighted_DISABILITYSCORE

weighted_DISABILITYSCORE_sd<-svyby(~DISABILITYSCORE, ~migrationwave1name, design = survey_design, FUN = svyvar)
weighted_DISABILITYSCORE_sd_2 <- sqrt(weighted_DISABILITYSCORE_sd$DISABILITYSCORE)
weighted_DISABILITYSCORE_sd_2

DISABILITY.plot.weighted<-ggplot(cubanwaves, aes(x=migrationwave1name, y=DISABILITYSCORE, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,300)+
  geom_point(data=weighted_DISABILITYSCORE, aes(y=DISABILITYSCORE), shape=20, size=5, color = "#3ab681")

DISABILITY.plot.weighted

ggarrange(CARE.plot, COG.plot, MOVE.plot, OUTROLE.plot, SOCIAL.plot,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow=2)

ggarrange(CARE.plot, COG.plot, MOVE.plot, OUTROLE.plot, SOCIAL.plot,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow=3)

ggarrange(CARE.plot.weighted, COG.plot.weighted, MOVE.plot.weighted, OUTROLE.plot.weighted, SOCIAL.plot.weighted,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 3, nrow=2)

ggarrange(CARE.plot.weighted, COG.plot.weighted, MOVE.plot.weighted, OUTROLE.plot.weighted, SOCIAL.plot.weighted,
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow=3)

#Checking correlation of disability scores across entire dataset

cor.test(x=df$DISABILITYSCORE, y=df$CARESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$DISABILITYSCORE, y=df$COGSCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$DISABILITYSCORE, y=df$MOVESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$DISABILITYSCORE, y=df$OUTROLESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$DISABILITYSCORE, y=df$SOCIALSCORE, method = c("pearson", "kendall", "spearman"))

cor.test(x=df$CARESCORE, y=df$COGSCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$CARESCORE, y=df$MOVESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$CARESCORE, y=df$OUTROLESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$CARESCORE, y=df$SOCIALSCORE, method = c("pearson", "kendall", "spearman"))

cor.test(x=df$COGSCORE, y=df$MOVESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$COGSCORE, y=df$OUTROLESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$COGSCORE, y=df$SOCIALSCORE, method = c("pearson", "kendall", "spearman"))

cor.test(x=df$MOVESCORE, y=df$OUTROLESCORE, method = c("pearson", "kendall", "spearman"))
cor.test(x=df$MOVESCORE, y=df$SOCIALSCORE, method = c("pearson", "kendall", "spearman"))

cor.test(x=df$OUTROLESCORE, y=df$SOCIALSCORE, method = c("pearson", "kendall", "spearman"))


#proportion of people with any disability
cubanwaves$CARESCORE.binary <- ifelse(cubanwaves$CARESCORE > 0, 1, 0)
cubanwaves$COGSCORE.binary <- ifelse(cubanwaves$COGSCORE > 0, 1, 0)
cubanwaves$MOVESCORE.binary <- ifelse(cubanwaves$MOVESCORE > 0, 1, 0)
cubanwaves$OUTROLESCORE.binary <- ifelse(cubanwaves$OUTROLESCORE > 0, 1, 0)
cubanwaves$SOCIALSCORE.binary <- ifelse(cubanwaves$SOCIALSCORE > 0, 1, 0)
cubanwaves$disability.binary <- ifelse(cubanwaves$DISABILITYSCORE > 0, 1, 0)

#chart of proportion with disability
count_cubanwaves <- table(cubanwaves$migrationwave1name, cubanwaves$disability.binary)
count_cubanwaves <- as.data.frame(count_cubanwaves)
names(count_cubanwaves) <- c("migrationwave1name", "disability.binary", "Count")

bar_width <- 0.7
group_spacing <- 0.8

disability.binary.colors <- c("0" = "#3ab681", "1" = "#9cc9b1")

ggplot(count_cubanwaves, aes(x=migrationwave1name, y= Count, fill=disability.binary)) +
  geom_bar(position = position_dodge(width = group_spacing), width = bar_width, stat = "identity") +
  scale_fill_manual(values = disability.binary.colors) +
  theme_minimal()

#new survey design to incorporate new variables
survey_design2 <- svydesign(ids = ~1, weights = ~NLAASWGT, data = cubanwaves)

#logistic regression models, weighted

logistic.CARESCORE.weighted  <- svyglm(CARESCORE.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.CARESCORE.weighted)

coefficients_CARESCORE <- coef(logistic.CARESCORE.weighted)
odds_ratios_CARESCORE<-exp(coefficients_CARESCORE)
std_errors_CARESCORE <- summary(logistic.CARESCORE.weighted)$coefficients[, "Std. Error"]
lower_ci_CARESCORE <- odds_ratios_CARESCORE * exp(-1.96 * std_errors_CARESCORE)
upper_ci_CARESCORE <- odds_ratios_CARESCORE * exp(1.96 * std_errors_CARESCORE)

odds_ratio_table_CARESCORE <- data.frame (
  Predictor = names(odds_ratios_CARESCORE),
  OddsRatio = odds_ratios_CARESCORE,
  LowerCI = lower_ci_CARESCORE,
  UpperCI = upper_ci_CARESCORE
)

odds_ratio_table_CARESCORE


logistic.COGSCORE.weighted<- svyglm(COGSCORE.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.COGSCORE.weighted)

coefficients_COGSCORE <- coef(logistic.COGSCORE.weighted)
odds_ratios_COGSCORE<-exp(coefficients_COGSCORE)
std_errors_COGSCORE <- summary(logistic.COGSCORE.weighted)$coefficients[, "Std. Error"]
lower_ci_COGSCORE <- odds_ratios_COGSCORE * exp(-1.96 * std_errors_COGSCORE)
upper_ci_COGSCORE <- odds_ratios_COGSCORE * exp(1.96 * std_errors_COGSCORE)

odds_ratio_table_COGSCORE <- data.frame (
  Predictor = names(odds_ratios_COGSCORE),
  OddsRatio = odds_ratios_COGSCORE,
  LowerCI = lower_ci_COGSCORE,
  UpperCI = upper_ci_COGSCORE
)

odds_ratio_table_COGSCORE


logistic.MOVESCORE.weighted <- svyglm(MOVESCORE.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.MOVESCORE.weighted)

coefficients_MOVESCORE <- coef(logistic.MOVESCORE.weighted)
odds_ratios_MOVESCORE<-exp(coefficients_MOVESCORE)
std_errors_MOVESCORE <- summary(logistic.MOVESCORE.weighted)$coefficients[, "Std. Error"]
lower_ci_MOVESCORE <- odds_ratios_MOVESCORE * exp(-1.96 * std_errors_MOVESCORE)
upper_ci_MOVESCORE <- odds_ratios_MOVESCORE * exp(1.96 * std_errors_MOVESCORE)

odds_ratio_table_MOVESCORE <- data.frame (
  Predictor = names(odds_ratios_MOVESCORE),
  OddsRatio = odds_ratios_MOVESCORE,
  LowerCI = lower_ci_MOVESCORE,
  UpperCI = upper_ci_MOVESCORE
)

odds_ratio_table_MOVESCORE


logistic.OUTROLESCORE.weighted <- svyglm(OUTROLESCORE.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.OUTROLESCORE.weighted)

coefficients_OUTROLESCORE <- coef(logistic.OUTROLESCORE.weighted)
odds_ratios_OUTROLESCORE<-exp(coefficients_OUTROLESCORE)
std_errors_OUTROLESCORE <- summary(logistic.OUTROLESCORE.weighted)$coefficients[, "Std. Error"]
lower_ci_OUTROLESCORE <- odds_ratios_OUTROLESCORE * exp(-1.96 * std_errors_OUTROLESCORE)
upper_ci_OUTROLESCORE <- odds_ratios_OUTROLESCORE * exp(1.96 * std_errors_OUTROLESCORE)

odds_ratio_table_OUTROLESCORE <- data.frame (
  Predictor = names(odds_ratios_OUTROLESCORE),
  OddsRatio = odds_ratios_OUTROLESCORE,
  LowerCI = lower_ci_OUTROLESCORE,
  UpperCI = upper_ci_OUTROLESCORE
)

odds_ratio_table_OUTROLESCORE


logistic.SOCIALSCORE.weighted <- svyglm(SOCIALSCORE.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.SOCIALSCORE.weighted)

coefficients_SOCIALSCORE <- coef(logistic.SOCIALSCORE.weighted)
odds_ratios_SOCIALSCORE<-exp(coefficients_SOCIALSCORE)
std_errors_SOCIALSCORE <- summary(logistic.SOCIALSCORE.weighted)$coefficients[, "Std. Error"]
lower_ci_SOCIALSCORE <- odds_ratios_SOCIALSCORE * exp(-1.96 * std_errors_SOCIALSCORE)
upper_ci_SOCIALSCORE <- odds_ratios_SOCIALSCORE * exp(1.96 * std_errors_SOCIALSCORE)

odds_ratio_table_SOCIALSCORE <- data.frame (
  Predictor = names(odds_ratios_SOCIALSCORE),
  OddsRatio = odds_ratios_SOCIALSCORE,
  LowerCI = lower_ci_SOCIALSCORE,
  UpperCI = upper_ci_SOCIALSCORE
)

odds_ratio_table_SOCIALSCORE


logistic.DISABILITY.weighted<- svyglm(disability.binary ~ migrationwave1name + Age + SEX + REGION, family = quasibinomial, design = survey_design2)
summary(logistic.DISABILITY.weighted)

coefficients_DISABILITYSCORE <- coef(logistic.DISABILITY.weighted)
odds_ratios_DISABILITYSCORE<-exp(coefficients_DISABILITYSCORE)
std_errors_DISABILITYSCORE <- summary(logistic.DISABILITY.weighted)$coefficients[, "Std. Error"]
lower_ci_DISABILITYSCORE <- odds_ratios_DISABILITYSCORE * exp(-1.96 * std_errors_DISABILITYSCORE)
upper_ci_DISABILITYSCORE <- odds_ratios_DISABILITYSCORE * exp(1.96 * std_errors_DISABILITYSCORE)

odds_ratio_table_DISABILITYSCORE <- data.frame (
  Predictor = names(odds_ratios_DISABILITYSCORE),
  OddsRatio = odds_ratios_DISABILITYSCORE,
  LowerCI = lower_ci_DISABILITYSCORE,
  UpperCI = upper_ci_DISABILITYSCORE
)

odds_ratio_table_DISABILITYSCORE

#ANOVA for age, weighted

aov.age.weighted<- aov(Age ~ migrationwave1name + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.age.weighted)
TukeyHSD(aov.age.weighted, which = 'migrationwave1name')

#ANOVA for CARESCORE weighted

aov.CARE.weighted<-aov(CARESCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.CARE.weighted)
TukeyHSD(aov.CARE.weighted, which = 'migrationwave1name')
#NOT SIGNIFICANT

#ANOVA for COGSCORE weighted

aov.COG.weighted<-aov(COGSCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.COG.weighted)
TukeyHSD(aov.COG.weighted, which = 'migrationwave1name')
#NOT SIGNIFICANT

#ANOVA for MOVESCORE weighted

aov.MOVE.weighted<-aov(MOVESCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.MOVE.weighted)
#NOT SIGNIFICANT

#ANOVA for OUTROLESCORE weighted

aov.OUTROLE.weighted<-aov(OUTROLESCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.OUTROLE.weighted)
TukeyHSD(aov.OUTROLE.weighted, which = 'migrationwave1name')

#ANOVA for SOCIALSCORE weighted

aov.SOCIAL.weighted<-aov(SOCIALSCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.SOCIAL.weighted)
TukeyHSD(aov.SOCIAL.weighted, which = 'migrationwave1name')

#ANOVA for DISABILITYSCORE weighted

aov.DIS.weighted<-aov(DISABILITYSCORE~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.DIS.weighted)
TukeyHSD(aov.DIS.weighted, which = 'migrationwave1name')

#Investigating medical discrimination
#Variable measured 1-5; None, a little, some, a lot, extreme

survey_design2 <- svydesign(ids = ~1, weights = ~NLAASWGT, data = cubanwaves)

cubanwaves$FD21 <-as.numeric(cubanwaves$FD21)

ggplot(cubanwaves, aes(x=FD21, fill = migrationwave1name)) + 
  geom_bar()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none")


table(cubanwaves$migrationwave1name, cubanwaves$FD21)

#Only 88 people in this dataset have a value for medical discrimination

#Medical discrimination, weighted

weighted_med_dis<-svyby(~FD21, ~migrationwave1name, design = survey_design2, FUN = svymean, na.rm = TRUE)

weighted_med_dis

weighted_med_dis_sd<-svyby(~FD21, ~migrationwave1name, design = survey_design2, FUN = svyvar, na.rm = TRUE)
weighted_med_dis_sd_2 <- sqrt(weighted_med_dis_sd$FD21)
weighted_med_dis_sd_2

ggplot(cubanwaves, aes(x=migrationwave1name, y=FD21, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") +
  geom_point(data=weighted_med_dis, aes(y=FD21), shape=20, size=5, color = "#3ab681")

#ANOVAs medical discrimination, weighted

aov.med.dis.weighted<- aov(FD21 ~ migrationwave1name+ Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.med.dis.weighted)
TukeyHSD(aov.med.dis.weighted, which = 'migrationwave1name')

#Everyday discrimination

#recoding to factor to be able to replace -9 and -8 values
cubanwaves$DS1a <-as.factor(cubanwaves$DS1a)
cubanwaves$DS1a <- revalue(cubanwaves$DS1a, c("-9" = "0","-8" = "0"))

cubanwaves$DS1b <-as.factor(cubanwaves$DS1b)
cubanwaves$DS1b <- revalue(cubanwaves$DS1b, c("-9" = "0","-8" = "0"))

cubanwaves$DS1c <-as.factor(cubanwaves$DS1c)
cubanwaves$DS1c <- revalue(cubanwaves$DS1c, c("-9" = "0","-8" = "0"))

cubanwaves$DS1d <-as.factor(cubanwaves$DS1d)
cubanwaves$DS1d <- revalue(cubanwaves$DS1d, c("-9" = "0","-8" = "0"))

cubanwaves$DS1e <-as.factor(cubanwaves$DS1e)
cubanwaves$DS1e <- revalue(cubanwaves$DS1e, c("-9" = "0","-8" = "0"))

cubanwaves$DS1f <-as.factor(cubanwaves$DS1f)
cubanwaves$DS1f <- revalue(cubanwaves$DS1f, c("-9" = "0","-8" = "0"))

cubanwaves$DS1g <-as.factor(cubanwaves$DS1g)
cubanwaves$DS1g <- revalue(cubanwaves$DS1g, c("-9" = "0","-8" = "0"))

cubanwaves$DS1h <-as.factor(cubanwaves$DS1h)
cubanwaves$DS1h <- revalue(cubanwaves$DS1h, c("-9" = "0","-8" = "0"))

cubanwaves$DS1i <-as.factor(cubanwaves$DS1i)
cubanwaves$DS1i<- revalue(cubanwaves$DS1i, c("-9" = "0","-8" = "0"))

#recoding back to numeric to be able to add them and make Everyday Discrimination Score (EDS)
cubanwaves$DS1a <-as.numeric(cubanwaves$DS1a)
cubanwaves$DS1b <-as.numeric(cubanwaves$DS1b)
cubanwaves$DS1c <-as.numeric(cubanwaves$DS1c)
cubanwaves$DS1d <-as.numeric(cubanwaves$DS1d)
cubanwaves$DS1e <-as.numeric(cubanwaves$DS1e)
cubanwaves$DS1f <-as.numeric(cubanwaves$DS1f)
cubanwaves$DS1g <-as.numeric(cubanwaves$DS1g)
cubanwaves$DS1h <-as.numeric(cubanwaves$DS1h)
cubanwaves$DS1i <-as.numeric(cubanwaves$DS1i)

#EDS Score
cubanwaves$EDS <- cubanwaves$DS1a + 
  cubanwaves$DS1b + 
  cubanwaves$DS1c + 
  cubanwaves$DS1d + 
  cubanwaves$DS1e + 
  cubanwaves$DS1f + 
  cubanwaves$DS1g + 
  cubanwaves$DS1h + 
  cubanwaves$DS1i

#EDS scores range from 9-54. higher score = less discrimination, lower score = more discrimination

#EDS by migration wave, weighted

survey_design2 <- svydesign(ids = ~1, weights = ~NLAASWGT, data = cubanwaves)
weighted_EDS<-svyby(~EDS, ~migrationwave1name, design = survey_design2, FUN = svymean, na.rm = TRUE)
weighted_EDS

weighted_EDS_sd<-svyby(~EDS, ~migrationwave1name, design = survey_design2, FUN = svyvar)
weighted_EDS_sd_2 <- sqrt(weighted_EDS_sd$EDS)
weighted_EDS_sd_2

ggplot(cubanwaves, aes(x=migrationwave1name, y=EDS, fill = migrationwave1name)) + 
  geom_boxplot()+ 
  scale_fill_manual(values = custom_colors)+
  theme_minimal() + theme(legend.position = "none") + ylim(0,60)+
  geom_point(data=weighted_EDS, aes(y=EDS), shape=20, size=5, color = "#3ab681")

#ANOVA of EDS by migration wave, weighted
aov.EDS.migration.weighted<-aov(EDS~migrationwave1name + Age + SEX + REGION, weights = NLAASWGT, data=cubanwaves)
summary(aov.EDS.migration.weighted)
TukeyHSD(aov.EDS.migration.weighted, which = 'migrationwave1name')







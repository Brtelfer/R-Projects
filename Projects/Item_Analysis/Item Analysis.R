package_list <- c("readxl", "car", "psych", "corrplot","devtools","tidyverse") #nolint
new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])] # nolint
if (length(new_packages)) install.packages(package_list)

invisible(lapply(package_list, library, character.only = TRUE))
Item_Analysis <- read_xlsx('/Users/brtelfer/Documents/Personal Stuff/R Projects/TM Item Analysis/Item_Analysis.xlsx')
Item.Analysis <- as.data.frame(Item_Analysis)

#Referent Goals reverse coding:
Item.Analysis$Q248 <- recode(Item.Analysis$Q248, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)

#Competittion Seeking reverse coding:
Item.Analysis$Q060 <- recode(Item.Analysis$Q060, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)
Item.Analysis$Q161 <- recode(Item.Analysis$Q161, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)
Item.Analysis$Q317 <- recode(Item.Analysis$Q317, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)
Item.Analysis$Q384 <- recode(Item.Analysis$Q384, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)
Item.Analysis$Q414 <- recode(Item.Analysis$Q414, `1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1, .default = NA_real_)

# Data Partritions
RFG <- Item.Analysis[ , c("Q001","Q014","Q022","Q035","Q079","Q083","Q084","Q187","Q198","Q248","Q281","Q293","Q294","Q315","Q327","Q336","Q341","Q377","Q387")]
CS <- Item.Analysis[ , c("Q060","Q104","Q156","Q161","Q202","Q246","Q317","Q384","Q395","Q414")]

# Strategy #1---------
#1a: Flag items with super high or low means, and tiny standard deviation
describe(RFG)
describe(CS)

#1b: 
# Cor
R_CS <- round(cor(CS, use = "complete.obs"), 3)
R_CS

R_RFG <- round(cor(RFG, use = "complete.obs"), 3)
R_RFG
# Heatmap
quartz()
corrplot(R_CS)
corrplot(R_RFG)

# Top/Bottom Cor
devtools::install_github("laresbernardo/lares")
library(lares)
dev.off()
quartz()
corr_cross(CS, max_pvalue = 0.05, top = 10)

v <- cor(CS)
cor_df <- as.data.frame(as.table(v))
names(cor_df) <- c("Var1", "Var2", "Correlation")
cor_df <- cor_df %>%
  filter(Var1 != Var2) %>%
  arrange(Correlation) %>%
  head(10)

cor_df

corr_cross(select(RFG), max_pvalue = 0.05, top = 10)

v <- cor(RFG)
cor_df <- as.data.frame(as.table(v))
names(cor_df) <- c("Var1", "Var2", "Correlation")
cor_df <- cor_df %>%
  filter(Var1 != Var2) %>%
  arrange(Correlation) %>%
  head(10)
cor_df

# Strategy #2---------
# Item-total cor

psych::alpha(RFG)
psych::alpha(CS)

# Strategy #3---------
CS_RFG <- cbind(CS, RFG)
parallel <-fa.parallel(CS_RFG, fa = "fa", cor = "cor", use = "pairwise")
parallel

EFA <-fa(CS_RFG, nfactors = 2, rotate = "promax", fm = "pa")
quartz()
fa.sort(EFA)

# Strategy #4---------
# Look at the means within each group
CS$sex <- Item.Analysis$sex
RFG$sex <- Item.Analysis$sex

describeBy(RFG, group = RFG$sex)
describeBy(CS, group = CS$sex)

# biserials
bicor <- apply(CS, 2, function(col)biserial(col, CS$sex))
bicor <- round((bicor),3)
bicor

bicor <- apply(RFG, 2, function(col)biserial(col, RFG$sex))
bicor <- round((bicor),3)
bicor

# Strategy #5---------
valid <- data.frame(Item.Analysis$GPA)
# Composite
valid$CS_Score <- rowMeans(CS)
valid$RFG_Score <- rowMeans(RFG)

# Corr between GPA & Composites
CS_cor <- corr.test(dplyr::select(valid, c('Item.Analysis.GPA', 'CS_Score')))
CS_cor

RFG_cor <- corr.test(dplyr::select(valid, c('Item.Analysis.GPA', 'RFG_Score')))
RFG_cor

corr.test(valid$Item.Analysis.GPA, CS)
corr.test(valid$Item.Analysis.GPA, RFG)
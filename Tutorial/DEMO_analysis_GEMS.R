# install required packages

# source required functions
source("R_functions/adjscores_A2024_v3.R")
source("R_functions/formula_transf_text.R")
source("R_functions/model_transf_text.R")
source("R_functions/transf_functions.R")
source("R_functions/tolLimits.obs.R")
source("R_functions/tolLimits.adjscores.R")
source("R_functions/ES.R")

# load required packages
require(effects)
require(car)

Test.dat = read.csv("Original_Data/GEMS_Dataset.csv", sep=",", dec=".")

# fix values for participants with zero Education otherwise some transformations (e.g. 1/x, log) could give inappropriate results
Test.dat[Test.dat$Education==0, "Education"] = 1

Test.dat = na.omit(Test.dat)

Test.ARC.res = adjscores_A2024(df = Test.dat, dep="Score", 
                             age="Age", edu="Education", sex="Sex",
                             dep.range = c(0, 100))

print(Test.ARC.res$model_text)

Test.ARC.lm.res = Test.ARC.res$lm.model
summary(Test.ARC.lm.res)


plot(allEffects(Test.ARC.lm.res, partial.residuals=T), residuals.cex=0.2)

AIC(Test.ARC.lm.res)

## calculate Equivalent Scores from Arcara2023 Method
Test.ES = ES(adjscores=Test.ARC.res$new.df$ADJ_SCORES)

print(Test.ES)



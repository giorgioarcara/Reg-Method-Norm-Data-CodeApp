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

# plot partial effects
plot(allEffects(Test.ARC.lm.res, partial.residuals=T), residuals.cex=0.2)

AIC(Test.ARC.lm.res)

## calculate Equivalent Scores from Arcara2023 Method
Test.ES = ES(adjscores=Test.ARC.res$new.df$ADJ_SCORES)

print(Test.ES)

dim(Test.dat)


# the following code include a comparison with code from Aiello as it is.
# NOTE: that the functions I used are based on Aiello, so this check only exclude some transcriptions issues.

tolLimits.obs(n=644)


#insert the sample size and the oTL to get the last Equivalent Scores (ESs); rounding controls are also provided
n <- 644
oTL <- 23
cd1 <- oTL/n
z1 <- qnorm(cd1)

z1_3 <- z1/3
z1_2 <- z1_3*2

cd2 <- pnorm(z1_2)
a <- (cd1-cd2)*n
a_r <- round(a)
ES1 <- -a_r+oTL
ES1

a
a_r

cd3 <- pnorm(z1_3)
b <- (cd3-cd2)*n
b_r <- round(b)
ES2 <- ES1+b_r
ES2

b
b_r

cd4 <- pnorm(0)
c <- (cd4-cd3)*n
c_r <- round(c)
ES3 <- ES2+c_r
ES3

c
c_r


Test.ARC.res$new.df$ADJ_SCORES[23]

### Check with different code
oTL == Test.ES$Observations[1]
ES1 == Test.ES$Observations[2]
ES2 == Test.ES$Observations[3]
ES3 == Test.ES$Observations[4]






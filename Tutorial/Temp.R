# launch this AFTER launching Tutorial script
# this is a temporary script to develop the adj_scores_transf_text function

source("R_functions/adj_scores_transf_text.R")
source("R_functions/adjscores_A2024_v3.R")


Test.ARC.res = adjscores_A2024(df = Test.dat, dep="Score", 
                               age="Age", edu="Education", sex="Sex",
                               dep.range = c(0, 100))


Age_mean = mean(Test.dat$Age)
Edu_mean=mean(Test.dat$Education) 
adj_scores_transf_text(mod = Test.ARC.res$lm.model, transfs=Test.ARC.res$transfs, var.names=c("Age", "Education"), dat=Test.dat, t.means=c(Age_mean, Edu_mean))

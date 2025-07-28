# interal function to transform a model into an formula for adj score (to improve readibility, e.g. "Observed score -(+ 0.8 * log(Age - 2.4) + 0.002 * quadr(Edu- 0.8) + 0.2 * Sex)")
# this function can be used with the output of regressions model in which a transformation has been used
# (e.g., in adjscores_C1987 (Capitani, 1987) or adjscores_A2024 (Arcara, 2024))
# NOTE: the use of "transf.names" are necessary steps because adjscore_C1987, adjscores_Arcara2023 use these transformations)

## mod  = a linear model
# transfs = the transformations OF THE MODEL, to be substituted with transf.names
# transfs.names = the name of age function in the function
# new.names = the new labels used to generate the formula.
# dat =  a dataset with the following columns , Age, Education, Sex, Score (see online ShinyApp for details).
# means = the means of TRANSFORMED predictors (e.g. mean(cube(age))) to be used for predictor centering in the formula.


# eg. 
#  mod = mod_final, transfs = lm.model$transfs, transfs.names=c("age_funct", "edu_funct"); new.names=c("Age", "Edu");
# example of the output  "12 + 0.8 * log(Age - 2.4) + 0.002 * quadr(Edu - 0.8) + 0.2 * Sex"

# Author Giorgio Arcara (2025) v.1.2 

adj_scores_transf_text = function(mod, transfs=NULL, transfs.names =NULL, new.names=NULL, dat=NULL, means = NULL, digits=3){
  
  
  coefs = coef(mod)
  model_res = NULL
  
  if (length(coefs)>1){
    
    for (iC in 1:length(coefs)){
      # case intercept (i.e. first term)
      if (iC == 1){
        model_res = paste(signif(as.numeric(coefs[iC]),digits=digits), "+", sep=" ")}    
      # case middle terms
      if (iC != 1 & iC != length(coefs)){
        model_res = paste(model_res, names(coefs)[iC], "*", 
                          signif(as.numeric(coefs[iC]),digits=digits), "+", sep=" ")}
      # case last terms
      if (iC == length(coefs)){
        model_res = paste(model_res, names(coefs)[iC], "*", 
                          signif(as.numeric(coefs[iC]),digits=digits), sep=" ")
      }
      
      for (iN in 1:length(transfs.names))
        model_res=gsub(transfs.names[iN], 
                       paste(transfs[iN], "(",new.names[iN], ")", sep=""), model_res)
    }
  }
  if (length(coefs)==1){
    model_res = paste(signif(as.numeric(coefs[1]),digits=digits), sep=" ")    
  }
  

  if (length(coefs)==1){
    model_res = "Adj score = Observed score" 
  }
  
  return(model_res)
}

# interal function to transform a model into an formula for adj score (to improve readibility, e.g. "Observed score -(+ 0.8 * log(Age - 2.4) + 0.002 * quadr(Edu- 0.8) + 0.2 * Sex)")
# this function can be used with the output of regressions model in which a transformation has been used
# (e.g., in adjscores_C1987 (Capitani, 1987) or adjscores_A2024 (Arcara, 2024))
# NOTE: the use of "transf.names" are necessary steps because adjscore_C1987, adjscores_A2024 use these transformations)

## mod  = a linear model
# transfs = the transformations OF THE MODEL, to be substituted with transf.names
# transfs.names = the name of age function in the function
# new.names = the new labels used to generate the formula.
# dat =  a dataset with the following columns , Age, Education, Sex, Score (see online ShinyApp for details).
# t.means = the means of TRANSFORMED predictors (e.g. mean(cube(age))) to be used for predictor centering in the formula.


# eg. 
#  mod = mod_final, transfs = lm.model$transfs, transfs.names=c("age_funct", "edu_funct"); new.names=c("Age", "Edu");
# example of the output  "12 + 0.8 * log(Age - 2.4) + 0.002 * quadr(Edu - 0.8) + 0.2 * Sex"

# Author Giorgio Arcara (2025) v.1.2 

adj_scores_transf_text = function(mod, transfs=NULL, transfs.names =NULL, var.names = NULL, new.names=NULL, sex.name="Sex", dat=NULL, t.means = NULL, digits=3){
  
  #cat("IMPORTANT: mean values must be provided BEFORE applying the required transformation\n")
  
  coefs = coef(mod)
  if (length(t.means)!=length(transfs)){
    stop("Error: the length of means for adjustments must be equal to the length of transfs")
  }
  
  
  #  if (length( grep(paste("^", sex.name, "$", sep=""), names(coefs))) > 0 ){
  #    t.means = c(t.means, 0.5)
  #  }
  
  
  t.means=round(t.means, digits=digits)
  
  model_res = NULL
  
  
  if (length(coefs)==1){
    model_res = "Adj score = Observed score" 
  }
  
  
  # proceed only if at least one coef is present   
  if (length(coefs)>1){
    
    
    # exclude Intercept from Coefs
    coefs = coefs[!names(coefs)%in% c("(Intercept)")]
    
    for (iC in 1:length(coefs)){
      
      # case first or middle terms
      if (iC != length(coefs)){
        
        
        model_res = paste(model_res,  signif(as.numeric(coefs[iC]),digits=digits), " * (", 
                          transfs[iC], "(", var.names[iC], ") - ", t.means[iC], ")", " + ", sep="")}
      # case last terms
      if (iC == length(coefs)){
        model_res = paste(model_res,  signif(as.numeric(coefs[iC]),digits=digits), " * (", 
                          transfs[iC], "(", var.names[iC], ") - ", t.means[iC], ")", sep="")}
      
      if (!is.null(new.names)){
        for (iN in 1:length(transfs.names))
          model_res=gsub(transfs.names[iN], 
                         paste(transfs[iN], "(",new.names[iN], ")", sep=""), model_res)
      }
    }
    model_res = paste("Adjusted score = Observed score - [ ", model_res, " ]", sep="")
    
    # trick to adjust formula (if +- is foudn, substitute with -)
    model_res = gsub("\\+ \\-", "\\-", model_res)
  }
  
  
  return(model_res)
}

# function to calculate Equivalent scores. 
# adapted rom Aiello e Depaoli (2022) 
# n = sample size
# adjscores = the adjusted scores as calculated from a regression based model.


# Author Giorgio Arcara (2023) v.1.0 , adapted from Aiello e Depaoli (2022)

ES <- function(n=NULL, adjscores=NULL, lower_tail = TRUE){
  
  if(length(n)>1){
    stop("n must have length 1")
  }
  
  if (!is.null(adjscores)){
    dat = data.frame(AS = adjscores)
    n = dim(dat)[1]
  }
  
  oTL = tolLimits.obs(n)$oTL
  
  if(oTL =="not defined"){
    stop("oTL is undefined: impossible to calculate ES")
  }
  
  cd1 <- oTL/n
  z1 <- qnorm(cd1)
  
  z1_3 <- z1/3
  z1_2 <- z1_3*2
  
  cd2 <- pnorm(z1_2, lower.tail = lower_tail)
  a <- (cd1-cd2)*n
  a_r <- round(a)
  ES1 <- -a_r+oTL
  
  
  cd3 <- pnorm(z1_3, lower.tail = lower_tail)
  b <- (cd3-cd2)*n
  b_r <- round(b)
  ES2 <- ES1+b_r
  
  
  cd4 <- pnorm(0, lower.tail = lower_tail)
  c <- (cd4-cd3)*n
  c_r <- round(c)
  ES3 <- ES2+c_r
  
  ES.n=c(oTL, ES1, ES2, ES3)
  names(ES.n)=c("ES0(oTL)-ES1", "ES1-ES2", "ES2-ES3", "ES3-ES4")
  
  if (!is.null(adjscores)){
    
    dat$ranked_AS = rank(dat$AS)
    
    if (!lower_tail){
      dat$ranked_AS = rev(dat$ranked_AS)
    }
    
    dat = dat[order(dat$AS), ]
    ES.s = unlist(dat[ES.n, "AS"])
    names(ES.n)=c("ES0(oTL)-ES1", "ES1-ES2", "ES2-ES3", "ES3-ES4")
    
    res = list(ES.n, ES.s)
    names(res) = c("Observations", "Adjusted_Scores")
    return(res)
    
  } else {
    
    res = ES.n
    
    return(res)
    
  }
  
}


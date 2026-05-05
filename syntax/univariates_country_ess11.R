for (c in levels(as.factor(ess$cntry))) {
#for (c in c("AT", "BE")) {

  ess.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess|>filter(cntry==c),nest=T)

  rslt.u<-  lapply(c(uvars[-length(uvars)]), function(v) vfreq(v, ess|>filter(cntry==c)))
  
  rslt.r <- lapply(c(rvars[-length(rvars)]), function(v) wfreq2(v, ess.s) )
  
    
  cat('### ', c, '\n')
  
  
  cat('::: {.panel-tabset}')
  
  cat('\n')
  cat('\n')
  
  
  for (i in 1:length(rslt.u)) {
    cat('### ', labs[[c(uvars,dvars)[i]]], '\n')
    
    cat(paste0("#### **Unweighted frequency table of unrecoded *",c(uvars,dvars)[i],"* **", '\n'))
    print(rslt.u[[i]])
    cat('\n')
    
    
    cat(paste0("#### **Weighted frequency table of recoded *", c(rvars,dvars)[i],"* **", '\n'))
    print(rslt.r[[i]])
    
    cat('\n')
  }
  
  cat('\n:::')
  cat('\n')
  
  
  
}

cat('\n::::')

ess.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess,nest=T)



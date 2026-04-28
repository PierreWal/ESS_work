for (c in levels(wvs$C_COW_NUM.f)) {

  wvs.s<-svydesign(~1,weights=~W_WEIGHT,data=wvs |> filter(C_COW_NUM.f==c)|> droplevels(),nest=T)
  
#  wvs.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess|>filter(cntry==c),nest=T)

  rslt.u<-  lapply(c(uvars[-length(uvars)]), function(v) vfreq(v, wvs|>filter(C_COW_NUM.f==c)))
  
  rslt.r <- lapply(c(rvars[-length(rvars)]), function(v) wfreq2(v, wvs.s) )
  
    
  cat('### ', c, '\n')
  
  
  cat('::: {.panel-tabset}')
  
  cat('\n')
  cat('\n')
  
  
  for (i in 1:length(rslt.u)) {
    cat('### ', labs[[c(uvars,dvars)[i]]], '\n')
    
    cat(paste0("#### **Unweighted frequencies**", '\n'))
    print(rslt.u[[i]])
    cat('\n')
    
    
    cat(paste0("#### **Weighted frequencies**", '\n'))
    print(rslt.r[[i]])
    
    cat('\n')
  }
  
  cat('\n:::')
  cat('\n')
  
  
  
}

cat('\n::::')

wvs.s<-svydesign(~C_COW_NUM,weights=~W_WEIGHT,data=wvs,nest=T)



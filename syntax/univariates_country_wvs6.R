for (c in levels(wvs$cow.fr)) {

  wvs.s<-svydesign(~1,
                   weights=~V258,
                   data=wvs |> 
                   filter(cow.f==c)|> 
                   droplevels(),
                   nest=T)
  
#  wvs.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess|>filter(cntry==c),nest=T)

  rslt.u<-  lapply(c(uvars6[-length(uvars6)]), 
                   function(v) vfreq(v, wvs|>
                    filter(cow.f==c)))
  
 rslt.r <- lapply(c(rvars6[-length(rvars6)]), function(v) wfreq2(v, wvs.s) )
  
    
  cat('### ', c, '\n')
  
  
  cat('::: {.panel-tabset}')
  
  cat('\n')
  cat('\n')
  
  
  for (i in 1:length(rslt.u)) {
    cat('### ', labs5[[c(uvars6,dvars6)[i]]], '\n')
    
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

wvs.s<-svydesign(~cow,weights=~V258,data=wvs,nest=T)



for (c in levels(wvs$COW.fr)) {

  wvs.s<-svydesign(~1,
                   weights=~V259,
                   data=wvs |> filter(COW.f==c)|> 
                   droplevels(),
                   nest=T)
  
#  wvs.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess|>filter(cntry==c),nest=T)

  rslt.u<-  lapply(c(uvars5[-length(uvars5)]), 
                   function(v) vfreq(v, wvs|>
                    filter(COW.f==c)))
  
  rslt.r <- lapply(c(rvars5[-length(rvars5)]), function(v) wfreq2(v, wvs.s) )
  
    
  cat('### ', c, '\n')
  
  
  cat('::: {.panel-tabset}')
  
  cat('\n')
  cat('\n')
  
  
  for (i in 1:length(rslt.u)) {
    cat('### ', labs5[[c(uvars5,dvars5)[i]]], '\n')
    
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

wvs.s<-svydesign(~COW,weights=~V259,data=wvs,nest=T)



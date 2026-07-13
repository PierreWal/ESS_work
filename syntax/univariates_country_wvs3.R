for (c in levels(wvs$COW.f)) {

  wvs.s<-svydesign(~1,
                   weights=~V236,
                   data=wvs |> filter(COW.f==c)|> 
                   droplevels(),
                   nest=T)
  
#  wvs.s<-svydesign(~psu,weights=~anweight,strata=~stratum,data=ess|>filter(cntry==c),nest=T)

  rslt.u<-  lapply(c(uvars3[-length(uvars3)]), 
                   function(v) vfreq(v, wvs|>
                    filter(COW.f==c)))
  
  rslt.r <- lapply(c(rvars3[-length(rvars3)]), function(v) wfreq2(v, wvs.s) )
  
    
  cat('### ', c, '\n')
  
  
  cat('::: {.panel-tabset}')
  
  cat('\n')
  cat('\n')
  
  
  for (i in 1:length(rslt.u)) {
    cat('### ', labs3[[c(uvars3,dvars3)[i]]], '\n')
    
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

wvs.s<-svydesign(~COW,weights=~V236,data=wvs,nest=T)



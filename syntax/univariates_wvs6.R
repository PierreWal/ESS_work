### Univariates analysis

wvs.s<-svydesign(~cow,weights=~V258,data=wvs,nest=T)

# Apply the function to all  variables 

rslt.u<-  lapply(c(uvars6), function(v) vfreq(v, wvs))

rslt.r <- lapply(c(rvars6), function(v) wfreq2(v, wvs.s) )


for (i in 1:length(rslt.u)) {
  cat('#### ', labs6[[c(uvars6,dvars6)[i]]], '\n')
  
  cat(paste0("##### **Unweighted frequencies  **", '\n'))
  print(rslt.u[[i]])
  cat('\n')
  
  
  cat(paste0("##### **Weighted frequencies  **", '\n'))
  print(rslt.r[[i]])
  
  cat('\n')
}

cat('\n:::\n')

cat('\n')
















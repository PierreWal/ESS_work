### Univariates analysis

wvs.s<-svydesign(~C_COW_NUM,weights=~W_WEIGHT,data=wvs,nest=T)

# Apply the function to all  variables 

rslt.u<-  lapply(c(uvars), function(v) vfreq(v, wvs))

rslt.r <- lapply(c(rvars), function(v) wfreq2(v, wvs.s) )


for (i in 1:length(rslt.u)) {
  cat('#### ', labs[[c(uvars,dvars)[i]]], '\n')
  
  cat(paste0("##### **Unweighted frequencies  **", '\n'))
  print(rslt.u[[i]])
  cat('\n')
  
  
  cat(paste0("##### **Weighted frequencies  **", '\n'))
  print(rslt.r[[i]])
  
  cat('\n')
}

cat('\n:::')
cat('\n')



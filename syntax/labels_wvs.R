wovars<-c("Q240","Q111")

wexvars<-c("Q260", "X003R", "Q275", "C_COW_NUM"
          ) 

bivars<-c("Q260", "X003R", "Q275"
) 

wivars<-c("W_WEIGHT")
uvars<-paste0(c(wovars,wexvars),".fr")
rvars<-paste0(c(wovars,wexvars),".fr")

dvars<-c("W_WEIGHT")

#bivars19<-c(exvars,dvars)



labs<-list(
  Q260.fr="Sex",
  X003R.fr="Age - banded",
  Q275.fr="Educational level",
  C_COW_NUM.fr="Country",
  Q240.fr="Left-Right scale",
  Q111.fr="Protecting environment vs. Economic growth"
)




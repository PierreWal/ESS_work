############################################################################################
## 
## 


wvs<-wvs|>mutate(
  V104.f=as_factor(V104, "both"),
  V104.fr=droplevels(as_factor(V104, "both")),
  V114.f=as_factor(V114, "both"),
  V114.fr=droplevels(as_factor(V114, "both")),
  V235.f=as_factor(V235, "both"),
  V235.fr=droplevels(as_factor(V235, "both")),
  V238.f=as_factor(V238, "both"),
  V238.f[is.na(V238.f)]<-"NA", 
  V238.fr=droplevels(as_factor(V238, "both")),
  V238.fr[is.na(V238.fr)]<-"NA",
  V237.f=as_factor(V237, "both"),
  V237.fr=as.factor(
  case_when(
  is.na(V237) ~ "NA",
  V237>=12 & V237<18 ~ "Age less than 18",
  V237>=18 & V237<26 ~ "18-25",
  V237>=26 & V237<37 ~ "26-36",
  V237>=37 & V237<48 ~ "37-47",
  V237>=48 & V237<59 ~ "48-58",
  V237>=59 & V237<70 ~ "59-69",
  V237>=70 & V237<121 ~ "More than 70")
),
  COW.f=as_factor(COW, "both"),
  COW.fr=droplevels(as_factor(COW, "both"))


  
  
  
  # AgeCat2.f=as.factor(case_when(
  #   agea>=50 & agea<70 ~ "50-69",
  #   agea>=70 ~ "70+"
  # )),
  #   AgeCat2.fr=as.factor(case_when(
  #   agea>=50 & agea<70 ~ "50-69",
  #   agea>=70 ~ "70+"
  # )),
  # AgeCat3.f=as.factor(case_when(
  #   agea>=50 & agea<70 ~ "50-69",
  #   agea>=70 & agea<80 ~ "70-79",
  #   agea>=80  ~ "80+"
  # )),
  # AgeCat3.fr=as.factor(case_when(
  #   agea>=50 & agea<70 ~ "50-69",
  #   agea>=70 & agea<80 ~ "70-79",
  #   agea>=80  ~ "80+"
  # )),
  #   maritalb.f=as_factor( maritalb,"both"),            
  # maritalb.fr=as.factor(case_when(
  #   maritalb==6  ~ "Single",
  #   maritalb==1 | maritalb==2 | rshpsts<=4 | rshpsgb<4 ~ "Married/SP",
  #   maritalb==3 | maritalb==4 ~ "Divorced/separated",
  #   maritalb==5  ~ "Widowed"
  # )),
  # mbtru.f=as_factor(mbtru,"both"),
  # mbtru.fr=as.factor(ifelse(!is.na(mbtru),mbtru,NA)),     
  #   eisced.f=as_factor( eisced,"both"),            
  # eisced.fr=as.factor(case_when(
  #   eisced==5 | eisced==6 | eisced==7 ~ "Further/Degree level",
  #   eisced==2 | eisced==3  | eisced==4 ~ "Secondary",
  #   eisced==0 | eisced==55~ "Other",
  #   eisced==1 ~ "Below secondary"
  # )),
  # hswrk.f=as_factor( hswrk,"both"),            
  # hswrk.fr=as.factor(case_when(
  #   hswrk==1  ~ "Did housework/cared for someone ",
  #   hswrk==0 ~ "Did not report"
  # )),
  # health.f=as_factor( health,"both"),
  # health.fr=as.factor(case_when(
  #   health==1  | health==2 ~ "Very good/good",
  #   health==3 ~ "Fair",
  #   health==4 | health==5  ~ "Poor"
  # )),
  # health2.f=as_factor( health,"both"),            
  # health2.fr=as.factor(case_when(
  #   health==1  | health==2 | health==3 ~ "Not bad",
  #   health==4 | health==5  ~ "Bad or very bad"
  # )),
  # hltphhc.f=as_factor( hltphhc,"both"),
  #   hltphhb.f=as_factor( hltphhb,"both"),
  # hltphbp.f=as_factor( hltphbp,"both"),
  #   hltphal.f=as_factor( hltphal,"both"),
  # hltphbn.f=as_factor( hltphbn,"both"),
  # hltphpa.f=as_factor( hltphpa,"both"),
  # hltphpf.f=as_factor( hltphpf,"both"),
  # hltphsd.f=as_factor( hltphsd,"both"),
  # hltphsc.f=as_factor( hltphsc,"both"),
  # hltphsh.f=as_factor(hltphsh,"both"),
  # hltphdi.f=as_factor(hltphdi,"both"),
  # hltphhc.fr=as_factor( hltphhc,"both"),
  # hltphhb.fr=as_factor( hltphhb,"both"),
  # hltphbp.fr=as_factor( hltphbp,"both"),
  # hltphal.fr=as_factor( hltphal,"both"),
  # hltphbn.fr=as_factor( hltphbn,"both"),
  # hltphpa.fr=as_factor( hltphpa,"both"),
  # hltphpf.fr=as_factor( hltphpf,"both"),
  # hltphsd.fr=as_factor( hltphsd,"both"),
  # hltphsc.fr=as_factor( hltphsc,"both"),
  # hltphsh.fr=as_factor(hltphsh,"both"),
  # hltphdi.fr=as_factor(hltphdi,"both"),
  # nrhltpb.f=hltphhc+  hltphhb+  hltphbp+  hltphal+  hltphbn+
  # hltphpa+  hltphpf+  hltphsd+  hltphsc+  hltphsh+ hltphdi,
  # nrhltpb.fr=as.factor(case_when(nrhltpb.f==0 ~"None",
  #                               nrhltpb.f==1 ~ "One" ,
  #                               nrhltpb.f==2 ~ "Two" ,
  #                               nrhltpb.f==3 | nrhltpb.f==4 ~ "3-4" ,
  #                               nrhltpb.f>=5 ~ "5+")) ,
  # tporgwk.f=as_factor(tporgwk,"both"),
  # tporgwk.fr=as.factor(case_when(
  #   tporgwk==1   ~ "Government (inc. local)",
  #   tporgwk==2 ~ "Other public",
  #   tporgwk==3 ~ "State-owned",
  #   tporgwk==4 ~ "Private sector",
  #   tporgwk==5 ~ "Self-employed",
  #   tporgwk==6 | tporgwk==7~ "Other/DNA"
  # )),
  # volunfp.f=as_factor(volunfp,"both"),
  # volunfp.fr=as.factor(ifelse(!is.na(volunfp),volunfp,NA)),     
  #   vote.f=as_factor(vote,"both"),
  #   vote.fr=as.ordered(case_when(
  #     vote==1   ~ "Voted",
  #     vote==2 | vote==3  ~ "Did not vote"
  # )),
  # polintr.f=as_factor(polintr,"both"),            
  # polintr.fr=as.factor(case_when(
  #   polintr==1  | polintr==2 ~ "Interested in politics",
  #   polintr==3 | polintr==4  ~ "Not interested in politics"
  # )),
  # netusoft.f=as_factor(netusoft,"both"),            
  # netusoft.fr=as.factor(case_when(
  #   netusoft==1   ~ "Never",
  #   netusoft==2 | netusoft==3  ~ "Occasionally/ a few times a week",
  #   netusoft==4 | netusoft==5  ~ "Most days/ every day"
  # )),
  # mnactic.f=as_factor(mnactic,"both"),            
  #     mnactic.fr=as.factor(case_when(
  #   mnactic==6  ~ "Retired (inc part)",
  #   mnactic==1 ~ "In employment",
  #   mnactic==8  ~ "FT carer",
  #   mnactic==5 ~ "LT sick/disabled",
  #   mnactic==2 | mnactic==3 | mnactic==4 |
  #     mnactic==7 | mnactic==9 ~ "Ec. inactive"
  # ))            
  
)            

for(vr in names(labs5[-19])){                        ### Leaving AgeCaat out
  attr(wvs[vr],"label")<-labs5[[vr]]
}

# levels(wvs$hincfel.fr)<-levels(as_factor(wvs$hincfel))[1:4]
#levels(wvs$mbtru.fr)<-levels(as_factor(wvs$mbtru))[1:3]
#levels(wvs$volunfp.fr)<-levels(as_factor(wvs$volunfp))[1:2]


# 
# ### correcting a missing value in the label of VotGE05
# 
# 
# for (hav in wvars  ) {
#   wvs<-cbind(wvs,tmp=tfactor(
#     eval(
#       parse(
#         text=paste0("wvs$",hav)))))
#   
#   names(wvs)[ncol(wvs)]<-paste0(hav,".f")
# }
# 
### Issue with value labels (2)
# levels(wvs$VotGE05.f)<-c("[-9] Not answered","[-1] Item not applicable","[0] No","[1] Yes")

# for(ov  in ovars) { ### Continuous vars exlo80 & exlo90 removed
#   #nv<-paste0(ov,".fr")
#   #ov<-paste0(v,".f")
#   wvs<-wvs|>mutate(nv=
#                          as.factor(eval(
#                            parse(
#                              text=paste0(  
#                                "ifelse(as.numeric(",ov,")<0 | as.numeric(",ov,")>80,NA,",
#                                ov,")"
#                              )
#                            )
#                          )
#                          )
#   )
#   names(wvs)[ncol(wvs)]<-paste0(ov,".fr")
# }




### Missing labels
# levels(wvs$scorgpo.fr)<-c("No", "Yes")
# levels(wvs$rlgatnd.fr)<-c("No", "Yes")
# levels(wvs$volunfp.fr)<-names(attr(wvs$volunfp, "labels")[1:2])
# levels(wvs$ethnic.fr)<-names(attr(wvs$ethnic, "labels")[3:4])
# levels(wvs$headlba.fr)<-names(attr(wvs$headlba, "labels")[4:5])
# levels(wvs$headlbe.fr)<-names(attr(wvs$headlbe, "labels")[4:5])
# levels(wvs$hobb.fr)<-names(attr(wvs$hobb, "labels")[4:5])
# levels(wvs$iafind.fr)<-names(attr(wvs$iafind, "labels")[4:7])
# levels(wvs$gndr.fr)<-names(attr(wvs$gndr, "labels")[1:2])
# #levels(wvs$happy.fr)<-names(attr(wvs$happy, "labels")[4:5])
# levels(wvs$pscede.fr)<-names(attr(wvs$pscede, "labels")[4:5])
# levels(wvs$spcar.fr)<-names(attr(wvs$spcar, "labels")[4:5])
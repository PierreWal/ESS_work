ovars<-c("ccnthum","ccrdprs","wrclmch","lrscale")

exvars<-c("gndr", "agegroup", "hincfel", "eisced","polintr"
          ) 
auxvars<-c("agea", "hhmmb","hltphhc", "hltphbp", "isco08", "rshpsts", "rshpsgb",
           "rshipa2","rshipa3","rshipa4","rshipa5","rshipa6","rshipa7",
           "rshipa8","rshipa9","rshipa10","rshipa11","rshipa12","rshipa13")

desvars<-c("psu","anweight","stratum")

dvars<-c("cntry")

uvars<-paste0(c(ovars,exvars,dvars),".f")
rvars<-paste0(c(ovars,exvars,dvars),".fr")

bivars19<-c(exvars,dvars)

regvars1<-c("AgeCat3.fr","gndr.fr","maritalb.fr","hhdtypb.fr", "eisced.fr","mnactic.fr")


labs<-list(
  #  ethnic.f="White vs Non-white",
#  fltlnl.f="Loneliness",
  gndr.f="Sex",
  agegroup.f="Age - banded",
  ccnthum.f="Climate change natural vs caused by humans",
  ccrdprs.f="Personal responsibility to act about  climate change",
  wrclmch.f="How worried about climate change",
  lrscale.f="Placement on left right scale",
  cntry.f="Country",
  eisced.f="Educational level",
#  happy.f="Happiness",
  # health.f="Self-reported general health V1",
  # health2.f="Self-reported general health V2",
  # hhdtypb.f="Recoded household type",
  hincfel.f="Subjective financial situation",
  # hltphhc.f="Heart or circulation problem",
  # hltphhb.f="High blood pressure",
  # hltphbp.f="Breathing problems",
  # hltphal.f="Allergies",
  # hltphbn.f="Back or neck pain",
  # hltphpa.f="Muscular or joint pain in hand or arm",
  # hltphpf.f="Muscular or joint pain in foot or leg",
  # hltphsd.f="Stomach or digestion related",
  # hltphsc.f="Skin condition related",
  # hltphsh.f="Severe headaches",
  # hltphdi.f="Diabetes",
  # hswrk.f="Whether doing house- or care work",
  isco1.f="Occupation, ISCO08 1 digit",
  # maritalb.f="Marital/relationship status",
  # mbtru.f="Trade union or similar membership",
  # mnactic.f= "Economic activity",
  # netusoft.f="Internet usage",
  # nrhltpb.f="Number of reported health problems",
  polintr.f="Interest for politics"
  # rlgatnd.f="Religious attendence",
  # sclmeet.f="Social life intensity",
  # tporgwk.f= "Type of organisation working/worked for",
  # volunfp.f="Whether volunteered",
  # vote.f="Whether voter at the last GE",
  # AgeCat2.fr="Recoded age, 2 category",
  # AgeCat3.fr="Recoded age, 3 category"
)


reglab1<-list(AgeCat3.fr="Age",
                              gndr.fr="Sex",
                              maritalb.fr="Marital/relationship status",
                              hhdtypb.fr="Household type",
                              eisced.fr="Highest qualification",
                              mnactic.fr="Economic activity"
              )

ivars<-c("nrhltpb.fr","health.fr","hincfel.fr", "fltlnl.fr", "happy.fr", "sclmeet.fr",
         "rlgatnd.fr",    "mbtru.fr"  , "hswrk.fr",  "tporgwk.fr", 
         "volunfp.fr",   "polintr.fr",  "netusoft.fr")


regvars2<-c(regvars1[-7],ivars)
reglab2<-list(AgeCat3.fr="Age",
              gndr.fr="Sex",
              maritalb.fr="Marital/relationship status",
              hhdtypb.fr="Household type",
              eisced.fr="Highest qualification",
              mnactic.fr="Economic activity",
              nrhltpb.fr="Nr of health problems",
              health.fr="Self-rated health",
              hincfel.fr="Subj. financial situation",
              fltlnl.fr="Feels lonely?",
              happy.fr="Feels happy?",
              sclmeet.fr="Meet friends/relatives?",
              rlgatnd.fr="Religious attendence",    
              mbtru.fr="Trade-union etc member?", 
              hswrk.fr="Whether does caring/housework?",  
              tporgwk.fr="Organisation works/worked for", 
              volunfp.fr="Whether volunteers",   
              polintr.fr="Interest in politics",  
              netusoft.fr="Internet usage")

              
regvars3<-c(regvars1[-7],"AgeCat3.fr:gndr.fr",ivars)
reglab3<-list(AgeCat3.fr="Age",
              "AgeCat3.fr:gndr.fr"="Age by sex", 
              gndr.fr="Sex",
              maritalb.fr="Marital/relationship status",
              hhdtypb.fr="Household type",
              eisced.fr="Highest qualification",
              mnactic.fr="Economic activity",
              nrhltpb.fr="Nr of health problems",
              health.fr="Self-rated health",
              hincfel.fr="Subj. financial situation",
              fltlnl.fr="Feels lonely?",
              happy.fr="Feels happy?",
              sclmeet.fr="Meet friends/relatives?",
              rlgatnd.fr="Religious attendence",    
              mbtru.fr="Trade-union etc member?", 
              hswrk.fr="Whether does caring/housework?",  
              tporgwk.fr="Organisation works/worked for", 
              volunfp.fr="Whether volunteers",   
              polintr.fr="Interest in politics",  
              netusoft.fr="Internet usage"
              )

regvars4<-c(regvars1[-7],"AgeCat3.fr:gndr.fr")
reglab4<-list(AgeCat3.fr="Age",
              "AgeCat3.fr:gndr.fr"="Age by sex", 
              gndr.fr="Sex",
              maritalb.fr="Marital/relationship status",
              hhdtypb.fr="Household type",
              eisced.fr="Highest qualification",
              mnactic.fr="Economic activity"
)







# wvars<-c("erfvol", "ethnic", "headlba","headlbe", "hobb", "iafind","indsex", "pscedd", "pscede", "scorgpo","scorgrl", "spcar",  "VotGE01","VotGE05","VotGE15","VotGE17","VotGE19")    




# 
# 
#       
# reglab2<-list(hobb.fr="Internet connection?",
#                               pscede.fr="Felt lonely", 
#                               hehelf3.fr="Self-rated general health", 
#                               iafcon.fr="How getting along financially",
#                               cfmetm3.fr="Self-rated mental health", 
#                               pscedd.fr="Happy last week?",
#                               sclfcoa.fr="Give back to community?", 
#                               erlvolpe.fr= "Unpaid carer",
#                               AgeGap.r="Perceived/actual age gap (cont)",
#                               AgeGap31.fr="Perceived/actual age gap (exact)",
#                               AgeGap32.fr="Perceived/actual age gap (+-3 years)",
#                               VotRec2.fr="Past GE turnout")
# 
# sdvars1<-c("AgeCat3.fr","indsex.fr","dimarr.fr","edqual.fr","wpdes.fr")
# sdvars2<-c("AgeCat3.fr","indsex.fr","dimarr.fr","edqual.fr","wpdes.fr","tenureb.fr")
# ivars<-c("hobb.fr","pscede.fr","hehelf3.fr","iafcon.fr","cfmetm3.fr","pscedd.fr","sclfcoa.fr","erlvolpe.fr","AgeGap32.fr")
# 
# regvars3<-c("VotRec2.fr",sdvars1,ivars)
# regvars4<-c("VotRec2.fr",sdvars2,ivars)
# 
# regvars5<-c(sdvars1)
# regvars6<-c(sdvars2)
# regvars7<-c(sdvars1,ivars)
# regvars8<-c(sdvars2,ivars)

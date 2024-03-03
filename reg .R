library(fixest)
library(data.table)
library(dplyr)

data <- read.csv('switch4ref2.csv')

data <- data %>%
  mutate(
    P_Patent = as.numeric(Patent_Count > 0),
    P_NCT = as.numeric(NCT_Count > 0),
    P_NEWS = as.numeric(NCT_Count > 0),
    P_TWEETS = as.numeric(NCT_Count > 0),
    Da = NDC / (NDC+NCC),
    D = as.numeric(Disruption > 0),
    D5 = as.numeric(Disruption5 > 0),
    D10 = as.numeric(Disruption10 > 0),
    N = as.numeric(Atyp_Median_Z < 0),
    E = EC / Citation_Count
  )

colnames(data)

reg1<-feols(Disruption~switchScore, data, se = "hetero")
reg2<-feols(Disruption~switchScore|Year, data, se = "hetero")
reg3<-feols(Disruption~switchScore|Year+FieldID, data, se = "hetero")
reg4<-feols(Disruption~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data, se = "hetero")
reg5<-feols(Disruption~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data, se = "hetero")
reg6<-feols(Disruption~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data)
reg7<-feols(Disruption~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data)
reg8<-feols(Disruption~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data)
reg9<- feglm(D~switchScore, data,family = "binomial", se = "hetero")
reg10<-feglm(D~switchScore|Year, data,family = "binomial", se = "hetero")
reg11<-feglm(D~switchScore|Year+FieldID, data,family = "binomial", se = "hetero")
reg12<-feglm(D~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg13<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data,family = "binomial", se = "hetero")
reg14<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data,family = "binomial")
reg15<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data,family = "binomial")
reg16<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data,family = "binomial")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16) , "F:/A J YANG PAPER/Working paper (under review)/NHB switch and disruption/Tables/reg11.csv")

reg1<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+FieldID+JournalID, data,split=~Year,family = "binomial")
write.csv(etable(reg1) , "F:/A J YANG PAPER/Working paper (under review)/NHB switch and disruption/Tables/reg12.csv")

reg1<-feglm(D~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+JournalID, data,split=~FieldID,family = "binomial")
write.csv(etable(reg1) , "F:/A J YANG PAPER/Working paper (under review)/NHB switch and disruption/Tables/reg13.csv")

reg1<-feols(log(Citation_Count+1)~switchScore, data, se = "hetero")
reg2<-feols(log(Citation_Count+1)~switchScore|Year, data, se = "hetero")
reg3<-feols(log(Citation_Count+1)~switchScore|Year+FieldID, data, se = "hetero")
reg4<-feols(log(Citation_Count+1)~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data, se = "hetero")
reg5<-feols(log(Citation_Count+1)~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data, se = "hetero")
reg6<-feols(log(Citation_Count+1)~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data)
reg7<-feols(log(Citation_Count+1)~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data)
reg8<-feols(log(Citation_Count+1)~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data)
reg9<- feglm(Hits5~switchScore, data,family = "binomial", se = "hetero")
reg10<-feglm(Hits5~switchScore|Year, data,family = "binomial", se = "hetero")
reg11<-feglm(Hits5~switchScore|Year+FieldID, data,family = "binomial", se = "hetero")
reg12<-feglm(Hits5~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg13<-feglm(Hits5~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data,family = "binomial", se = "hetero")
reg14<-feglm(Hits5~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data,family = "binomial")
reg15<-feglm(Hits5~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data,family = "binomial")
reg16<-feglm(Hits5~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data,family = "binomial")
reg17<-feols(E~switchScore, data, se = "hetero")
reg18<-feols(E~switchScore|Year, data, se = "hetero")
reg19<-feols(E~switchScore|Year+FieldID, data, se = "hetero")
reg20<-feols(E~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data, se = "hetero")
reg21<-feols(E~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data, se = "hetero")
reg22<-feols(E~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data)
reg23<-feols(E~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data)
reg24<-feols(E~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data)
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16,reg17,reg18,reg19,reg20,reg21,reg22,reg23,reg24) , "F:/A J YANG PAPER/Working paper (under review)/NHB switch and disruption/Tables/reg2.csv")

reg1<-feols(RaoStirling_Ref~switchScore, data, se = "hetero")
reg2<-feols(RaoStirling_Ref~switchScore|Year, data, se = "hetero")
reg3<-feols(RaoStirling_Ref~switchScore|Year+FieldID, data, se = "hetero")
reg4<-feols(RaoStirling_Ref~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data, se = "hetero")
reg5<-feols(RaoStirling_Ref~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data, se = "hetero")
reg6<-feols(RaoStirling_Ref~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data)
reg7<-feols(RaoStirling_Ref~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data)
reg8<-feols(RaoStirling_Ref~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data)
reg9<- feglm(N~switchScore, data,family = "binomial", se = "hetero")
reg10<-feglm(N~switchScore|Year, data,family = "binomial", se = "hetero")
reg11<-feglm(N~switchScore|Year+FieldID, data,family = "binomial", se = "hetero")
reg12<-feglm(N~switchScore+Funding+log(Reference_Count+1)|Year+FieldID, data,family = "binomial", se = "hetero")
reg13<-feglm(N~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|Year+FieldID, data,family = "binomial", se = "hetero")
reg14<-feglm(N~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam|AuthorID+Year+FieldID, data,family = "binomial")
reg15<-feglm(N~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID, data,family = "binomial")
reg16<-feglm(N~switchScore+Funding+log(Reference_Count+1)+log(Team_Size)+International+InterDisTeam+log(Age+1)+FocalField+log(Past_Pub+1)+log(H.index+1)|AuthorID+Year+FieldID+JournalID, data,family = "binomial")
write.csv(etable(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16) , "F:/A J YANG PAPER/Working paper (under review)/NHB switch and disruption/Tables/reg3.csv")
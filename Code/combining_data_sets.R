# load libraries
library(data.table)

#load individual data sets
ml<-fread('GitHub/GiveMercuryAMoon/Data/Mercury_ML_20181105.csv')
weather<-fread('GitHub/GiveMercuryAMoon/Data/Egypt_historical_weather.csv')
holidays_weekends<-fread('GitHub/GiveMercuryAMoon/Data/weekends_and_holidays.csv')


# lag for time series: 16 days of lag
ml<-ml[,list(DATE,
             CU.Count,MA.Count,GDELT.Doc.Count,Pos.Tone,Neg.Tone,
             CU.Count.t1=shift(CU.Count,n = 1),
             MA.Count.t1=shift(MA.Count,n = 1),
             GDELT.Doc.Count.t1 = shift(GDELT.Doc.Count,n=1),
             Pos.Tone.t1 = shift(Pos.Tone,n=1),
             Neg.Tone.t1 = shift(Neg.Tone,n=1),
             
             CU.Count.t2=shift(CU.Count,n = 2),
             MA.Count.t2=shift(MA.Count,n = 2),
             GDELT.Doc.Count.t2 = shift(GDELT.Doc.Count,n=2),
             Pos.Tone.t2 = shift(Pos.Tone,n=2),
             Neg.Tone.t2 = shift(Neg.Tone,n=2),
             
             CU.Count.t3=shift(CU.Count,n = 3),
             MA.Count.t3=shift(MA.Count,n = 3),
             GDELT.Doc.Count.t3 = shift(GDELT.Doc.Count,n=3),
             Pos.Tone.t3 = shift(Pos.Tone,n=3),
             Neg.Tone.t3 = shift(Neg.Tone,n=3),
             
             CU.Count.t4=shift(CU.Count,n = 4),
             MA.Count.t4=shift(MA.Count,n = 4),
             GDELT.Doc.Count.t4 = shift(GDELT.Doc.Count,n=4),
             Pos.Tone.t4 = shift(Pos.Tone,n=4),
             Neg.Tone.t4 = shift(Neg.Tone,n=4),
             
             CU.Count.t5=shift(CU.Count,n = 5),
             MA.Count.t5=shift(MA.Count,n = 5),
             GDELT.Doc.Count.t5 = shift(GDELT.Doc.Count,n=5),
             Pos.Tone.t5 = shift(Pos.Tone,n=5),
             Neg.Tone.t5 = shift(Neg.Tone,n=5),
             
             CU.Count.t6=shift(CU.Count,n = 6),
             MA.Count.t6=shift(MA.Count,n = 6),
             GDELT.Doc.Count.t6 = shift(GDELT.Doc.Count,n=6),
             Pos.Tone.t6 = shift(Pos.Tone,n=6),
             Neg.Tone.t6 = shift(Neg.Tone,n=6),
             
             CU.Count.t7=shift(CU.Count,n = 7),
             MA.Count.t7=shift(MA.Count,n = 7),
             GDELT.Doc.Count.t7 = shift(GDELT.Doc.Count,n=7),
             Pos.Tone.t7 = shift(Pos.Tone,n=7),
             Neg.Tone.t7 = shift(Neg.Tone,n=7),
             
             CU.Count.t8=shift(CU.Count,n = 8),
             MA.Count.t8=shift(MA.Count,n = 8),
             GDELT.Doc.Count.t8 = shift(GDELT.Doc.Count,n=8),
             Pos.Tone.t8 = shift(Pos.Tone,n=8),
             Neg.Tone.t8 = shift(Neg.Tone,n=8),
             
             CU.Count.t9=shift(CU.Count,n = 9),
             MA.Count.t9=shift(MA.Count,n = 9),
             GDELT.Doc.Count.t9 = shift(GDELT.Doc.Count,n=9),
             Pos.Tone.t9 = shift(Pos.Tone,n=9),
             Neg.Tone.t9 = shift(Neg.Tone,n=9),
             
             CU.Count.t10=shift(CU.Count,n = 10),
             MA.Count.t10=shift(MA.Count,n = 10),
             GDELT.Doc.Count.t10 = shift(GDELT.Doc.Count,n=10),
             Pos.Tone.t10 = shift(Pos.Tone,n=10),
             Neg.Tone.t10 = shift(Neg.Tone,n=10),
             
             CU.Count.t11=shift(CU.Count,n = 11),
             MA.Count.t11=shift(MA.Count,n = 11),
             GDELT.Doc.Count.t11 = shift(GDELT.Doc.Count,n=11),
             Pos.Tone.t11 = shift(Pos.Tone,n=11),
             Neg.Tone.t11 = shift(Neg.Tone,n=11),
             
             CU.Count.t12=shift(CU.Count,n = 12),
             MA.Count.t12=shift(MA.Count,n = 12),
             GDELT.Doc.Count.t12 = shift(GDELT.Doc.Count,n=12),
             Pos.Tone.t12 = shift(Pos.Tone,n=12),
             Neg.Tone.t12 = shift(Neg.Tone,n=12),
             
             CU.Count.t13=shift(CU.Count,n = 13),
             MA.Count.t13=shift(MA.Count,n = 13),
             GDELT.Doc.Count.t13 = shift(GDELT.Doc.Count,n=13),
             Pos.Tone.t13 = shift(Pos.Tone,n=13),
             Neg.Tone.t13 = shift(Neg.Tone,n=13),
             
             CU.Count.t14=shift(CU.Count,n = 14),
             MA.Count.t14=shift(MA.Count,n = 14),
             GDELT.Doc.Count.t14 = shift(GDELT.Doc.Count,n=14),
             Pos.Tone.t14 = shift(Pos.Tone,n=14),
             Neg.Tone.t14 = shift(Neg.Tone,n=14),
             
             CU.Count.t15=shift(CU.Count,n = 15),
             MA.Count.t15=shift(MA.Count,n = 15),
             GDELT.Doc.Count.t15 = shift(GDELT.Doc.Count,n=15),
             Pos.Tone.t15 = shift(Pos.Tone,n=15),
             Neg.Tone.t15 = shift(Neg.Tone,n=15),
             
             CU.Count.t16=shift(CU.Count,n = 16),
             MA.Count.t16=shift(MA.Count,n = 16),
             GDELT.Doc.Count.t16 = shift(GDELT.Doc.Count,n=16),
             Pos.Tone.t16 = shift(Pos.Tone,n=16),
             Neg.Tone.t16 = shift(Neg.Tone,n=16)
)]

#attach weather
ml<-merge(ml,weather,by.x=c('DATE'),by.y=c('dt'),all.x=T)
#attach date info
ml<-merge(ml,holidays_weekends,by.x=c('DATE'),by.y=c('Date'),all.x=T)

# fwrite(ml,'GitHub/GiveMercuryAMoon/Data/Mercury_ML_20181201.csv')

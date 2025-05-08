d1=read.table("data/student-mat.csv",sep=";",header=TRUE)
# rename the G1, G2, G3 columns to G1_math, G2_math, G3_math
colnames(d1)[colnames(d1)=="G1"] <- "G1_math"
colnames(d1)[colnames(d1)=="G2"] <- "G2_math"
colnames(d1)[colnames(d1)=="G3"] <- "G3_math"

d2=read.table("data/student-por.csv",sep=";",header=TRUE)
# rename the G1, G2, G3 columns to G1_por, G2_por, G3_por
colnames(d2)[colnames(d2)=="G1"] <- "G1_por"
colnames(d2)[colnames(d2)=="G2"] <- "G2_por"
colnames(d2)[colnames(d2)=="G3"] <- "G3_por"


col_interested = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                    "guardian", "traveltime", "schoolsup", "famsup", 
                    "activities", "higher", "romantic", "famrel", "freetime", 
                    "goout", "Dalc", "Walc", "health")

# keep only the columns of interest and the columns containing the grades
d1 = d1[,c(col_interested,"G1_math","G2_math","G3_math")]
d2 = d2[,c(col_interested,"G1_por","G2_por","G3_por")]
# merge the two dataframes by the columns of interest
d3 = merge(d1,d2,by=col_interested)

write.csv(d3,"data/student-merge.csv",row.names=FALSE)

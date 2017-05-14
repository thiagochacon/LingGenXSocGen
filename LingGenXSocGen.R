setwd("/Users/sarahpeoples/Documents/PhD_Orga/Conferences_Workshops_Presentations/QMSS_2017/LingGenXSocGen/")
setwd("C:/Users/jn/OneDrive/Courses/QMSS 2017/LingGenXSocGen/")

# Read in data files
socgen= read.csv(file="dplace-societies-2017-05-09_socialgender.csv", header=T, skip = 1, encoding = "UTF-8")
str(socgen)

descent= read.csv(file="dplace-societies-2017-05-12_descent.csv", header=T, skip = 1, encoding = "UTF-8")
#Glottolog code  socgen $ Glottolog.language.dialect.id 
str(descent)

# # option 1: (can only be used when you are absolutely SURE that the rows are in exactly the same order in both data frames!!) create a copy of the data, create a variable of the code from descent, then add the descent code to the new data
# new.data= socgen
# descentcode=descent$Code..EA043.Descent..major.type
# new.data$descentcode <- descentcode 
# str(new.data)

# option 2: merge d-place data
merge.data= merge(socgen, descent, by="Glottolog.language.dialect.id")
str(merge.data)
clean.data= data.frame(merge.data$Glottolog.language.dialect.id, merge.data$Preferred.society.name.x, merge.data$ Society.id.x, merge.data$ Language.family.x  , merge.data$ Code..EA050.Sex.differences..gathering, merge.data$ Code..EA052.Sex.Differences..fishing, merge.data$ Code..EA054.Sex.differences..agriculture, merge.data$ Code..EA051.Sex.differences..hunting, merge.data$ Revised.latitude.x, merge.data$ Revised.longitude.x, merge.data$ Code..EA043.Descent..major.type)
str(clean.data)

# merge d-place data and grambank data
grambank= read.table("gb51_fixed.txt", sep='\t', stringsAsFactors=F, header=T, encoding = "UTF-8")
str(grambank)
clean.gdata = data.frame(grambank$Glottolog, grambank$Gender_Value)
# names.gdata=c("Glottolog.language.dialect.id",  "GB_Feature", "gramgender")
# names(clean.gdata)= names.gdata
str(clean.gdata)

clean.data = merge(clean.data, clean.gdata, by.x= "merge.data.Glottolog.language.dialect.id", by.y="grambank.Glottolog")
str(clean.data)


### Step 1: look at gender in society and language
hist(clean.data$grambank.Gender_Value)
hist(clean.data $ merge.data.Code..EA050.Sex.differences..gathering)
hist(clean.data $ merge.data.Code..EA051.Sex.differences..hunting)
hist(clean.data $ merge.data.Code..EA054.Sex.differences..agriculture)
hist(clean.data$ merge.data.Code..EA052.Sex.Differences..fishing)

### Step 2: in D-Place, 1&2 should be coded as 1, 3&4 as 0, 5&6 as 1
data_wo_descent<-clean.data[,-which(names(clean.data)=="merge.data.Code..EA043.Descent..major.type")]

recode = function(x){
  y = rep(0,length(x))
  y[x %in% c(1,2,5,6)] = 2
  y[x %in% c(3)] = 1
  y[x %in% c(4,7)] = 0
  y[x %in% c(8,9)] = NA
  y
}

#recode = function(x){
#  y = rep(0,length(x))
#  y[x %in% c(1,2,5,6)] = 1
#  y[x %in% c(3,4,7)] = 0
#  y[x %in% c(8,9)] = NA
#  y
#}

data_wo_descent$gathering = recode(data_wo_descent$merge.data.Code..EA050.Sex.differences..gathering)
data_wo_descent$fishing = recode(data_wo_descent$merge.data.Code..EA052.Sex.Differences..fishing)
data_wo_descent$agriculture = recode(data_wo_descent$merge.data.Code..EA054.Sex.differences..agriculture)
data_wo_descent$hunting = recode(data_wo_descent$merge.data.Code..EA051.Sex.differences..hunting)

hist(data_wo_descent$gathering)
hist(data_wo_descent$fishing)
hist(data_wo_descent$agriculture)
hist(data_wo_descent$hunting)

boxplot(data_wo_descent$grambank.Gender_Value, data_wo_descent$hunting)
boxplot(data_wo_descent$grambank.Gender_Value, data_wo_descent$gathering)
boxplot(data_wo_descent$grambank.Gender_Value, data_wo_descent$fishing)

data_wo_descent$soc_gen_score = rowSums(data_wo_descent[,c("gathering","fishing","agriculture","hunting")], na.rm = T)

plot(data_wo_descent$grambank.Gender_Value, data_wo_descent$soc_gen_score)
boxplot(data_wo_descent$grambank.Gender_Value, data_wo_descent$soc_gen_score)
table(data_wo_descent$grambank.Gender_Value)

# boxplot suggests that languages without sex-based gender, have lower social gender score (sex-based distinctions) and vice versa
# greater variation in languages with sex-based gender
# consistent with general hypothesis that language mirrors society, but society changes faster than language

#families: Austronesian, Atlantic-Congo, Afro-Asiatic, Arawakan

# mixed effects model: with geography, lang family as random effects and social score as predictor variable
# look at lgs with gender but with low social gender score
# coding of Social Gender Score (SGS) = 0 means society does not have gender segregation in an acivity; 1 = neutral; 2 = social roles well defined based on gender

library(maps)
#language sample
languages = unique(data_wo_descent$merge.data.Preferred.society.name.x)
colours = rainbow(length(languages))
map()
points(data_wo_descent$merge.data.Revised.longitude.x, data_wo_descent$merge.data.Revised.latitude.x, col = colours, pch=16, ncol=2)

#language families
lang_fam = unique(data_wo_descent$merge.data.Language.family.x)
colours = rainbow(length(lang_fam))
map()
points(data_wo_descent$merge.data.Revised.longitude.x, data_wo_descent$merge.data.Revised.latitude.x, col = colours, pch=16, ncol=2)
legend(-45,-40, legend=lang_fam, col=colours, pch=16, ncol=2)

# sex-based gender
gender = unique(data_wo_descent$grambank.Gender_Value)
colours = rainbow(length(gender))
map()
points(data_wo_descent$merge.data.Revised.longitude.x, data_wo_descent$merge.data.Revised.latitude.x, col = colours, pch=16, ncol=2)
legend(-45,-40, legend=gender, col=colours, pch=16, ncol=2)

##############################################################################
#################### Classification of work Types ############################

# Notes
# Kuokota karafuu -> scramble competitiion. 
# Kupamba harusi <- wedding Planner
# Chikichi <- making coconut oil - used for cooking


###############################################################################
################### CATEGROIZING SELF EMPLOYMENT###############################

if(MAKE!= TRUE) source('code/script/process_scripts.R')

names(d)
d$self$main<-tolower(d$self$main)
sort(table(d$self$main))
v<-d$self$main
sort(table(v))
length(table(v))


# Kokoto 

v[which(grepl("kokoto", v))] <- "kokoto"

# Chikichi
v[which(grepl("chik", v))] <- "chikichi"

# Mwani
table(v[which(grepl("mwani", v))])
v[which(grepl("kununuwa mwani", v))] <- "biashara ya mwani" 
v[which(grepl("kuokota mwani", v))] <- "mwani"
v[which(grepl("kupanda mwani", v))] <- "mwani"

# mali kachara
table(v[which(grepl("kachar", v))])
v[which(grepl("kachari", v))] <- "mali kachara" 
v[which(grepl("kachara", v))] <- "mali kachara" 
v[which(grepl("kununuwa chuma chakavu", v))] <- "mali kachara" 

# Agricultural Business

# Makuti
table(v[which(grepl("makuti", v))])
v[which(grepl("makuti", v))] <- "makuti" 
v[which(grepl("kupiga nakuti", v))] <- "makuti" 
v[which(grepl("kusuka makutu", v))] <- "makuti" 

# Karafu
table(v[which(grepl("karafu", v))])
v[which(grepl("kukota karafuu", v))] <- "kuokota karafuu" 
v[which(grepl("kuwokota karafuu", v))] <- "kuokota karafuu" 
v[which(grepl("kuokota_karafuu", v))] <- "kuokota karafuu" 
v[which(grepl("kuchuma karafuu", v))] <- "kuokota karafuu" 
v[which(grepl("kuokota  karafuu", v))] <- "kuokota karafuu" 
v[grepl("karafu", d$self$main)& d$self$main != "ananunuwa karafu na kuuza zst"] <- "kibarua ya karafuu" 



# Mafuta na nguo
v[which(grepl("kuuza mafuta ya nywele", v))] <- "mafuta na nguo" 
v[which(grepl("kuuza nguo na mafuta nywele", v))] <- "mafuta na nguo" 
v[which(grepl("kuuza nguo na mafuta ya nywele", v))] <- "mafuta na nguo" 
v[which(grepl("mafuta ya nywele", v))] <- "mafuta na nguo" 
v[which(grepl("kuuza kanga na mafuta ya nyele", v))] <- "mafuta na nguo" 
v[which(grepl("kuuza nguo na vipodozi", v))] <- "mafuta na nguo" 
v[which(grepl("kunengeza na vipodozi", v))] <- "mafuta na nguo" 

# Kukata Nywele
v[which(grepl("kusuka nywele", v))] <- "kukata nywele" 


# Safarisha Bithaa
v[which(grepl("kusafirisha mizigo kwa mashua", v))] <- "usafiri ya bithaa" 
v[which(grepl("usafirishaji wa mizigo", v))] <- "usafiri ya bithaa" 
v[which(grepl("kusafirisha mizigo kwa jahazi", v))] <- "usafiri ya bithaa" 
v[which(grepl("kusafirisha nguo tanga", v))] <- "usafiri ya bithaa" 
v[which(grepl("kubeba mizigo kwa gari ya ng'ombe", v))] <- "usafiri ya bithaa" 
v[which(grepl("kubeba matofali kwa gari ya ng'ombe", v))] <- "usafiri ya bithaa" 
v[which(grepl("kusafirisha nguo tanga", v))] <- "usafiri ya bithaa" 
v[which(grepl("kununua, kusafirisha, na kuuza bitha", v))] <- "usafiri ya bithaa" 

# udalali
v[which(grepl("dalali", v))] <- "udalali" 

# Selling crops
v[which(grepl("kuuza tungule", v))] <- "kuuza mazao" 
v[which(grepl("kuuza nazi", v))] <- "kuuza mazao" 
v[which(grepl("kushindia viazi", v))] <- "kuuza mazao" 

# Small Crafts
v[which(grepl("utembo", v))] <- "kutengenza vitu vidogo vidogo" 
v[which(grepl("mikeka na mikoba", v))] <- "kutengenza vitu vidogo vidogo"
v[which(grepl("kutengeza kamba", v))] <- "kutengenza vitu vidogo vidogo"
v[which(grepl("kutengeza kamba", v))] <- "kutengenza vitu vidogo vidogo"
v[which(grepl("kushona makawa", v))] <- "kutengenza vitu vidogo vidogo"
v[which(grepl("makwaru", v))] <- "kutengenza vitu vidogo vidogo"
v[which(grepl("kushona mikeka", v))] <- "kutengenza vitu vidogo vidogo"


# Njukuti
v[which(grepl("njukuti", v))] <- "kutengeneza njukuti" 
v[which(grepl("utengengeneza njukuti", v))] <- "kutengeneza njukuti" 
v[which(grepl("kutengeneza fagio", v))] <- "kutengeneza njukuti" 
v[which(grepl("mafyagio)", v))] <- "kutengeneza njukuti" 

# Buzi
v[which(grepl("mbuzi", v))] <- "mbuzi" 


# Buzi
v[which(grepl("kuchukua abiria", v))] <- "usafiri" 


# Kamba
v[which(grepl("kamba", v))] <- "kutengeza kamba"

# Piga Tano
v[which(grepl(" tanu", v))] <- "kuni"
v[which(grepl("piga mkaa", v))] <- "kuni"

# Sabuni
v[which(grepl("sabuni", v))] <- "kutengeneza sabuni"

# Cloth sales
v[which(grepl("kuuza nguo ( vijora)", v))] <- "kuuza nguo"
v[which(grepl("vijora", v))] <- "kuuza nguo"
v[which(grepl("kuuza viatu na nguo", v))] <- "kuuza nguo"


#Biashara ya chakula
v[which(grepl("kuuza chipsi", v))] <- "biashara ya chakula"
v[which(grepl("kuuza vinywaji", v))] <- "biashara ya chakula"
v[which(grepl("biashara ndogo ndogo", v))] <- "biashara ya chakula"
v[which(grepl("biashara ndogondogo", v))] <- "biashara ya chakula"
v[which(grepl("kufanya vileja", v))] <- "biashara ya chakula"
v[which(grepl("kutengeneza juisi", v))] <- "biashara ya chakula"

# ART 
v[which(grepl("uchoraje", v))] <- "uchoraji"



# kufua
v[which(grepl("kufua", v))] <- "kufua nguo"
v[which(grepl("kufuwa", v))] <- "kufua nguo"

# utembo
v[which(grepl("utembo", v))] <- "utembo"

# Teaching 
v[which(grepl("mwalimu", v))] <- "mwalimu"
v[which(grepl("somesha", v))] <- "mwalimu"
v[which(grepl("someshe", v))] <- "mwalimu"
v[which(grepl("kusome", v))] <- "mwalimu"
v[which(grepl("uwalimu", v))] <- "mwalimu"
v[which(grepl("masomo", v))] <- "mwalimu"


# Fish Prep
v[which(grepl("samaki", v))] <- "tayrisha samaki"
v[which(grepl("kuanika dagaa", v))] <- "tayrisha samaki"

# bodaboda
d$self$usafiri[which(grepl("bodaboda", v))] <- "bodaboda"
v[which(grepl("bodaboda", v))] <- "usafiri"
d$self$usafiri[which(grepl("noah", v))] <- "noah"
v[which(grepl("noah", v))] <- "usafiri"

#fundi
d$self$fundi[which(grepl("kutengeneza nguo", v))] <- "shona"
v[which(grepl("kutengeneza nguo", v))] <- "fundi"

# traditional med
v[which(grepl("dawa", v))] <- "trad medicine"
v[which(grepl("tiba za asili", v))] <- "trad medicine"

# Safari
d$self$usafiri[which(grepl("dereva", v))] <- "gari"
v[which(grepl("dereva", v))] <- "usafiri"

# Cleaning Feilds
v[which(grepl("kusafisha shamba", v))] <- "kibarua ya kilimo"

# kuuza genge
v[which(grepl("kuuza genge", v))] <- "biasahara ya kilimo"

v[which(grepl("biasahara ya kilimo", v))] <- "biashara ya kilimo"


# Cleaning 2
v[which(grepl("2", v))] <- NA

# Cleaning Other
v[which(grepl("other", v))] <- NA



d$self$main_cat <- v

sort(table(d$self$main_cat))

###############################################################################
################### CATEGROIZING WAGE LABOR ###################################

sort(table(d$wage$main))
length(sort(table(d$wage$main)))

v <- d$wage$main
v <- tolower(v)
sort(table(v))

# Cutting wood
v[which(grepl("mbao", v))] <- "kukata mbao"

# Cleaning
v[which(grepl("safi", v))] <- "kusafisha eneo"

# cleaning clothes
v[which(grepl("nguo", v))] <- "kibarua kufua nguo"

# cleaning clothes
v[which(grepl("nazi", v))] <- "kibarua ya nazi"

# cleaning mwani
v[which(grepl("kufunga mwani", v))] <- "kibarua ya mwani"
v[which(grepl("kibarua ya kupanda mwani", v))] <- "kibarua ya mwani"

# cleaning ulinzi
v[which(grepl("ulinzi", v))] <- "kibarua ya ulinzi"
v[which(grepl("mlinzi", v))] <- "kibarua ya ulinzi"

# Kibarua ya afya

v[which(grepl("dawa", v))] <- "kibarua ya afya"
v[which(grepl("chanjo", v))] <- "kibarua ya afya"

# 
v[which(grepl("kushona nyavu", v))] <- "kibarua ya uvuvi"

#
v[which(grepl("clove picking", v))] <- "kibarua ya karafuu"

v[which(grepl("kazi za nyumbani", v))] <- "kazi za ndani"

v[which(grepl("kupanda kunde", v))] <- "kibarua ya kilimo"

v[which(grepl("askari", v))] <- "polisi"

v[which(grepl("aandikisha vitambulisho wilayani", v))] <- "mshahara ya serekali"
v[which(grepl("afisa mipango wizara ya afya pemba", v))] <- "mshahara ya serekali"
v[which(grepl("afisa tehama", v))] <- "mshahara ya serekali"
v[which(grepl("afisa tehama mkoani", v))] <- "mshahara ya serekali"
v[which(grepl("afya ya jamii", v))] <- "kibarua ya afya"
v[which(grepl("baraza la mji", v))] <- "mshahara ya serekali"
v[which(grepl("baraza la mji mkoani", v))] <- "mshahara ya serekali"
v[which(grepl("huduma za afya", v))] <- "kibarua ya afya"
v[which(grepl("halmashauri", v))] <- "mshahara ya serekali"
v[which(grepl("halmashauri", v))] <- "mshahara ya serekali"
v[which(grepl("halmashauri", v))] <- "mshahara ya serekali"
v[which(grepl("halmashauri(council)", v))] <- "mshahara ya serekali"
v[which(grepl("kuandikisha vitambulisho wilayani", v))] <- "mshahara ya serekali"
v[which(grepl("kuandikisha vitambulisho wilayani", v))] <- "mshahara ya serekali"
v[which(grepl("ofisi ya vitambulisho", v))] <- "mshahara ya serekali"
v[which(grepl("ofisi ya vitambulisho", v))] <- "mshahara ya serekali"
v[which(grepl("ofisi ya vitambulisho", v))] <- "mshahara ya serekali"
v[which(grepl("mamlaka ya maji", v))] <- "mshahara ya serekali"
v[which(grepl("tehama", v))] <- "mshahara ya serekali"
v[which(grepl("halmashauri", v))] <- "mshahara ya serekali"
v[which(grepl("afisa kilimo", v))] <- "mshahara ya serekali"
v[which(grepl("wizarani", v))] <- "mshahara ya serekali"
v[which(grepl("sheha", v))] <- "mshahara ya serekali"
v[which(grepl("mahakamani", v))] <- "mshahara ya serekali"
v[which(grepl("aandikisha vitambulisho wilayani", v))] <- "mshahara ya serekali"
v[which(grepl("sensa", v))] <- "mshahara ya serekali"

v[which(grepl("mahakamani", v))] <- "mshahara ya serekali"

# auctioneer
v[which(grepl("dalali wa samaki", v))] <- "dalali"

# bakery
v[which(grepl("bekari", v))] <- "bekary"

# bakery
v[which(grepl("bekari", v))] <- "bekary"

# bodaboda
v[which(grepl("boda boda", v))] <- "usafiri"

# Argicultural Labor
v[which(grepl("kibarua ya kusagisha mpunga", v))] <- "kibarua ya kilimo"
v[which(grepl("kushindia viazi", v))] <- "kibarua ya kilimo"
v[which(grepl("kusagisha mpunga", v))] <- "kibarua ya kilimo"
v[which(grepl("kibarua ya kusagisha mpunga", v))] <- "kibarua ya kilimo"
v[which(grepl("kutema/kufyeka", v))] <- "kibarua ya kilimo"

# Retail trade
v[which(grepl("wakala wa kupokea na kutoa mizigo", v))] <- "usafiri ya bithaa"



# boat builder
v[which(grepl("kuchonga dau", v))] <- "kibarua ya fundi"

# Porter
v[which(grepl("kuchukua mizigo bandarini", v))] <- "kibarua kubeba mzigo"
v[which(grepl("kubeba", v))] <- "kibarua kubeba mzigo"

# Religous education
v[which(grepl("kusomesha qurani", v))] <- "madrassa"
v[which(grepl("kusomesha qurani", v))] <- "madrassa"
v[which(grepl("kusomesha madrasa", v))] <- "madrassa"

# Tassaf
v[which(grepl("kaya masikini", v))] <- "tasaf"

v[grepl("jesh", v)] <- "jeshi"
v[grepl("jesh", v)] <- "mshahara ya serekali"


# KAZI NDANI
v[which(grepl("kusafisha eneo", v))] <- "kazi za ndani"
v[which(grepl("kibarua ya kufua", v))] <- "kazi za ndani"



# Bank
v[which(grepl("bank", v))] <- "mshahara ya kampuni"

# Semina
v[which(grepl("semina", v))] <- "mshahara ya serekali"


# Voluteering
v[which(grepl("kujitolea", v))] <- "kujitolea"

# Sawmill
v[which(grepl("kukata magogo ya miti", v))] <- "kukata mbao"
v[which(grepl("kukata mikeshia", v))] <- "kukata mbao"
v[which(grepl("kukata miti", v))] <- "kukata mbao"
v[which(grepl("kukata mikeshia", v))] <- "kukata mbao"
v[which(grepl("kukata mikeshia", v))] <- "kukata mbao"

d$wage$main_cat <- v
sort(table(d$wage$main_cat))

###############################################################################
################# CATEGORIZATION FOREST #######################################

sort(table(d$forest$main))
sort(table(v))

d$forest$main <- tolower(d$forest$main)
v<-d$forest$main
v[which(grepl("alikata miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("alipasua boriti", v))] <- "miti kujenga"
v[which(grepl("kuka miti", v))] <- "miti kujenga"
v[which(grepl("kukata fito", v))] <- "miti kujenga"
v[which(grepl("kukata magogo kwa ajili ya kutengeneza mkaa", v))] <- "miti kujenga"
v[which(grepl("kukata miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("kukata miti ya kujengea kibanda", v))] <- "miti kujenga"
v[which(grepl("miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("mbao", v))] <- "miti kujenga"
v[which(grepl("matawi ya mjiti", v))] <- "miti kujenga"

v[which(grepl("utembo", v))] <- "makuti"
v[which(grepl("chikichi", v))] <- "chikichi"
v[which(grepl("mkaa", v))] <- "mkaa"
v[which(grepl("firewood", v))] <- "kuni"
v[which(grepl("mabungo", v))] <- "matunda ya msitu"
v[which(grepl("mabungo", v))] <- "matunda ya msitu"
v[which(grepl("matunda", v))] <- "matunda ya msitu"
v[which(grepl("asali", v))] <- "asali ya pori/msitu"

d$forest$main_cat <- v
sort(table(d$forest$main_cat))



# Forest Crop

d$forest_crop$type <- tolower(d$forest_crop$type)
v<-d$forest_crop$type
v[which(grepl("alikata miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("alipasua boriti", v))] <- "miti kujenga"
v[which(grepl("kuka miti", v))] <- "miti kujenga"
v[which(grepl("kukata fito", v))] <- "miti kujenga"
v[which(grepl("kukata magogo kwa ajili ya kutengeneza mkaa", v))] <- "miti kujenga"
v[which(grepl("kukata miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("kukata miti ya kujengea kibanda", v))] <- "miti kujenga"
v[which(grepl("miti ya kujengea", v))] <- "miti kujenga"
v[which(grepl("mbao", v))] <- "miti kujenga"
v[which(grepl("matawi ya mjiti", v))] <- "miti kujenga"

v[which(grepl("utembo", v))] <- "makuti"
v[which(grepl("chikichi", v))] <- "chikichi"
v[which(grepl("mkaa", v))] <- "mkaa"
v[which(grepl("firewood", v))] <- "kuni"
v[which(grepl("mabungo", v))] <- "matunda ya msitu"
v[which(grepl("mabungo", v))] <- "matunda ya msitu"
v[which(grepl("matunda", v))] <- "matunda ya msitu"
v[which(grepl("asali", v))] <- "asali ya pori/msitu"

d$forest_crop$main_cat <- v
sort(table(d$forest$main_cat))




##############################################################################
##################### ANIMALS ################################################

d$animal_emp$main<- tolower(d$animal_emp$main)
d$animal_emp$main[which(grepl("gnombe", d$animal_emp$main))] <- "n'gombe"
d$animal_emp$main[which(grepl("beef", d$animal_emp$main))] <- "n'gombe"


###############################################################################################
##################### CHANGES WITH ILARIA #####################################################


# malaka ya maji needs to put together under mshahara ya serekali
# ask makwaru
# kuvua -> kuvua
# kuzua genge ->  find out
# kusomesha quarni and kushomeha madrassa needs to be merged
# Kutengenza jusis -> biashara ya jusi 
# Kufyeka shamba and kutema/fyeka both become kibaru ya kilimo
# para njiti  = kutengeza nyajukuti
# kukata miyale is kukata nywele
# merege all senus work together.
# kukata magogo and kukata mikesha and kukata miti merge with miti kujenga
# kujitolea hosipitali and some other mispelling needs to be merged
# kujitolea skuli and kujitolea mwalimu
# kufanya vilejia -> biasahara ya chakula
# kuchuna mbuzi -> mbuzi 
# kuchuka abaria -> usafiri
# kuchukua mizigo bandarini 
# adisa mipgano wizara -> msharaha ya serekali
# merge bekeri and bekery
# merge halamshauri and council
# kaya maskini is tasaf
# kibarua kufua is the same as kufua nguo
# 


###############################################################################################
######################## CHANGES FOR MATT #####################################################

# Clean sales 
# Clean Rental
# Get Milk
##############################################################################
############## CHANGES JAN 2024 ##############################################

d$self$main_cat <- category_clean("self", "main_cat", 24)
d$wage$main_cat <- category_clean("wage", "main_cat", 24)
d$forest$main_cat <- category_clean("forest", "main_cat", 24)
d$forest_crop$main_cat <- category_clean("forest_crop", "main_cat", 24)
# sort(table(d$ag_crop$type)) <- category_clean("forest_crop", "main_cat", 24)
###########################################################
############ MEETINGS #####################################

table(d$main$`_submitted_by`)

by = "sijuitaponi"
period <- c(max(d$main$period)-1, max(d$main$period))
after <- as.Date("2024-01-01 00:26:58 UTC")


# New
View(d$hhm_details[d$hhm_details$"_submission__submitted_by" ==by & d$hhm_details$"_submission__submission_time" > after, ])

# Left
View(d$left[d$left$"_submission__submitted_by" ==by & d$left$period %in% period, ])

# Safari
View(d$safari[d$safari$"_submission__submitted_by" == by & d$safari$period %in% period,])

a <-d$wage %>% group_by(`_submission_submitted_by`, period) %>% summarize(mean = mean(income, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = `_submission_submitted_by`)) +
  geom_line(aes(color=`_submission_submitted_by`))

a <-d$wage %>% group_by(`_submission_submitted_by`, period) %>% summarize(mean = mean(income, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = `_submission_submitted_by`)) +
  geom_line(aes(color=`_submission_submitted_by`))

a <-d$wage %>% group_by(shehia, period) %>% summarize(mean = sum(income, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$self %>% group_by(`_submission_submitted_by`, period) %>% summarize(mean = sum(profits, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = `_submission_submitted_by`)) +
  geom_line(aes(color=`_submission_submitted_by`))


d$self %>% group_by(`_submission_submitted_by`, period) %>% summarize(mean = sum(profits, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = `_submission_submitted_by`)) +
  geom_line(aes(color=`_submission_submitted_by`))

d$self %>% group_by(shehia, period) %>% summarize(mean = sum(profits, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))


d$self %>% group_by(shehia, period) %>% filter(main == "duka") %>% summarize(mean = sum(profits, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))



d$self %>% group_by(shehia, period) %>% summarize(mean = sum(hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$wage %>% group_by(shehia, period) %>% summarize(mean = sum(hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$wage %>% group_by(shehia, period) %>% summarize(mean = sum(hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))


d$mwani %>% group_by(shehia, period) %>% summarize(mean = sum(prepare_mwani_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$mwani %>% group_by(shehia, period) %>% summarize(mean = sum(harvest_mwani_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$mwani %>% group_by(shehia, period) %>% summarize(mean = sum(ag_okota_mwani_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$debt %>% group_by(shehia, period) %>% summarize(mean = sum(debtor_amount_borrowed, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 1000000) 

d$debt %>% group_by(shehia, period) %>% filter(shehia == "mtambili") %>% summarize(mean = sum(debtor_amount_borrowed, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 1000000) +
  xlim(0, 40)

d$debt %>% group_by(shehia, period) %>% filter(shehia == "chokocho") %>% summarize(mean = sum(debtor_amount_borrowed, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 1000000) +
  xlim(0, 40)

d$savings%>% group_by(shehia, period) %>% filter(shehia == "mkoani") %>% summarize(mean = sum(store_money, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 10000000) +
  xlim(0, 40)


d$ag_crop %>% group_by(shehia, period) %>% summarize(mean = sum(price*sold, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$main %>% group_by(shehia, period) %>% summarize(mean = sum(con_food, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$main %>% group_by(shehia, period) %>% summarize(mean = sum(con_clothes, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$main %>% group_by(shehia, period) %>% summarize(mean = sum(con_other, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))


d$ag_crop %>% group_by(shehia, period) %>% summarize(mean = sum(price*sold, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia))

d$gift_give %>% group_by(shehia, period) %>% summarize(mean = sum(gift_give_value, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 10000000)


d$gift_rec %>% group_by(shehia, period) %>% summarize(mean = sum(gift_rec_value, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 10000000)

d$ag %>% group_by(shehia, period) %>% summarize(mean = sum(plant_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 2500)


d$ag %>% group_by(shehia, period) %>% summarize(mean = sum(weed_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 2500)



d$ag %>% filter(shehia == "kifundi") %>% group_by(period) %>% summarize(mean = sum(plant_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 2500)


a<-d$ag %>% filter(shehia == "kifundi") %>% group_by(period) %>% summarize(mean = sum(plant_hours, na.rm =T))


ggplot(a, aes(x = period, y = mean)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 2500)


 
d$ag %>% group_by(shehia, period) %>% summarize(mean = sum(harvest_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 3000)

d$ag %>% group_by(shehia, period) %>% summarize(mean = sum(harvest_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 5000)

d$ag %>% group_by(shehia, period) %>% summarize(mean = sum(harvest_hours, na.rm =T))  %>% 
  ggplot(aes(x = period, y = mean, group = shehia)) +
  geom_line(aes(color=shehia)) +
  ylim(0, 5000)


# SELF
d$self$main[d$self$by == by & d$self$period %in% period]
d$self$profits[d$self$by == by & d$self$period %in% period]
d$self$crew[d$self$by == by & d$self$period %in% period]
d$self$hours[d$self$by == by & d$self$period %in% period]
d$self$notes[d$self$by == by & d$self$period %in% period]
d$self[d$self$by == by & d$self$period %in% period, ]

View(d$self[d$self$by == by & d$self$period %in% period, ]
)
d$self[d$self$by == by & d$self$period %in% period, c("main", "shehia")]


# Seaweeed
View(d$mwani[d$mwani$"_submission_submitted_by" == by & d$mwani$period %in% period, ])
d$main$mwani_price[d$main$"_submitted_by" == by & d$main$period %in% period]
d$main$mwani_sold_amount[d$main$"_submitted_by" == by & d$main$period %in% period]

# Wage
d$wage$main[d$wage$by == by & d$wage$period %in% period]
d$wage$income[d$wage$by == by & d$wage$period %in% period]
d$wage$hours[d$wage$by == by & d$wage$period %in% period] # Note that the one guy worked 49 hours as an ujenzi and only got 49
d$wage$notes[d$wage$by == by & d$wage$period %in% period]
View(d$wage[d$wage$by == by & d$wage$period %in% period, ])

# Agriculture

d$ag_crop$type[d$ag_crop$by == by & d$ag_crop$period %in% period]
d$ag_crop$harvest[d$ag_crop$by == by & d$ag_crop$period %in% period]
d$ag_crop[d$ag_crop$by == by & d$ag_crop$period %in% period, ]
View(d$ag_crop[d$ag_crop$period %in% period & d$ag_crop$by == by, ])

# Animal

View(d$animal[d$animal$by == by & d$animal$period %in% period, ])
View(d$animal_emp[d$animal_emp$`_submission__submitted_by` == by & d$animal_emp$period %in% period, ])

# Forest

View(d$forest_crop[d$forest_crop$by == by & d$forest_crop$period %in% period, ])
d$forest[d$forest$`_submission__submitted_by` ==by  & d$forest$period %in% period, ]

# Gift Give
View(d$gift_give_rep[d$gift_give_rep$`_submission__submitted_by` ==by & d$gift_give_rep$"_submission__submission_time" >after, ])


# Gift Give
View(d$gift_rec[d$gift_rec$`_submission__submitted_by` ==by & d$gift_rec$"_submission__submission_time" >after, ])


# Store Credit
View(d$store_credit[d$store_credit_rep$`_submission__submitted_by` ==by & d$store_credit_rep$"_submission__submission_time" >after, ])

# Sold 
View(d$sold[d$sold$`_submission__submitted_by` ==by & d$sold$"_submission__submission_time" >after, ])

# Rent
View(d$rent[d$rent$`_submission__submitted_by` ==by & d$rent$"_submission__submission_time" >after, ])

# Ag help
temp<-select(d$main, contains("ag_help"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])
# AG labor
temp<-select(d$main, contains("ag_labor"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])
# Animal Help
temp<-select(d$main, contains("animals_labor"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])
# Animal Labor
temp<-select(d$main, contains("animals_help"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])
# Mwani help
temp<-select(d$main, contains("mwani_help"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])
# Mwani labor
temp<-select(d$main, contains("mwani_help"))
View(temp[d$main$`_submitted_by`==by & d$main$period %in% period,])






View(d$main[d$main$`_submitted_by`==by & d$main$period %in% period,])
d$main$debtor_lent[d$main$`_submitted_by`==by]
d$main$creditor_lent[d$main$`_submitted_by`==by]
d$main$creditor_amount_returned[d$main$`_submitted_by`==by]


View(d$main[d$main$`_submitted_by`=="bakarimakame" & d$main$period %in% period,])


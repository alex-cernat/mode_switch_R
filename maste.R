#############################################################
#
# Project looking at how epopel change modes in time
#
# RQ1: What are the modes
#
#
#
#############################################################

# clear working space
rm(list = ls ())



# Admin -------------------------------------------------------------------

# folders for installed packages

.libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package")))


# create folders and unzip
# dir.create("./data")
# dir.create("./output")
# dir.create("./functions")


# install packages and load

# use packrat to install packages localy

pkg <- c("tidyverse", "ggthemes", "haven",
         "lme4", "rmarkdown", "reshape2",
         "MplusAutomation", "poLCA", "plotly",
         "texreg", "broom", "ggalluvial")

#install.packages(pkg)
sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# get ind mode and outcome
indall_data <- list.files("./data/stata13/",
           pattern = "indall", full.names = T)

i <- 5
ip_all <- map(indall_data[5:10], function(x) {

  temp <- read_dta(x) %>%
    rename_all(funs(str_remove(., "^[d-j]_"))) %>%
    select(pidp, hidp, indmode, ivfio) %>%
      mutate(wave = i)

    i <<- i + 1

    temp

  })

ip_all

# get mode design
hhsamp_paths <- list.files("./data/stata13/",
                          pattern = "hhsamp", full.names = T)

i <- 5
hhsamp_data <- map(hhsamp_paths[5:10], function(x) {

  temp <- read_dta(x) %>%
    rename_all(funs(str_remove(., "^[d-j]_"))) %>%
    select(hidp, matches("gridmodew")) %>%
    rename_all(funs(str_remove(., "^ff_"))) %>%
    mutate(wave = i)

  i <<- i + 1

  temp

})

# delete extra gridmodes
hhsamp_data[4:6] <- map(hhsamp_data[4:6], function(x) select(x, -gridmodew5))
hhsamp_data[5:6] <- map(hhsamp_data[5:6], function(x) select(x, -gridmodew8))
hhsamp_data[[6]] <- select(hhsamp_data[[6]], -gridmodew9)

hhsamp_data <- map(hhsamp_data, function(x) {
  x %>%
    setNames(c("hidp", "mm", "wave"))
}
)

# make super long data
ip_long <- reduce(ip_all, rbind)
hhsamp_long <- reduce(hhsamp_data, rbind)

out_long <- left_join(ip_long, hhsamp_long, by = c("hidp", "wave"))

# keep only mixed mode data
desc_tab(out_long$mm)

out_long2 <- filter(out_long, mm == 3)

table(out_long2$wave,
      out_long2$indmode,
      useNA = "always")

attributes(out_long2$indmode)

desc_tab(out_long2$ivfio)
attributes(out_long2$ivfio)


# excclude youth and telephone
out_long2 <- out_long2 %>%
  filter(!ivfio %in% c(21:25, 63)) %>%
  filter(indmode != 2)


# exclude people who were not in all waves
out_long2 <- out_long2 %>%
  tbl_df() %>%
  group_by(pidp) %>%
  mutate(sum_var = max(row_number())) %>%
  ungroup() %>%
  filter(sum_var == 6)



# make non-response variable and code missing
out_long2 <- out_long2 %>%
  mutate(f2f = case_when(indmode == 1 ~ 1,
                         indmode == 3 ~ 0),
         f2f_nomiss = ifelse(is.na(f2f), 0, f2f),
         non_resp = ifelse(ivfio == 1, 0, 1))


desc_tab(out_long2$f2f)
desc_tab(out_long2$non_resp)
desc_tab(out_long2$f2f_nomiss)

out_long2 <- tbl_df(out_long2) %>%
  dplyr::select(pidp, wave, everything(), -hidp)

# make long data

out_long2 <- as.data.frame(out_long2)
out <-
  reshape(
    data = out_long2,
    timevar = "wave",
    sep = "_",
    idvar = "pidp",
    direction = "wide"
  )


out <- tbl_df(out) %>%
  arrange(pidp)


write.csv(file = "./data/ip_wide_out.csv", out)

# Cluster analysis --------------------------------------------------------



cls_data <- out %>%
  dplyr::select(pidp, matches("f2f")) %>%
  mutate_at(vars(matches("f2f")),
            funs(. + 1))

# Create a convenience function that will fit the K-class model to
#     the political participation data
fitLCA <- function(k) {
  poLCA(cbind(f2f_5, f2f_6,
              f2f_7, f2f_8,
              f2f_9) ~ 1,
        data = cls_data, nclass = k, nrep = 50)

}

# Apply the function to successively increasingly classes K=1,2,3....,6
MK <- lapply(1:6, fitLCA)


# Possible to look at AIC, BIC, etc.
cbind(sapply(MK, `[[`, "aic"),
      sapply(MK, `[[`, "bic"))


MK[[4]]

count(out, non_resp_5)
count(out2, out_5)


out2 <- out %>%
  mutate(out_5 = case_when(f2f_5 == 1 & non_resp_5 == 0 ~ "F2f",
                           non_resp_5 == 1 ~ "Non-resp",
                           TRUE ~ "Web"),
         out_6 = case_when(f2f_6 == 1  & non_resp_6 == 0 ~ "F2f",
                           non_resp_6 == 1 ~ "Non-resp",
                           TRUE ~ "Web"),
         out_7 = case_when(f2f_7 == 1  & non_resp_7 == 0 ~ "F2f",
                           non_resp_7 == 1 ~ "Non-resp",
                           TRUE ~ "Web"),
         out_8 = case_when(f2f_8 == 1 ~ "F2f",
                           non_resp_8 == 1 ~ "Non-resp",
                           TRUE ~ "Web"),
         out_9 = case_when(f2f_9 == 1 ~ "F2f",
                           non_resp_9 == 1 ~ "Non-resp",
                           TRUE ~ "Web"))



out3 <- out2 %>%
  select(pidp, out_5:out_9) %>%
  count(out_5, out_6, out_7, out_8, out_9) %>%
  gather(value, key, -n) %>%
  mutate(wave = as.numeric(str_remove(value, "out_"))) %>%
  group_by(wave) %>%
  mutate(id = row_number())



out3 %>%
  mutate(key = as.factor(key),
         key = factor(key, levels = levels(key)[c(3, 1, 2)])) %>%
  ggplot(aes(x = wave, stratum = key, alluvium = id,
             y = n,
             fill = key, label = key)) +
#  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  theme_tufte(base_size = 18) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Wave",
       y = "Frequency",
       fill = "Outcome") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#999999"))

ggsave(file = "./output/mode_switch.png", dpi = 500)


cls_data <- out %>%
  dplyr::select(pidp, matches("f2f_nomiss"), matches("non_resp")) %>%
  mutate_at(vars(matches("f2f_nomiss"), matches("non_resp")),
            funs(. + 1))


# Create a convenience function that will fit the K-class model to
#     the political participation data
fitLCA <- function(k) {
  poLCA(cbind(f2f_nomiss_5, f2f_nomiss_6,
              f2f_nomiss_7, f2f_nomiss_8,
              f2f_nomiss_9, non_resp_5,
              non_resp_6, non_resp_7,
              non_resp_8, non_resp_9) ~ 1,
        data = cls_data, nclass = k, nrep = 50)

}

# Apply the function to successively increasingly classes K=1,2,3....,6
MK <- lapply(1:6, fitLCA)


fit_tab <- map(MK, lca_fit_extract) %>%
  reduce(rbind) %>%
  mutate(class = 1:6) %>%
  select(class, everything())

fit_tab


probs <- MK[[5]]$probs

probs <- reduce(probs, rbind) %>%
  tbl_df() %>%
  mutate(var = rep(names(probs), each = 5),
         var = str_remove_all(var, "_nomiss|non_"),
         var = str_replace(var, "f2f", "web"),
         class = rep(str_c("Class ", 1:5), 10)) %>%
  rename(Yes = `Pr(1)`, No = `Pr(2)`)

g <- probs %>%
  ggplot(aes(var, Yes, color = class, group = class)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.8) +
  theme_bw()


probs <- probs %>%
  tidyr::separate(var, into = c("Outcome", "Wave"), sep = "_")

g <- probs %>%
  ggplot(aes(Wave, Yes, color = class, group = class)) +
  facet_wrap(~Outcome) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.8) +
  theme_bw()

ggplotly(g)



# cleand data in wave 4 ---------------------------------------------------

ip4 <- read_dta("./data/stata13/d_indresp_ip.dta")
ip9 <- read_dta("./data/stata13/i_indresp_ip.dta")

control_data <- list("ip4" = ip4, "ip9" = ip9)

control_data <- map(control_data, function(x) {
  x %>%
  rename_all(funs(str_remove_all(., "d_|i_"))) %>%
  select(pidp, jbsemp, dvage,
         hiqual_dv, sex, racel_dv, mastat_dv, sf1,
         gor_dv, urban_dv,
         hhorig, health)

})

map(control_data, function(x) desc_tab(x["hiqual_dv"]))


control_data2 <- map(control_data, function(x) {
  data <- x %>%
    mutate(inwork = case_when(jbsemp %in% 1:2 ~ 1,
                              jbsemp == -8 ~ 0),
           edu = case_when(hiqual_dv %in% 1:2 ~ "Higher",
                           hiqual_dv == 3 ~ "A level",
                           hiqual_dv == 4 ~ "GCSE",
                           hiqual_dv %in% 5:9 ~ "Other"),
           edu = as.factor(edu),
           edu = factor(edu,levels(edu)[c(3, 1, 2, 4)]),
           agecat = cut(dvage, breaks = c(16, 35, 55, 75, 102),
                           labels = c("age_35", "age_55", "age_75", "age_102")),
           female = sex - 1,
           white = case_when(racel_dv %in% 1:4 ~ 1,
                             racel_dv > 4 ~ 0),
           partner = case_when(mastat_dv %in% c(2:3, 10) ~ 1,
                                mastat_dv %in% c(1, 4:9) ~ 0),
           sf1 = ifelse(sf1 < 1, NA, sf1),
           sf1 = as.numeric(5 - sf1),
           longill = case_when(health == 1 ~ 1,
                                health == 2 ~ 0),
           london = ifelse(gor_dv == 7, 1, 0),
           north = ifelse(gor_dv %in% c(1:3, 10:12), 1, 0),
           urban = ifelse(urban_dv == 1, 1, 0),
           refresh = ifelse(hhorig == 11, 0 , 1)) %>%
    select(pidp, inwork, edu, agecat, female, white, partner,
           sf1, longill, london, north, urban, refresh)

  cbind(data, dummyfy(data$edu), dummyfy(data$agecat)) %>%
    tbl_df()

})


# give wave prefix
names(control_data2[[1]]) <- str_c(names(control_data2[[1]]), "_4")
names(control_data2[[2]]) <- str_c(names(control_data2[[2]]), "_9")

control_data2 <- map(control_data2,
                     function(x) rename(x, pidp = matches("pidp")))

# merge
control_data3 <- reduce(control_data2, full_join, by = "pidp")


# Make outcomes wave 10 ---------------------------------------------------


ip10 <- read_dta("./data/stata13/j_indall_ip.dta")
ip10 <- ip10 %>%
  rename_all(funs(str_remove(., "j_"))) %>%
  select(pidp, hidp, indmode, ivfio) %>%
  filter(!ivfio %in% c(21:25, 63)) %>% # exclude children
  filter(indmode != 2) %>% #exclude telphone
  mutate(f2f = case_when(indmode == 1 ~ 1,
                         indmode == 3 ~ 0),
         f2f_nomiss = ifelse(is.na(f2f), 0, f2f),
         non_resp = ifelse(ivfio == 1, 0, 1)) %>%
  select(-ivfio, -indmode)




# keep only mixed mode data
ip10_hh <- read_dta("./data/stata13/j_hhsamp_ip.dta")
ip10_hh <- ip10_hh %>%
  rename_all(funs(str_remove_all(., "j_|ff_"))) %>%
  filter(gridmodew10 == 3) %>%
  select(hidp)


ip10 <- right_join(ip10, ip10_hh, by = "hidp") %>%
  select(-hidp)


# Bring together all data -------------------------------------------------

ip_data <- full_join(out, control_data3, by = "pidp") %>%
  full_join(ip10, by = "pidp") %>%
  mutate(id = row_number())

save(ip_data, file = "./data/ip_data_clean.RData")

# Export to Mplus ---------------------------------------------------------

ip_data2 <- ip_data %>%
  mutate_all(funs(as.numeric(.)))

prepareMplusData(ip_data2, "./mplus/ip_data.dat")


# Import Mplus results ----------------------------------------------------


lca2 <- readModels("./mplus/mode_switch_lca_2.out")
lca3 <- readModels("./mplus/mode_switch_lca_3.out")
lca4 <- readModels("./mplus/mode_switch_lca_4.out")
lca5 <- readModels("./mplus/mode_switch_lca_5.out")
lca6 <- readModels("./mplus/mode_switch_lca_6.out")


# fit table

info <- c("Observations", "Parameters",
          "ChiSqCategoricalPearson_Value",
          "LL", "AIC", "BIC", "Entropy")



mplus_lca_fit <- function(x,
                          info = c("Observations",
                                   "Parameters",
                                    "ChiSqCategoricalPearson_Value",
                                    "LL", "AIC", "BIC", "Entropy")) {
  x$summaries %>%
    tbl_df() %>%
    select(info) %>%
    rename(n = Observations,
           Chi2 = ChiSqCategoricalPearson_Value) %>%
    select(Chi2, LL, Parameters, n, everything())

}

mplus_res <- list(lca2, lca3, lca4, lca5, lca6)

lca_fit_mplus <- map(mplus_res, mplus_lca_fit) %>%
  reduce(rbind) %>%
  mutate(Classes = 2:6) %>%
  select(Classes, everything())


# link with other data
ip_data3 <- lca5$savedata %>%
  tbl_df() %>%
  select(ID:C) %>%
  rename_all(funs(str_to_lower(.))) %>%
  right_join(ip_data)


# to wave 2 prediction

desc_tab(ip_data3$c)

ip_data3 <- ip_data3 %>%
  mutate(class = as.factor(str_c("Class ", c)))



res1 <- glm("non_resp ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9",
  data = ip_data3, family=binomial())


res2 <- glm("non_resp ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + class",
           data = ip_data3, family=binomial())


summary(res1)
summary(res2)


res3 <- glm("f2f ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9", family=binomial(),
           data = ip_data3)


res4 <- glm("f2f ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + class", family=binomial(),
           data = ip_data3)


glance(res1)

glance(res1)
glance(res2)
glance(res3)
glance(res4)

labs_res1 <- c("Intercept", "Edu: A level", "Edu: GCSE",
               "Edu: Other", "Age: 36-55", "Age: 56-75",
               "Age: >75", "Female", "Partner", "Urban",
               "London", "North", "Wave 9 f2f",
               "Class 2", "Class 3", "Class 4", "Class 5")

texreg(list(res1, res2, res3, res4),
        caption = "Explaining response and mode in wave 10",
        custom.model.names = c("Response m1", "Response m2",
                               "Face to face m1", "Face to face m2"),
        custom.coef.names = c(labs_res1),
        custom.note = "Degree, under 36, rural, male, wave in wave 9 and
        class 1 are refrences")

# r squares

# Explained deviance

1 - (res1$deviance/res1$null.deviance)
1 - (res2$deviance/res2$null.deviance)


1 - (res3$deviance/res3$null.deviance)
1 - (res4$deviance/res4$null.deviance)


# graphs of predicted probabilities ---------------------------------------




res_data2 <- augment(res2)
res_data4 <- augment(res4)


mean(ip_data3$non_resp, na.rm = T)
mean(res_data2$non_resp, na.rm = T)
mean(res_data2$.hat, na.rm = T)

mean(ip_data3$f2f, na.rm = T)
mean(res_data4$f2f, na.rm = T)
mean(res_data4$.hat, na.rm = T)

res_data2 %>%
  mutate(prob = exp(.fitted)/(1 + exp(.fitted)),
    class = as.factor(class),
         class = factor(class, labels = c("Switch and non-resp",
                                      "Fast switch",
                                      "Slow switch",
                                      "F2f resp",
                                      "Web resp"))) %>%
  ggplot(aes(class, prob)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  labs(x = "Class",
       y = "Predicted non-resp. probability")

ggsave(file = "./output/ip10_pred_nonresp.png", dpi = 500)

res_data4 %>%
  mutate(prob = exp(.fitted)/(1 + exp(.fitted)),
         class = as.factor(class),
         class = factor(class, labels = c("Switch and non-resp",
                                          "Fast switch",
                                          "Slow switch",
                                          "F2f resp",
                                          "Web resp"))) %>%
  ggplot(aes(class, prob)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  labs(x = "Class",
       y = "Predicted f2f probability")


ggsave(file = "./output/ip10_pred_f2f.png", dpi = 500)


# Make probability graphs -------------------------------------------------

lca5$parameters$probability.scale

LCAfit <- lca5

a <- lca5$parameters$unstandardized

b <- a[a$paramHeader == "Means" | a$paramHeader == "Thresholds",
       c("param", "paramHeader", "est", "LatentClass") ]
b <- b[-grep("#", b$param), ]

# make probability of choosing second cat
b$test <- exp(b$est) / (1 + exp(b$est))
b$est[b$paramHeader == "Thresholds"] <-  1 - b$test[b$paramHeader == "Thresholds"]
b$test <- NULL
b$est <- round(b$est, 2)

b <- b %>%
  tbl_df() %>%
  group_by(param, LatentClass) %>%
  mutate(param2 = str_c(param, "_", row_number() + 4)) %>%
  ungroup() %>%
  select(-param) %>%
  rename(param = param2)

# reshape, reorder and rename
b <- as.data.frame(b)
cdata <- reshape(data = b,
                 idvar = c("param", "paramHeader"),
                 direction = "wide",
                 timevar = "LatentClass")

cdata2 <- cdata %>%
  tbl_df() %>%
  set_names(c("var", "coef", paste0("class", 1:5))) %>%
  mutate(coef = str_to_lower(coef),
         coef = str_remove_all(coef, "_nomi\\$1|_resp\\$1"),
         var = "Yes%")

cdata3 <- cdata2 %>%
  gather(key = key, value = value, class1:class5)



probs <- reduce(probs, rbind) %>%
  tbl_df() %>%
  mutate(var = rep(names(probs), each = 5),
         var = str_remove_all(var, "_nomiss|non_"),
         var = str_replace(var, "f2f", "web"),
         class = rep(str_c("Class ", 1:5), 10)) %>%
  rename(Yes = `Pr(1)`, No = `Pr(2)`)

cdata3 %>%
  ggplot(aes(coef, value, color = key, group = key)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(size = 1, alpha = 0.8) +
  theme_bw()


probs <- cdata3 %>%
  tidyr::separate(coef, into = c("Outcome", "Wave"), sep = "_")

class_prop <- round(lca5$class_counts$modelEstimated$proportion*100, 1)

g <- probs %>%
  mutate(value = 1 - value,
         Outcome = as.factor(Outcome),
         Outcome = factor(Outcome,
                          labels = c("Web", "Response")),
         Outcome = fct_rev(Outcome),
         key = as.factor(key),
         key = factor(key, labels = c("Switch and non-response (4.6%)",
                                          "Fast switchers (6.5%)",
                                          "Slow switchers (11.9%)",
                                          "Face to face respondents (15.9%)",
                                          "Web respondents (61.1%)")),
         key = fct_rev(key)) %>%
  ggplot(aes(Wave, value, color = key, group = key)) +
  facet_wrap(~Outcome) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(size = 1.5, alpha = 0.8) +
  theme_bw(base_size = 18) +
  labs(y = "Probability Yes",
       color = "Class")



ggplotly(g)

g




ggsave("./output/5class_profile.png", dpi = 500)

# Class 1 - Swtich and non-response
# Class 2 - Fast switchers
# Class 3 - Slow switchers
# Class 4 - Face to face respondents
# Class 5 - Web respondents










# Import 3 step model -----------------------------------------------------

lca5_pred <- readModels("./mplus/mode_switch_lca_5_class_pred_2.out")




pred_data <- lca5_pred$savedata %>%
  tbl_df()

pred_data2 <- pred_data %>%
  select(FEMALE_4:CPROB5) %>%
  rename_all(funs(
    str_remove_all(
      str_to_lower(.),
      "\\_4")
  ))

pred_data3 <- pred_data2 %>%
  gather(-female:-age_102_,
         key = class, value = prob)

pred_data4 <- pred_data3 %>%
  mutate(female = ifelse(female == 1, "Female", "Male"),
         partner = ifelse(partner_ == 1, "Partner", "No partner"),
         area = case_when(london == 1 ~ "London",
                          north == 1 ~ "North",
                          TRUE ~ "Other"),
         urban = ifelse(urban == 1, "Yes", "No"),
         refresh = ifelse(refresh_ == 1, "Yes", "No"),
         edu = case_when(higher == 1 ~ "Higher",
                         alevel == 1 ~ "A level",
                         gcse == 1 ~ "GCSE",
                         TRUE ~ "Lower"),
         age = case_when(age_55 == 1 ~ "Age: 36-55",
                         age_75 == 1 ~ "Age: 56-75",
                         age_102_ == 1 ~ "Age: >75",
                         TRUE ~ "Age 16-35"),
         class_fct = factor(class, labels = c("Switch and non-resp",
                                              "Fast switch",
                                              "Slow switch",
                                              "F2f resp",
                                              "Web resp")))


pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% 3:4, F, T)) %>%
  drop_na(female) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(female),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Sex") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_sex.png", dpi = 500)


pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% 1:2, F, T)) %>%
  drop_na(partner) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(partner),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Partner") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_partner.png", dpi = 500)


pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1:2, 5), F, T)) %>%
  drop_na(area) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(area),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_area.png", dpi = 500)


# not sig
# pred_data4 %>%
#   drop_na(urban) %>%
#   ggplot(aes(class_fct, prob, fill = as.factor(urban))) +
#   geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
#   labs(x = "Classes",
#        y = "Probability",
#        fill = "Urban") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 10, hjust = 0.6))

pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% 1, F, T)) %>%
  drop_na(refresh) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(refresh),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Refreshment") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_refresh.png", dpi = 500)


pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1, 2), F, T)) %>%
  drop_na(edu) %>%
  mutate(edu = as.factor(edu),
         edu = factor(edu, levels = levels(edu)[c(3, 1, 2, 4)])) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(edu),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Education") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_edu.png", dpi = 500)


pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% 1, F, T)) %>%
  drop_na(age) %>%
  mutate(age = as.factor(age),
         age = factor(age, levels = levels(age)[c(1, 3, 4, 2)])) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(age),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Age") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
  scale_alpha_discrete(guide = FALSE)

ggsave(file = "./output/class_pred_age.png", dpi = 500)





# Try alluvium by class ---------------------------------------------------

class_data <- ip_data2 %>%
  select(pidp, id) %>%
  right_join(lca5$savedata, by = c("id" = "ID")) %>%
  select(pidp, C)




out3 <- out2 %>%
  select(pidp, out_5:out_9) %>%
  left_join(class_data) %>%
  count(out_5, out_6, out_7, out_8, out_9, C) %>%
  gather(value, key, -n, -C) %>%
  mutate(wave = as.numeric(str_remove(value, "out_"))) %>%
  group_by(wave) %>%
  mutate(id = row_number())



out3 %>%
  mutate(key = as.factor(key),
         key = factor(key, levels = levels(key)[c(3, 1, 2)]),
         C = factor(C, labels = c("Switch and non-response (4.6%)",
                                    "Fast switchers (6.5%)",
                                    "Slow switchers (11.9%)",
                                    "Face to face respondents (15.9%)",
                                    "Web respondents (61.1%)")),
         C = fct_rev(C))%>%
  ggplot(aes(x = wave, stratum = key, alluvium = id,
             y = n,
             fill = key, label = key)) +
  facet_wrap(~C, scales = "free", ncol = 1) +
  #  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  theme_tufte(base_size = 18) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Wave",
       y = "Frequency",
       fill = "Outcome") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#999999"))

ggsave(file = "./output/mode_switch_class.png", dpi = 500,
       width = 7, height = 10)





# save info ---------------------------------------------------------------


save(probs, ip_data3, lca_fit_mplus,
     file = "./data/ip_data_clean.RData")




# make descriptives -------------------------------------------------------

load("./data/ip_data_clean.RData")


# participation -----------------------------------------------------------

out_data <- out2 %>%
  select(matches("out_")) %>%
  map(desc_tab) %>%
  reduce(rbind) %>%
  tbl_df() %>%
  filter(Code != "NA") %>%
  mutate(Wave = rep(5:9, each = 3))

out_data <- data.frame(out_data)

outcome_save <- reshape(data = out_data,
        timevar = "Wave",
        sep = "_",
        idvar = "Code",
        direction = "wide") %>%
  tbl_df() %>%
  rename_all(~str_remove(., "\\.")) %>%
  mutate(Code = fct_relevel(Code, "Web")) %>%
  arrange(Code) %>%
  select(Code, matches("Freq"))


write_csv(outcome_save, "./output/outcome_table.csv")



# independent variables ---------------------------------------------------


ctrl4 <- control_data3 %>%
  select(pidp, matches("_4"))

ctrl9 <- control_data3 %>%
  select(pidp, matches("_9"))


ctrl4_table <- ctrl4 %>%
  rename_all(~str_remove(., "_4")) %>%
  semi_join(out2, by = "pidp") %>%
  select(-pidp) %>%
  sum_tab(give_data_name = F)

ctrl9_table <- ctrl9 %>%
  rename_all(~str_remove(., "_9")) %>%
  semi_join(out2, by = "pidp") %>%
  select(edu, agecat,
         female, partner, urban, london, north) %>%
  sum_tab(give_data_name = F)


write_csv(ctrl4_table, "./output/desc_w4.csv")
write_csv(ctrl9_table, "./output/desc_w9.csv")



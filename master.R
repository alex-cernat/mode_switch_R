#############################################################
#
# Project looking at how epopel change modes in time
#
# RQ1. What are the underlying classes of mode switching in a
# sequential mixed-mode design?
#
# RQ2. What types of respondents and households are found in
# each mode switch class?
#
# RQ3. Can the identified mode switch classes predict future mode
# of interview and nonresponse?
#
#############################################################

# clear working space
rm(list = ls ())



# Admin -------------------------------------------------------------------

# create folders and unzip
# dir.create("./data")
# dir.create("./output")
# dir.create("./functions")


# install packages and load

# use packrat to install packages locally

pkg <- c("tidyverse", "ggthemes", "haven",
         "lme4", "rmarkdown", "reshape2",
         "MplusAutomation", "poLCA", "plotly", "ggpubr",
         "texreg", "broom", "ggalluvial", "viridis")

#install.packages(pkg)
sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)



# colpal <- c("#E69F00", "#56B4E9", "#999999")


# get ind mode
indall_data <- list.files("./data/stata13/",
           pattern = "indall", full.names = T)

i <- 5
ip_all <- map(indall_data[5:10], function(x) {

  temp <- read_dta(x) %>%
    rename_all(funs(str_remove(., "^[d-j]_"))) %>%
    select(pidp, hidp, indmode) %>%
      mutate(wave = i)

    i <<- i + 1

    temp

  })

ip_all



# get ind outcome
indsamp_data <- list.files("./data/stata13/",
                          pattern = "indsamp", full.names = T)

i <- 5
ip_samp <- map(indsamp_data[4:9], function(x) {

  temp <- read_dta(x) %>%
    rename_all(funs(str_remove(., "^[d-j]_"))) %>%
    select(pidp, ivfio) %>%
    mutate(wave = i)

  i <<- i + 1

  temp

})

ip_samp




# get mode design
hhsamp_paths <- list.files("./data/stata13/",
                          pattern = "hhsamp", full.names = T)

i <- 5
hhsamp_data <- map(hhsamp_paths[5:10], function(x) {

  temp <- read_dta(x) %>%
    rename_all(funs(str_remove(., "^[d-j]_"))) %>%
    select(hidp, matches("gridmodew"), matches("ivfho")) %>%
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
    setNames(c("hidp", "mm", "ivfho", "wave"))
}
)

# make super long data
ip_long <- reduce(ip_all, rbind)
ip_samp_long <- reduce(ip_samp, rbind)
hhsamp_long <- reduce(hhsamp_data, rbind)

out_long <- left_join(ip_long, hhsamp_long, by = c("hidp", "wave")) %>%
  left_join(ip_samp_long, by = c("pidp", "wave"))

# keep only people who were in the mixed mode in wave 5
desc_tab(out_long$mm)
count(out_long, ivfio)

out_long2 <- out_long %>%
  mutate(keep = ifelse(mm == 3 & wave == 5, 1, NA)) %>%
  group_by(pidp) %>%
  mutate(keep1 = sum(keep, na.rm = T)) %>%
  ungroup() %>%
  filter(keep1 == 1)

out_long2 %>%
  count(wave, mm)


# exclude if youth in any wave (378 individuals)
out_long3 <- out_long2 %>%
  mutate(keep = ifelse(ivfio %in% c(21:25, 53), 1, NA)) %>%
  group_by(pidp) %>%
  mutate(keep1 = sum(keep, na.rm = T)) %>%
  ungroup() %>%
  filter(keep1 == 0)


# check
count(out_long3, ivfio)
count(out_long3, indmode, mm)


# exclude people that were moved to single mode face to face:
# 61 people in wave 8, 49 in wave 9 and 39 in wave 10
# 63 people in total
out_long4 <- out_long3 %>%
  mutate(keep = ifelse(mm == 1, 1, NA)) %>%
  group_by(pidp) %>%
  mutate(keep1 = sum(keep, na.rm = T)) %>%
  ungroup() %>%
  filter(keep1 == 0)

count(out_long4, wave, mm)

# exclude people that answered by telephone
# 7 in wave 6, 3 in wave 7, 14 in wave 8, 15 in 9 - 33 people in total
out_long5 <- out_long4 %>%
  mutate(keep = ifelse(indmode == 2, 1, NA)) %>%
  group_by(pidp) %>%
  mutate(keep1 = sum(keep, na.rm = T)) %>%
  ungroup() %>%
  filter(keep1 == 0) %>%
  select(-keep, -keep1)

count(out_long5, wave, indmode)


count(out_long5, ivfio)


# make non-response variable and code missing
out_long5 <- out_long5 %>%
  mutate(f2f = case_when(indmode == 1 ~ 1,
                         indmode == 3 ~ 0),
         f2f_nomiss = ifelse(is.na(f2f), 0, f2f),
         non_resp = ifelse(ivfio == 1, 0, 1),
         non_resp2 = case_when(ivfio == 1 ~ 0,
                               ivfio %in% c(10, 81) ~ 1,
                               TRUE ~ 2),
         out = case_when(f2f == 1 & non_resp2 == 0 ~ "F2f",
                         f2f == 0 & non_resp2 == 0 ~ "Web",
                         non_resp2 == 1 ~ "Refusal",
                         non_resp2 == 2 ~ "Other non-resp",
                         non_resp2 == 3 ~ "Not-issued"))


count(out_long5, f2f, f2f_nomiss, indmode)
count(out_long5, non_resp, non_resp2)
count(out_long5, out, ivfio)



# approach from initial draft paper where we look only at eligible respondents

# out_long2 <- out_long2 %>%
#   tbl_df() %>%
#   group_by(pidp) %>%
#   mutate(sum_var = max(row_number())) %>%
#   ungroup() %>%
#   filter(sum_var == 6)




out_long5 <- tbl_df(out_long5) %>%
  dplyr::select(pidp, wave, everything())

# make long data

out_long5 <- as.data.frame(out_long5)
out <-
  reshape(
    data = out_long5,
    timevar = "wave",
    sep = "_",
    idvar = "pidp",
    direction = "wide"
  )


out <- tbl_df(out) %>%
  arrange(pidp)

# code all missing as not issued

out <- out %>%
  mutate_at(vars(starts_with("non_resp2")),
            ~ifelse(is.na(.), 3, .))


write.csv(file = "./data/ip_wide_out2.csv", out)





# Code new outcomes -------------------------------------------------------



# get outcome again for cases that were excluded


ip_samp_flat <- ip_samp %>%
  map(function(x) {
    w <- mean(x[["wave"]])
    str_c("ivfio_", w)
    x %>%
      setNames(c("pidp", str_c("ivfio_", w), "wave")) %>%
      select(-wave)
  }) %>%
  reduce(full_join)


out2 <- out %>%
  select(-matches("ivfio")) %>%
  left_join(ip_samp_flat)

count(out2, out_7, ivfio_7) %>%
  print(n = 50)


out2 <- out2 %>%
  mutate(
    out_6 = case_when(
    is.na(out_6) & ivfio_6 %in% c(10, 50, 81) ~ "Refusal",
    is.na(out_6) & ivfio_6 %in% c(12, 32, 52, 53, 57) ~ "Other non-resp",
    ivfio_6 == 1 & indmode_6 == 1 ~ "F2f",
    ivfio_6 == 1 & indmode_6 == 3 ~ "Web",
    out_6 == "F2f" ~ "F2f",
    out_6 == "Web" ~ "Web",
    out_6 == "Refusal" ~ "Refusal",
    out_6 == "Other non-resp" ~ "Other non-resp",
    TRUE ~ "Not-issued"),

    out_7 = case_when(
      is.na(out_7) & ivfio_7 %in% c(10, 50, 81) ~ "Refusal",
      is.na(out_7) & ivfio_7 %in%
        c(11, 12, 15, 32, 51, 52, 53, 57) ~ "Other non-resp",
      ivfio_7 == 1 & indmode_7 == 1 ~ "F2f",
      ivfio_7 == 1 & indmode_7 == 3 ~ "Web",
      out_7 == "F2f" ~ "F2f",
      out_7 == "Web" ~ "Web",
      out_7 == "Refusal" ~ "Refusal",
      out_7 == "Other non-resp" ~ "Other non-resp",
      TRUE ~ "Not-issued"),

    out_8 = case_when(
      is.na(out_8) & ivfio_8 %in% c(10, 50, 81) ~ "Refusal",
      is.na(out_8) & ivfio_8 %in%
        c(11, 12, 14, 15, 32, 51, 52, 53, 57) ~ "Other non-resp",
      ivfio_8 == 1 & indmode_8 == 1 ~ "F2f",
      ivfio_8 == 1 & indmode_8 == 3 ~ "Web",
      out_8 == "F2f" ~ "F2f",
      out_8 == "Web" ~ "Web",
      out_8 == "Refusal" ~ "Refusal",
      out_8 == "Other non-resp" ~ "Other non-resp",
      TRUE ~ "Not-issued"),

    out_9 = case_when(
      is.na(out_9) & ivfio_9 %in% c(10, 50, 81) ~ "Refusal",
      is.na(out_9) & ivfio_9 %in%
        c(9, 11, 12, 14, 15, 32, 51, 52, 53, 57) ~ "Other non-resp",
      ivfio_9 == 1 & indmode_9 == 1 ~ "F2f",
      ivfio_9 == 1 & indmode_9 == 3 ~ "Web",
      out_9 == "F2f" ~ "F2f",
      out_9 == "Web" ~ "Web",
      out_9 == "Refusal" ~ "Refusal",
      out_9 == "Other non-resp" ~ "Other non-resp",
      TRUE ~ "Not-issued"))









out3 <- out2 %>%
  select(pidp, out_5, out_6, out_7, out_8, out_9)  %>%
  count(out_5, out_6, out_7, out_8, out_9) %>%
  gather(value, key, -n) %>%
  mutate(wave = as.numeric(str_remove(value, "out_"))) %>%
  group_by(wave) %>%
  mutate(id = row_number()) %>%
  ungroup()




out3 %>%
  mutate(key = as.factor(key),
         key = fct_relevel(key, "Web", "F2f", "Refusal",
                           "Other non-resp")) %>%
  ggplot(aes(x = wave, stratum = key,
             alluvium = id,
             y = n,
             fill = key, label = key)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  theme_tufte(base_size = 18) +
  labs(x = "Wave",
       y = "Frequency",
       fill = "Outcome") +
  scale_fill_viridis_d(direction = -1)

ggsave(file = "./output/mode_switch2.png", dpi = 500)






# cleaned data in wave 4 ---------------------------------------------------

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


# Get new ip4 info --------------------------------------------------------


ip4 <- read_dta("./data/stata13/d_indresp_ip.dta")
ip4_samp <- read_dta("./data/stata13/d_indsamp_ip.dta")

# not found: pcnet,
# many missing: oprlg, (new entrents only) nbrcoh1 (branch experiment),  volfreq
#
# OK : netpuse, mobuse, jbstat/jbhas, volun, chargv, vote1, mplike
# wave2 mode, inerviewer observation, propensity w4


ip4_extra <- ip4 %>%
  rename_all(~str_remove(., "d_")) %>%
  mutate(net_d = ifelse(netpuse == 1, 1, 0),
         net_n = ifelse(netpuse %in% 6:7, 1, 0),
         net_s = ifelse(netpuse %in% 2:5, 1, 0),
         net_m = ifelse(netpuse < 0, 1, 0),
         mobile = ifelse(mobuse == 1, 1, 0),
         work = ifelse(jbhas == 1, 1, 0),
         vol_y = ifelse(volun == 1, 1, 0),
         vol_m = ifelse(volun < 0, 1, 0),
         don_y = ifelse(chargv == 1, 1, 0),
         don_m = ifelse(chargv < 0, 1, 0),
         vote_y = ifelse(vote1 == 1, 1, 0),
         vote_m = ifelse(vote1 < 0, 1, 0),
         p_web = ifelse(mplike_a == 4 | mplike_b == 4, 1, 0),
         p_f2f = ifelse(mplike_a == 1 | mplike_b == 1, 1, 0),
         p_tel = ifelse(mplike_a == 2 | mplike_b == 2, 1, 0),
         p_mail = ifelse(mplike_a == 3 | mplike_b == 3, 1, 0),
         p_other = ifelse(mplike_a %in% 1:4 | mplike_b %in% 1:4, 0, 1),
         coop = ifelse(ivcoop == 1, 1, 0),
         coop_m = ifelse(ivcoop < 0, 1, 0),
         suspi_n = ifelse(susp == 1, 1, 0),
         suspi_m = ifelse(susp < 0, 1, 0)) %>%
  select(pidp, hidp, net_d:suspi_m)




# get experimental group wave 2

ip4_all <- read_dta("./data/stata13/d_hhsamp_ip.dta")

ip4_exp <- ip4_all %>%
  rename_all(~str_remove(., "d_")) %>%
  mutate(mm2 = ifelse(ff_modew2 %in% 2:3, 1, 0)) %>%
  select(hidp, mm2)

ip4_extra <- left_join(ip4_extra, ip4_exp, by = "hidp")


# get call record data

call_dat <- read_dta("./data/stata13/d_callrec_ip.dta")

call_dat2 <- call_dat %>%
  rename_all(~str_remove(., "d_")) %>%
  group_by(hidp) %>%
  mutate(ncalls = max(row_number())) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(hidp, ncalls) %>%
  mutate(ncall2 = ifelse(ncalls < 3, 1, 0),
         ncall6 = ifelse(ncalls < 6 & ncalls > 2, 1, 0),
         ncall30 = ifelse(ncalls > 5, 1, 0))

# results
call_dat2 %>%
  count(ncalls, ncall2, ncall6, ncall30) %>%
  print(n = 27)


# add to all the control data
control_data4 <- full_join(control_data3, ip4_extra, by = "pidp") %>%
  left_join(call_dat2, by = "hidp")


# Make outcomes wave 10 ---------------------------------------------------


ip10 <- read_dta("./data/stata13/j_indall_ip.dta")
ip10 <- ip10 %>%
  rename_all(funs(str_remove(., "j_"))) %>%
  select(pidp, hidp, indmode, ivfio) %>%
  filter(!ivfio %in% c(21:25, 63)) %>% # exclude children
  filter(indmode != 2) %>% # exclude telephone
  mutate(f2f = case_when(indmode == 1 ~ 1,
                         indmode == 3 ~ 0),
         f2f_nomiss = ifelse(is.na(f2f), 0, f2f),
         non_resp = ifelse(ivfio == 1, 0, 1),
         f2f_offer = case_when(f2f == 1 | non_resp == 1 ~ 1,
                               f2f == 0 ~ 0)) %>%
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

ip_data <- full_join(out2, control_data4, by = "pidp") %>%
  full_join(ip10, by = "pidp") %>%
  mutate(id = row_number())

# fix space in name
ip_data <- rename_all(ip_data, ~str_replace(., "A level", "alevel"))

save(ip_data, file = "./data/ip_data_clean2.RData")



# Export to Mplus ---------------------------------------------------------



# make dummies
rec_vars <- select(ip_data, matches("out_"), -out_10) %>%
  map2(5:9, function(x, y) {
    x %>%
      as.factor() %>%
      dummyfy() %>%
      tbl_df() %>%
      rename(f2f2 = F2f, othernr = `Other non-resp`,
             refuse = Refusal, web = Web) %>%
      rename_all(~str_c(., "_", y))
  }
) %>%
  reduce(cbind) %>%
  tbl_df() %>%
  rename_all(~str_replace(., "Not-issued", "nissue"))


# add new vars
ip_data <- cbind(ip_data, rec_vars) %>%
  tbl_df()

# check results
ip_data %>%
  count(out_6, f2f2_6, nissue_6, othernr_6, refuse_6, web_6)


ip_data2 <- ip_data %>%
  mutate_all(funs(as.numeric(.)))

prepareMplusData(ip_data2, "./mplus/ip_data2.dat")


# Import Mplus results ----------------------------------------------------


lca2 <- readModels("./mplus/mode_switch_lca_2.out")
lca3 <- readModels("./mplus/mode_switch_lca_3.out")
lca4 <- readModels("./mplus/mode_switch_lca_4.out")
lca5 <- readModels("./mplus/mode_switch_lca_5.out")
lca6 <- readModels("./mplus/mode_switch_lca_6.out")


# fit table

info <- c("Observations", "Parameters",
          "LL", "AIC", "BIC", "Entropy")



mplus_lca_fit <- function(x,
                          info = c("Observations",
                                   "Parameters",
                                    "LL", "AIC", "BIC", "Entropy")) {
  x$summaries %>%
    tbl_df() %>%
    select(info) %>%
    rename(n = Observations,) %>%
    select(LL, Parameters, n, everything())

}

mplus_res <- list(lca2, lca3, lca4, lca5, lca6)

lca_fit_mplus <- map(mplus_res, mplus_lca_fit) %>%
  reduce(rbind) %>%
  mutate(Classes = 2:6) %>%
  select(Classes, everything())


write_csv(lca_fit_mplus, "./output/lca_fit_mplus.csv")


# link with other data
ip_data3 <- lca5$savedata %>%
  tbl_df() %>%
  select(ID:C) %>%
  rename_all(funs(str_to_lower(.))) %>%
  right_join(ip_data)




# regression models -------------------------------------------------------



# make factor from class
ip_data3 <- ip_data3 %>%
  mutate(class = as.factor(str_c("Class ", c)),
         class = relevel(class, ref = 5))


# make indicator if someone missed any wave
ip_data3 <- ip_data3 %>%
  rowwise() %>%
  mutate(cum_nonresp = sum(
    ifelse(out_5 %in% c("Refusal", "Other non-resp"), 1, 0),
    ifelse(out_6 %in% c("Refusal", "Other non-resp"), 1, 0),
    ifelse(out_7 %in% c("Refusal", "Other non-resp"), 1, 0),
    ifelse(out_8 %in% c("Refusal", "Other non-resp"), 1, 0),
    ifelse(out_9 %in% c("Refusal", "Other non-resp"), 1, 0),na.rm = T),
         any_nonresp = ifelse(cum_nonresp > 0, 1, 0))




res1 <- glm("non_resp ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp",
  data = ip_data3, family = binomial())


res2 <- glm("non_resp ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp + class",
           data = ip_data3, family = binomial())



summary(res1)
summary(res2)


res3 <- glm("f2f ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp", family = binomial(),
           data = ip_data3)

res4 <- glm("f2f ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp + class",
             data = ip_data3, family = binomial())


summary(res3)
summary(res4)


count(ip_data3, non_resp, f2f, f2f_offer)


res5 <- glm("f2f_offer ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp", family = binomial(),
            data = ip_data3)

res6 <- glm("f2f_offer ~ edu_9 + agecat_9 +
          female_9 + partner_9 + urban_9 + london_9 + north_9 +
           f2f_nomiss_9 + any_nonresp + class",
            data = ip_data3, family = binomial())


summary(res5)
summary(res6)






# rmsea

res_list <- list(res1, res2, res3, res4, res5, res6)
rmsea_vct <- map(res_list, function(x) qpcR::RMSE(x)) %>%
  unlist()

map(res_list, car::vif)


res_df <- map_df(res_list, glance) %>%
  mutate(rmsea = rmsea_vct)

res_df


# labels
# Class 5 = "Web respondents"
# Class 2 = "Face to face respondents"
# Class 1 = "Single mode/early drop-offs"  "Early drop-offs"
# Class 3 = "Face to face/late drop-offs"
# Class 4 = "Switchers"

labs_res1 <- c("Intercept", "Edu: A level", "Edu: GCSE",
               "Edu: Other", "Age: 36-55", "Age: 56-75",
               "Age: >75", "Female", "Partner", "Urban",
               "London", "North", "Wave 9 f2f", "Any non-response",
               "Class: Face to face resp.",
               "Class: Face to face/late drop-offs",
               "Class: Switchers")

texreg(res_list,
        caption = "Explaining response and mode in wave 10",
        custom.model.names = c("Non-response m1",
                               "Non-response m2",
                               "Face to face m1",
                               "Face to face m2",
                               "Offered face to face m1",
                               "Offered face to face m2"),
        custom.coef.names = c(labs_res1),
        custom.note = "Degree, under 36, rural, male, wave in wave 9 and
        class 1 are refrences")

# r squares

# Explained deviance

1 - (res1$deviance/res1$null.deviance)
1 - (res2$deviance/res2$null.deviance)


1 - (res3$deviance/res3$null.deviance)
1 - (res4$deviance/res4$null.deviance)







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
  gather(-female:-ncall30 ,
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
         netuse = case_when(net_d == 1 ~ "Internet: daily",
                            net_s == 1 ~ "Internet: sometimes",
                            net_m == 1 ~ "Internet: missing",
                            TRUE ~ "Internet: never"),
         mobuse = ifelse(mobile == 1, "Yes", "No"),
         employed = ifelse(work == 1, "Yes", "No"),
         vol = ifelse(vol_y == 1, "Yes", "No"),
         don = ifelse(don_y == 1, "Yes", "No"),
         vote = ifelse(vote_y == 1, "Yes", "No"),
         prefer = case_when(p_web == 1 ~ "Prefer: Web",
                            p_mail == 1 ~ "Prefer: Mail",
                            p_other == 1 ~ "Prefer: Other",
                            TRUE ~ "Prefer: Face to face/tel"),
         cooperation = ifelse(coop == 1, "Yes", "No"),
         not_susp = ifelse(suspi_n == 1, "Yes", "No"),
         class_fct = factor(class, labels = c("Single mode/early drop-offs",
                                              "Face to face",
                                              "Face to face/late drop-offs",
                                              "Switchers",
                                              "Web")),
         class_fct = factor(class_fct, levels =
                              levels(class_fct)[c(1, 3, 4,2,  5)]))



# labels
# Class 1 = "Single mode/early drop-offs"  "Early drop-offs"
# Class 4 = "Face to face respondents"
# Class 2 = "Face to face/late drop-offs"
# Class 3 = "Switchers"
# Class 5 = "Web respondents"

g1 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(4, 5), T, F),
         partner = as.factor(partner),
         partner = fct_recode(partner,
                              "Yes" = "Partner",
                              "No" = "No partner" )) %>%
  drop_na(partner) %>%
  ggplot(aes(class_fct, prob, fill = partner, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Has partner") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g1, file = "./output/class_pred_partner.png", dpi = 500)



g2 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(3), F, T)) %>%
  drop_na(area) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(area) %>% fct_rev(),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Area") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g2, file = "./output/class_pred_area.png", dpi = 500)




g3 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1, 2, 3), F, T),
         mm2 = ifelse(mm2 == 1, "Yes", "No")) %>%
  drop_na(refresh) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(mm2), alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Mixed mode \nwave 2") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g3, file = "./output/class_pred_mm2.png", dpi = 500)





g4 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1, 2), F, T),
         edu = as.factor(edu),
         edu = fct_relevel(edu, "Higher") %>% fct_rev()) %>%
  drop_na(edu) %>%
  ggplot(aes(class_fct, prob, fill = edu, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Education") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g4, file = "./output/class_pred_edu.png", dpi = 500)





g5 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(2, 5), F, T),
         age = as.factor(age),
         age = factor(age, levels = levels(age)[c(1, 3, 4, 2)]) %>%
           fct_rev(),
         age = fct_recode(age,
                          "16-25" = "Age 16-35",
                          "36-55" = "Age: 36-55",
                          "56-75" = "Age: 56-75",
                          ">75" = "Age: >75")) %>%
  drop_na(age) %>%
  ggplot(aes(class_fct, prob, fill = age, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Age") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g5, file = "./output/class_pred_age.png", dpi = 500)




g6 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1, 2), F, T),
         netuse = as.factor(netuse),
         netuse = factor(netuse, levels = levels(netuse)[c(1, 4, 3, 2)]) %>%
           fct_rev(),
         netuse = fct_recode(netuse,
                          "Daily" = "Internet: daily",
                          "Sometimes" = "Internet: sometimes",
                          "Never" = "Internet: never",
                          "Missing" = "Internet: missing")) %>%
  drop_na(netuse) %>%
  ggplot(aes(class_fct, prob, fill = netuse, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Internet use") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g6, file = "./output/class_pred_netuse.png", dpi = 500)







g7 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(2, 3, 1), F, T)) %>%
  drop_na(mobuse) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(mobuse), alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Use mobile") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g7, file = "./output/class_pred_mobuse.png", dpi = 500)




g8 <-pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(2, 5, 1), F, T)) %>%
  drop_na(employed) %>%
  ggplot(aes(class_fct, prob, fill = as.factor(vol),
             alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Volunteers") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g8, file = "./output/class_pred_vol.png", dpi = 500)


g9 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(2), F, T),
         prefer = as.factor(prefer),
         prefer = fct_relevel(prefer, "Prefer: Face to face/tel",
                              "Prefer: Web") %>%
           fct_rev(),
         prefer = fct_recode(prefer,
                          "F2f/tel" = "Prefer: Face to face/tel",
                          "Web" = "Prefer: Web",
                          "Mail" = "Prefer: Mail",
                          "Other" = "Prefer: Other")) %>%
  drop_na(prefer) %>%
  ggplot(aes(class_fct, prob, fill = prefer, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Mode preference") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g9, file = "./output/class_pred_prefer.png", dpi = 500)



g10 <- pred_data4 %>%
  mutate(Sig. = ifelse(as.numeric(class_fct) %in% c(1, 3), F, T),
         call = case_when(ncall6 == 1 ~ "Nr. calls: 3-6",
                          ncall30 == 1 ~ "Nr. calls: >6",
                          TRUE ~ "Nr. calls : 1-2"),
         call = as.factor(call),
         call = fct_relevel(call, "Nr. calls : 1-2",
                              "Nr. calls: 3-6") %>%
           fct_rev(),
         call = fct_recode(call,
                          "1-2" = "Nr. calls : 1-2",
                          "3-6" = "Nr. calls: 3-6",
                          ">6" = "Nr. calls: >6")) %>%
  drop_na(female) %>%
  ggplot(aes(class_fct, prob, fill = call, alpha = Sig.)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
  labs(x = "Classes",
       y = "Probability",
       fill = "Nr. calls") +
  theme_bw() +
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), direction = -1) +
  scale_alpha_discrete(guide = FALSE) +
  coord_flip()

ggsave(g10, file = "./output/class_pred_call.png", dpi = 500)



ggarrange(g1 + labs(y = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g2 + labs(y = "", x = "")  +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g3 + labs(y = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g4 + labs(x = "", y = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g5 + labs(y = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g6 + labs(y = "", x = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g7 + labs(y = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g8 + labs(y = "", x = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g9 + labs() +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          g10 + labs( x = "") +
            theme(text = element_text(size = 13),
                  legend.key.size = unit(0.3, 'cm')),
          ncol = 2, nrow = 5)

ggsave("./output/figs_comb.png", dpi = 700, width = 10, height = 9)

# Try alluvium by class ---------------------------------------------------

class_data <- ip_data2 %>%
  select(pidp, id) %>%
  right_join(lca5$savedata, by = c("id" = "ID")) %>%
  select(pidp, C)

class_prop <- lca5$class_counts$modelEstimated$proportion

class_prop <- round(class_prop, 3) * 100

out3 <- out2 %>%
  select(pidp, out_5:out_9) %>%
  left_join(class_data) %>%
  count(out_5, out_6, out_7, out_8, out_9, C) %>%
  gather(value, key, -n, -C) %>%
  mutate(wave = as.numeric(str_remove(value, "out_"))) %>%
  group_by(wave) %>%
  mutate(id = row_number())

# labels
# Class 5 = "Web respondents"
# Class 2 = "Face to face respondents"
# Class 1 = "Single mode/early drop-offs"  "Early drop-offs"
# Class 3 = "Face to face/late drop-offs"
# Class 4 = "Switchers"


class_nm <- c("Single mode/early drop-offs", "Face to face respondents",
              "Face to face/late drop-offs", "Switchers", "Web respondents")

class_nm2 <- str_c(class_nm, " (", class_prop, "%)")


out3 %>%
  ungroup() %>%
  mutate(key = as.factor(key),
         key = factor(key, levels = levels(key)[c(5, 1, 4, 3, 2)]),
         class = factor(C, labels = class_nm2),
         class = factor(class, levels = levels(class)[c(5, 2, 4, 3, 1)])) %>%
  ggplot(aes(x = wave, stratum = key, alluvium = id,
             y = n,
             fill = key, label = key)) +
  facet_wrap(~class,
             scales = "free", ncol = 1) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  theme_tufte(base_size = 16) +
  labs(x = "Wave",
       y = "Frequency",
       fill = "Outcome") +
  scale_fill_viridis_d(direction = -1)



ggsave(file = "./output/mode_switch_class2.png", dpi = 500,
       width = 7, height = 10)



# save info ---------------------------------------------------------------


save(ip_data3, lca_fit_mplus,
     file = "./data/ip_data_clean.RData")




# make descriptives -------------------------------------------------------

# load("./data/ip_data_clean.RData")


# participation -----------------------------------------------------------


out2 %>%
  select(out_5, out_6, out_7, out_8, out_9)  %>%
  map(table) %>%
  map(as.data.frame) %>%
  reduce(full_join, by = "Var1") %>%
  setNames(c("Outcome", str_c("Wave ", 5:9))) %>%
  arrange(desc(`Wave 5`)) %>%
  write_csv("./output/outcome_table.csv")



# independent variables ---------------------------------------------------


ctrl4 <- control_data4 %>%
  select(pidp, matches("_4"), net_d:ncall30, -ncalls)



ctrl9 <- ip_data3 %>%
  select(pidp, edu_9, agecat_9, female_9, partner_9,
         urban_9, london_9, north_9,
           f2f_nomiss_9, any_nonresp, non_resp, class)



ctrl4_table <- ctrl4 %>%
  rename_all(~str_remove(., "_4")) %>%
  semi_join(out2, by = "pidp") %>%
  select(-pidp, -Higher:-age_102) %>%
  sum_tab(give_data_name = F)

ctrl9 <- ctrl9 %>%
  rename_all(~str_remove(., "_9")) %>%
  semi_join(out2, by = "pidp")


ctrl9 <- left_join(ctrl9,
                   select(ip_data3, pidp, f2f_nomiss_9),
                   by = "pidp")

ctrl9_table <- ctrl9 %>%
  filter(!is.na(non_resp), !is.na(class)) %>%
  select(edu, agecat,
         female, partner, urban, london, north, f2f_nomiss_9, any_nonresp) %>%
  sum_tab(give_data_name = F)


write_csv(ctrl4_table, "./output/desc_w4.csv")
write_csv(ctrl9_table, "./output/desc_w9.csv")





# Make probability graphs -------------------------------------------------
#
# LCAfit <- lca5
#
# a <- lca5$parameters$unstandardized
#
# b <- a[a$paramHeader == "Means" | a$paramHeader == "Thresholds",
#        c("param", "paramHeader", "est", "LatentClass") ]
# b <- b[-grep("#", b$param), ]
#
# # make probability of choosing second cat
# b$test <- exp(b$est) / (1 + exp(b$est))
# b$est[b$paramHeader == "Thresholds"] <-  1 - b$test[b$paramHeader == "Thresholds"]
# b$test <- NULL
# b$est <- round(b$est, 2)
#
# b <- b %>%
#   tbl_df() %>%
#   group_by(param, LatentClass) %>%
#   mutate(param2 = str_c(param, "_", row_number() + 4)) %>%
#   ungroup() %>%
#   select(-param) %>%
#   rename(param = param2)
#
# # reshape, reorder and rename
# b <- as.data.frame(b)
# cdata <- reshape(data = b,
#                  idvar = c("param", "paramHeader"),
#                  direction = "wide",
#                  timevar = "LatentClass")
#
# cdata2 <- cdata %>%
#   tbl_df() %>%
#   set_names(c("var", "coef", paste0("class", 1:5))) %>%
#   mutate(coef = str_to_lower(coef),
#          coef = str_remove_all(coef, "_nomi\\$1|_resp\\$1"),
#          var = "Yes%")
#
# cdata3 <- cdata2 %>%
#   gather(key = key, value = value, class1:class5)
#
# #
# #
# # probs <- reduce(probs, rbind) %>% as.data.frame() %>%
# #   tbl_df() %>%
# #   mutate(var = rep(names(probs), each = 5),
# #          var = str_remove_all(var, "_nomiss|non_"),
# #          var = str_replace(var, "f2f", "web"),
# #          class = rep(str_c("Class ", 1:5), 10)) %>%
# #   rename(Yes = `Pr(1)`, No = `Pr(2)`)
#
# cdata3 %>%
#   ggplot(aes(coef, value, color = key, group = key)) +
#   geom_point(size = 2, alpha = 0.8) +
#   geom_line(size = 1, alpha = 0.8) +
#   theme_bw() +
#   coord_flip()
#
#
# probs <- cdata3 %>%
#   mutate(coef = str_replace(coef, "_.+_", "_")) %>%
#   tidyr::separate(coef, into = c("Outcome", "Wave"), sep = "_")
#
# class_prop <- round(lca5$class_counts$modelEstimated$proportion*100, 1)
#
#
#
# probs %>%
#   group_by(Wave, key) %>%
#   summarise(sum(value))
#
#
# g <- probs %>%
#   mutate(value = 1 - value,
#          Outcome = as.factor(Outcome),
#          Outcome = factor(Outcome,
#                           labels = c("Web", "Response")),
#          Outcome = fct_rev(Outcome),
#          key = as.factor(key),
#          key = factor(key, labels = c("Switch and non-response (4.6%)",
#                                           "Fast switchers (6.5%)",
#                                           "Slow switchers (11.9%)",
#                                           "Face to face respondents (15.9%)",
#                                           "Web respondents (61.1%)")),
#          key = fct_rev(key)) %>%
#   ggplot(aes(Wave, value, color = key, group = key)) +
#   facet_wrap(~Outcome) +
#   geom_point(size = 3, alpha = 0.8) +
#   geom_line(size = 1.5, alpha = 0.8) +
#   theme_bw(base_size = 18) +
#   labs(y = "Probability Yes",
#        color = "Class")
#
#
# g
#
#
#
#
# ggsave("./output/5class_profile.png", dpi = 500)
#
# # Class 1 - Swtich and non-response
# # Class 2 - Fast switchers
# # Class 3 - Slow switchers
# # Class 4 - Face to face respondents
# # Class 5 - Web respondents
#
#
#
#




#
#
# # alluvial 6 classes ------------------------------------------------------
#
#
# class_data6 <- ip_data2 %>%
#   select(pidp, id) %>%
#   right_join(lca6$savedata, by = c("id" = "ID")) %>%
#   select(pidp, C)
#
# class_prop <- lca6$class_counts$modelEstimated$proportion
#
# class_prop <- round(class_prop, 3) * 100
#
# out3_6 <- out2 %>%
#   select(pidp, out_5:out_9) %>%
#   left_join(class_data6) %>%
#   count(out_5, out_6, out_7, out_8, out_9, C) %>%
#   gather(value, key, -n, -C) %>%
#   mutate(wave = as.numeric(str_remove(value, "out_"))) %>%
#   group_by(wave) %>%
#   mutate(id = row_number())
#
#
#
# out3_6 %>%
#   ungroup() %>%
#   mutate(key = as.factor(key),
#          key = factor(key, levels = levels(key)[c(5, 1, 4, 3, 2)]),
#          size = case_when(C == 1 ~ class_prop[1],
#                           C == 2 ~ class_prop[2],
#                           C == 3 ~ class_prop[3],
#                           C == 4 ~ class_prop[4],
#                           C == 5 ~ class_prop[5],
#                           C == 6 ~ class_prop[6]),
#          C = factor(C, labels = str_c("Class ", 1:6,
#                                       " (", class_prop, "%)")),
#          C = fct_rev(C)) %>%
#   ggplot(aes(x = wave, stratum = key, alluvium = id,
#              y = n,
#              fill = key, label = key)) +
#   facet_wrap(~fct_reorder(C, size) %>% fct_rev(),
#              scales = "free", ncol = 1) +
#   geom_flow() +
#   geom_stratum(alpha = .5) +
#   theme_tufte(base_size = 16) +
#   labs(x = "Wave",
#        y = "Frequency",
#        fill = "Outcome") +
#   scale_fill_viridis_d(direction = -1)
#
#
#
# ggsave(file = "./output/mode_switch_class_6.png", dpi = 500,
#        width = 7, height = 10)





# graphs of predicted probabilities ---------------------------------------

#
#
#
# res_data2 <- augment(res2)
# res_data4 <- augment(res4)
# res_data6 <- augment(res6)
#
# mean(ip_data3$non_resp, na.rm = T)
# mean(res_data2$non_resp, na.rm = T)
# mean(res_data2$.hat, na.rm = T)
#
# mean(ip_data3$f2f, na.rm = T)
# mean(res_data4$f2f, na.rm = T)
# mean(res_data4$.hat, na.rm = T)
#
#
# res_data2 %>%
#   mutate(prob = exp(.fitted)/(1 + exp(.fitted)),
#          prob_rev = 1 - prob,
#          class = factor(class, labels = c("Web resp",
#                                           "Switch and non-resp",
#                                       "Fast switch",
#                                       "Slow switch",
#                                       "F2f resp"))) %>%
#   ggplot(aes(class, prob_rev)) +
#   geom_bar(stat = "summary", fun.y = "mean") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
#   labs(x = "Class",
#        y = "Predicted response probability")
#
# ggsave(file = "./output/ip10_pred_nonresp.png", dpi = 500)
#
# res_data4 %>%
#   mutate(prob = exp(.fitted)/(1 + exp(.fitted)),
#          class = factor(class, labels = c("Web resp",
#                                           "Switch and non-resp",
#                                           "Fast switch",
#                                           "Slow switch",
#                                           "F2f resp"))) %>%
#   ggplot(aes(class, prob)) +
#   geom_bar(stat = "summary", fun.y = "mean") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
#   labs(x = "Class",
#        y = "Predicted f2f probability")
#
#
# ggsave(file = "./output/ip10_pred_f2f.png", dpi = 500)
#
# res_data6 %>%
#   mutate(prob = exp(.fitted)/(1 + exp(.fitted)),
#          class = factor(class, labels = c("Web resp",
#                                           "Switch and non-resp",
#                                           "Fast switch",
#                                           "Slow switch",
#                                           "F2f resp"))) %>%
#   ggplot(aes(class, prob)) +
#   geom_bar(stat = "summary", fun.y = "mean") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 10, hjust = 0.6)) +
#   labs(x = "Class",
#        y = "Predicted f2f offer probability")
#
#
# ggsave(file = "./output/ip10_pred_f2f_offer.png", dpi = 500)
#

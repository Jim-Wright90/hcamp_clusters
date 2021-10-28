head(one_impact_test)
names(one_impact_test)
names(two_impact_test)

names(clusters_two_impact_test_complete)
names(clusters_one_impact_test_scaled_long)


test_bind <- bind_rows(one_impact_test, 
                       two_impact_test,
                       three_impact_test,
                       four_impact_test,
                       .id = "number_tests_completed")


# test_bind <- test_bind %>% 
#   mutate(test_1_age = age + age_post_injury_1)


one_test_mod_1 <- lm(post_injury_1_score ~ gender:age:symptom_cluster,
                     data = clusters_one_impact_test_scaled_long)

summary(one_test_mod_1)


# exploring merged data 

names(one_impact_sims)
head(one_impact_sims)

one_impact_sims %>% 
  count(student_id)

two_impact_sims %>% 
  count(student_id)

three_impact_sims %>% 
  count(student_id)

four_impact_sims %>% 
  count(student_id)


str(one_impact_sims)
str(two_impact_sims)
str(three_impact_sims)
str(four_impact_sims)

one_impact_sims <- one_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.factor(number_of_concussions_post_injury_1))

one_impact_sims <- one_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.numeric(number_of_concussions_post_injury_1))


two_impact_sims <- two_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.factor(number_of_concussions_post_injury_1))

two_impact_sims <- two_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.numeric(number_of_concussions_post_injury_1))


three_impact_sims <- three_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.factor(number_of_concussions_post_injury_1))

three_impact_sims <- three_impact_sims %>% 
  mutate(number_of_concussions_post_injury_1 = as.numeric(number_of_concussions_post_injury_1))

# four_impact_sims <- four_impact_sims %>% 
#   mutate(number_of_concussions_post_injury_1 = as.factor(number_of_concussions_post_injury_1))
# 
# four_impact_sims <- four_impact_sims %>% 
#   mutate(number_of_concussions_post_injury_1 = as.numeric(number_of_concussions_post_injury_1))


data_one <- one_impact_sims %>% 
  select(1:26, 52:58)

# data_two <- two_impact_sims %>% 
#   select(1:5, 7:29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 
#          71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99)

# data_three <- three_impact_sims %>% 
#   select(1:5, 8:30, contains("_post_injury_1"))
# 
# data_four <- four_impact_sims %>% 
#   select(1:5, 9:31, contains("_post_injury_1"))


#trying to add cluster and total symptom scores per Troy's suggestion
data_two <- two_impact_sims %>% 
  select(1:27, 79:92)

data_three <- three_impact_sims %>% 
  select(1:28, 105:125)

data_four <- four_impact_sims %>% 
  select(1:29, 131:158)

sims_test_bind <- bind_rows(data_one, 
                            data_two,
                            data_three,
                            data_four,
                            .id = "number_tests_completed")


sims_test_bind %>% 
  group_by(number_tests_completed) %>% 
  summarize(mean(dys_btwn_onset_rtp_7))

sims_test_bind %>% 
  group_by(number_tests_completed) %>% 
  summarize(mean(dys_btwn_onset_rtp_3))

sims_test_bind %>% 
  group_by(number_tests_completed) %>% 
  count()

str(sims_test_bind)

sims_test_bind <- sims_test_bind %>% 
  mutate(gender = as.factor(gender),
         age = as.factor(age))

sims_test_bind %>% 
  summarize(mean(total_symptom_score_post_injury_1))


# models below are early attempts with merged sims and Impact data 


test_mod_1 <- lm(dys_btwn_onset_rtp_3 ~ gender:age, data = sims_test_bind)
  
summary(test_mod_1)

head(sims_test_bind)

str(sims_test_bind)

clusters_scaled <- sims_test_bind %>% 
  select(student_id,
         headache_migraine_cluster_score_post_injury_1,
         cognitive_cluster_score_post_injury_1,
         anxiety_mood_cluster_score_post_injury_1,
         ocular_motor_cluster_score_post_injury_1,
         vestibular_cluster_score_post_injury_1,
         sleep_cluster_score_post_injury_1) %>% 
  rename(headache_migraine_test_1 = headache_migraine_cluster_score_post_injury_1,
         cognitive_test_1 = cognitive_cluster_score_post_injury_1,
         anxiety_mood_test_1 = anxiety_mood_cluster_score_post_injury_1,
         ocular_motor_test_1 = ocular_motor_cluster_score_post_injury_1,
         vestibular_test_1 = vestibular_cluster_score_post_injury_1,
         sleep_test_1 = sleep_cluster_score_post_injury_1)

clusters_scaled <- map_df(clusters_scaled, rescale01b)

sims_test_bind_joined <- bind_cols(sims_test_bind, clusters_scaled)


long_sims_scaled <- sims_test_bind_joined %>% 
  pivot_longer(
    cols = c(66:71),
    names_to = "symptom_cluster",
    values_to = "post_injury_1_score"
  )

# model to explore relationship between age, gender, total symptom score and RTL time

rtl_mod1 <- lm(dys_btwn_onset_rtp_3 ~ age:gender:total_symptom_score_post_injury_1,
               data = sims_test_bind_joined)

summary(rtl_mod1)

# model to explore relationship between age, gender, and cluster on test one severity 

rtl_mod2 <- lm(post_injury_1_score ~ age:gender:symptom_cluster,
               data = long_sims_scaled)

summary(rtl_mod2)


head(sims_concussion_data)

sims_concussion_data %>% 
  count(student_id)

view(sims_concussion_data)


## 10/25/21 Troy meeting 


# Outcome variables: 
  ## days to complete RTP
  ## days to  complete RTL 
    ### maybe focus on RTL completion time but can comment on RTP duration and RTL-RTP duration

# Predictor variables: 
  ## total symptom score
  ## age
  ## sex

### Create variables for onset-date to test-2-date, test-3-date, etc
### try to analyze change in symptom trends between test dates 

sims_test_bind %>% 
  group_by(number_tests_completed) %>% 
  summarize(mean(dys_btwn_onset_test_1))

sims_test_bind %>% 
  count(school_year)

sims_concussion_data %>% 
  count(school_year)

sims_test_bind %>% 
  count(gender)

# concussion injuries represent injuries sustained from the 2012-2013 - 2019-2020 school years


# analysis 1
  ## multiple regression analysis 
  ## what is the relationship between age, sex, and PCSS total symptom score score and 
  ## duration to complete RTL?

# follow up analysis
  ## run the anova comparing symptom cluster severity at time of test 1
  ## this idea removes the previous plan to compare symptom trajectories over the course
  ## of repetitive testing 
  ## interaction effect anova gender:symptom cluster 


## making new variables for Troy for onset to test 2, 3, and 4 dates 

sims_test_bind2 <- sims_test_bind_joined %>% 
  mutate(dys_btwn_onset_test_2 = test_date_post_injury_2 - onset,
         dys_btwn_onset_test_3 = test_date_post_injury_3 - onset,
         dys_btwn_onset_test_4 = test_date_post_injury_4 - onset,
         dys_btwn_test_1_2 = test_date_post_injury_2 - test_date_post_injury_1,
         dys_btwn_test_2_3 = test_date_post_injury_3 - test_date_post_injury_2,
         dys_btwn_test_3_4 = test_date_post_injury_4 - test_date_post_injury_3,
         dys_btwn_test_1_3 = test_date_post_injury_3 - test_date_post_injury_1,
         dys_btwn_test_1_4 = test_date_post_injury_4 - test_date_post_injury_1) %>% 
  mutate(dys_btwn_onset_test_2 = as.numeric(dys_btwn_onset_test_2),
         dys_btwn_onset_test_3 = as.numeric(dys_btwn_onset_test_3),
         dys_btwn_onset_test_4 = as.numeric(dys_btwn_onset_test_4),
         dys_btwn_test_1_2 = as.numeric(dys_btwn_test_1_2),
         dys_btwn_test_2_3 = as.numeric(dys_btwn_test_2_3),
         dys_btwn_test_3_4 = as.numeric(dys_btwn_test_3_4),
         dys_btwn_test_1_3 = as.numeric(dys_btwn_test_1_3),
         dys_btwn_test_1_4 = as.numeric(dys_btwn_test_1_4))

sims_test_bind2 %>% 
  summarize(mean(dys_btwn_onset_test_1))


sims_test_bind2 %>% 
  filter(number_tests_completed == 4) %>% 
  summarize(mean(dys_btwn_onset_test_4))

write.csv(sims_test_bind2, "clean_impact_sims_data.csv")

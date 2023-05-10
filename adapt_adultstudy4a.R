#Library
library(tidyverse)
library(janitor)
library(rstanarm)
library(na.tools)
library(here)

#Adult Data
#Load data 4a
data_4a <- read.csv(here("data", "adultdatalog4a1.csv"), na.strings = c("", "NA")) #load data

## Clean data
cleandata_4a <- data_4a %>% 
  clean_names() %>% #lowercase column names
  select(q6:language_1) %>% #remove unnecessary columns
  rename("sound_check" = "q6")

## Remove first two rows (contain no data/are unnecessary)
cleandata_4a <- cleandata_4a[-c(1,2), ]

## Data cleaning and participant exclusion
tidydata_4a <- cleandata_4a %>% 
  filter(attention_check_1 == 1,
         attention_check_2 == 1,
         sound_check == 1) %>% #exclude participants who failed attention and sound checks
  mutate(subject_id = row_number()) %>% #make a subject id column
  select(subject_id, everything()) %>% 
  select(subject_id:rast_white)

## Pivot columns
tidy4a <- tidydata_4a %>%
  pivot_longer(cols = c("clop_instrumental", "fraw_instrumental", "gobb_instrumental", "norl_instrumental", "terb_instrumental", "plip_instrumental", "surk_instrumental", "rast_instrumental",
                        "clop_silence", "fraw_silence", "gobb_silence", "norl_silence", "terb_silence", "plip_silence", "surk_silence", "rast_silence",
                        "clop_white", "fraw_white", "gobb_white", "norl_white", "terb_white", "plip_white", "surk_white", "rast_white",
                        "clop_mtb", "fraw_mtb", "gobb_mtb", "norl_mtb", "terb_mtb", "plip_mtb", "surk_mtb", "rast_mtb"),
               names_to = "activity",
               values_to = "rating") %>% 
  separate(col = activity,
           into = c("activity", "noise_type"),
           sep = '[_]') %>% 
  na.omit()

## Make rating category numeric
tidy4a$rating <- as.integer(tidy4a$rating)

##Pull out 4 activities that relate to Experiment 2
tidy_data4b_exp2 <- tidy4a %>% 
  filter(activity %in% c("fraw", "gobb", "plip", "clop")) %>%
  rename("activity_old" = "activity") %>% 
  mutate(activity = case_when(
            activity_old == "clop" ~ "terb",
            activity_old == "gobb" ~ "gobb",
            activity_old == "fraw" ~ "fraw",
            activity_old == "plip" ~ "plip"))

## Calculate confidence intervals
cidata_4a <- tidy_data4b_exp2 %>%
  group_by(activity, noise_type) %>% 
  summarise(n = n(),
            mean_rating = mean(rating, na.rm=TRUE), 
            sem = sd(rating) / sqrt(n),
            ci.l = mean_rating - sem * 1.96,
            ci.u = mean_rating + sem * 1.96) 
  #mutate(activity = fct_reorder(activity, mean_quiet, .desc = TRUE))

# Data Visualization

xlabels <- c("babble", "music", "silence", "white noise")

ggplot(data = cidata_4a, 
       mapping = aes(x = factor(noise_type, level= c("mtb", "instrumental", "silence", "white")), y = mean_rating, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,7) +
  xlab("Auditory Stimulus") +
  ylab("Rating") +
  #scale_fill_brewer(palette="Set2") +
  scale_fill_manual(values = wes_palette("GrandBudapest1")) +
  scale_x_discrete(labels = xlabels) +
  facet_wrap(~activity) +
  geom_hline(yintercept = 3.5, lty =2) +
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none",
        title = element_text(size = 8),
        axis.text = element_text(size = 8),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.5, "lines"),
        axis.text.x = element_text(angle = 30, hjust=1),
        text = element_text(size=10))


  geom_pointrange(aes(ymin = ci.l,
                      ymax = ci.u)) +
  facet_wrap(~activity) +
  coord_flip() + 
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggplot(data = tidydata_4, 
       mapping = aes(x = noise_type, y = rating, fill = activity)) +
  geom_bar() +
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

#4a1

#Load data 4a1
data_4a1 <- read.csv("adultdatalog4a1.csv", na.strings = c("", "NA"))

View(data_4a1)

## Clean data
cleandata_4a1 <- data_4a1 %>% 
  clean_names() %>% #lowercase column names
  select(q6:language_1) %>% #remove unnecessary columns
  rename("sound_check" = "q6")

## Remove first two rows (contain no data/are unnecessary)
cleandata_4a1 <- cleandata_4a1[-c(1,2), ]

## Data cleaning and participant exclusion
tidydata_4a1 <- cleandata_4a1 %>% 
  filter(attention_check_1 == 1,
         attention_check_2 == 1,
         sound_check == 1) %>% #exclude participants who failed attention and sound checks
  mutate(subject_id = row_number()) %>% #make a subject id column
  select(subject_id, everything()) %>% 
  select(subject_id:rast_white)

View(tidydata_4a1)

## Pivot columns
tidy4a1 <- tidydata_4a1 %>%
  pivot_longer(cols = c("clop_instrumental", "fraw_instrumental", "gobb_instrumental", "norl_instrumental", "terb_instrumental", "plip_instrumental", "surk_instrumental", "rast_instrumental",
                        "clop_silence", "fraw_silence", "gobb_silence", "norl_silence", "terb_silence", "plip_silence", "surk_silence", "rast_silence",
                        "clop_white", "fraw_white", "gobb_white", "norl_white", "terb_white", "plip_white", "surk_white", "rast_white",
                        "clop_mtb", "fraw_mtb", "gobb_mtb", "norl_mtb", "terb_mtb", "plip_mtb", "surk_mtb", "rast_mtb"),
               names_to = "activity",
               values_to = "rating") %>% 
  separate(col = activity,
           into = c("activity", "noise_type"),
           sep = '[_]') %>% 
  na.omit()

## Make rating category numeric
tidy4a1$rating <- as.integer(tidy4a1$rating)

## Calculate confidence intervals
cidata_4a1 <- tidy4a1 %>%
  group_by(activity, noise_type) %>% 
  summarise(n = n(),
            mean_rating = mean(rating, na.rm=TRUE), 
            sem = sd(rating) / sqrt(n),
            ci.l = mean_rating - sem * 1.96,
            ci.u = mean_rating + sem * 1.96)

# Data Visualization
ggplot(data = cidata_4a1, 
       mapping = aes(x = noise_type, y = mean_rating)) +
  geom_pointrange(aes(ymin = ci.l,
                      ymax = ci.u)) +
  facet_wrap(~activity) +
  coord_flip() + 
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggplot(data = tidydata_41, 
       mapping = aes(x = noise_type, y = rating, fill = activity)) +
  geom_bar() +
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

#make predictions using inv.logit (y = inv.logit(b0 +...)), great way to check model performance
boot::inv.logit(b[1]) #prediction of intercept

dat <- data.frame(
  "referred" = c(12, 4),
  "nonreferred" = c(11, 4),
  row.names = c("TB", "FB"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("referred", "nonreferred")

dat

test <- fisher.test(dat)
test
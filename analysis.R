###---###---###---###---###---###---
# Introduction ---------------------
###---###---###---###---###---###---

# Datafest 2024
# March 22nd-24th
# University of Edinburgh, King's Campus, Nucleus Building
# Hawthorne Teaching Room

# Team Chameleon (No. 19)
# Team members: Sophie Grant, Wendy Deng, Valtteri Vuorio


###---###---###---###---###---###---
# Load libraries and extensions ----
###---###---###---###---###---###---

library(tidyverse) #general purpose library
library(psych) #statistical tools useful for all psychological questions
library(lme4) #lme4 function for calculation linear mixed effects models
library(sjPlot) #functions for plotting linear mixed effects models
#library(effects)
library(lmeresampler) #boostrapping for linear mixed effects models
library(performance) #gives the 'r2' function for marginal and conditional R^2
library(kableExtra) #neater tables

###---###---###---###---###---###---
# Setup ----------------------------
###---###---###---###---###---###---

# Set seed for boostrapping procedures
set.seed(666)
# Set working directory to script location; helps with reading in data, as all addresses
# can be stated as "./filename.csv" instead of the full path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###---###---###---###---###---###---
# Load data ------------------------
###---###---###---###---###---###---

page_views <- read_csv("./page_views.csv") %>%
  mutate(institution_id = factor(institution_id, labels = paste0("i", c(LETTERS[1:11])))) %>%
  filter(book == "College / Advanced Statistics and Data Science (ABCD)")
eoc <- read_csv("./checkpoints_eoc.csv") %>%
  filter(book == "College / Advanced Statistics and Data Science (ABCD)")
pulse <- read_csv("./checkpoints_pulse.csv") %>%
  mutate(institution_id = factor(institution_id, labels = paste0("i", c(LETTERS[1:11])))) %>%
  filter(book == "College / Advanced Statistics and Data Science (ABCD)")
items <- read_csv("./items.csv")
responses <- read_csv("./responses.csv")
media <- read_csv("./media_views.csv") %>%
  mutate(institution_id = factor(institution_id, labels = paste0("i", c(LETTERS[1:11])))) %>%
  filter(book == "College / Advanced Statistics and Data Science (ABCD)")

###---###---###---###---###---###---
# Sanity checks --------------------
###---###---###---###---###---###---

page_views %>% #checking that each students belongs to only one institutions
  group_by(student_id, institution_id) %>%
  count() %>%
  pivot_wider(names_from = institution_id, values_from = n) %>%
  mutate(across(.cols = c("iB", "iH", "iF", "iI", "iC", "iG"), function(x) ifelse(!is.na(x), 1, 0))) %>%
  mutate(values = iB+iH+iF+iI+iC+iG) %>%
  filter(values != 1)

###---###---###---###---###---###---
# Scripts for "page_views" ---------
###---###---###---###---###---###---

top_pages <- page_views %>%
  group_by(page) %>% #Group by page
  count() %>% #count the number of instances per page; produces a new column "n"
  arrange(desc(n)) %>% #arrange them from largest to smallest by n
  ungroup() %>%
  mutate(rank = row_number()) #create a new column with row number as its value

top_pages %>%
  ggplot(., aes(x = rank, y = n)) +
  geom_point() + #adds individual values into the plot
  geom_smooth(method = "lm", se = FALSE) + #creates a linear regression plot based on x and y
  theme_bw() +
  xlab("\nPage rank") + #Modify x-axis label, \n adds a little space between the graph and text
  ylab("Page views\n") +
  ggtitle("Number of page views as a function of page rank.\n")

page_views %>%
  mutate(was_complete = factor(case_when( #values need to be renamed for pivot_wider (user later)
    was_complete == FALSE ~ "FALSE", #when "was_complete" is FALSE, give it a name "FALSE"
    TRUE ~ "TRUE"))) %>% #in all other cases, its names will be "TRUE"
  group_by(chapter_number, was_complete) %>%
  summarise(counts = n()) %>% #for each chapter_number and was_complete condition, count number of rows
  na.omit() %>% #omit rows which have any NAs in them
  pivot_wider( #convert the data from long to wide format
    names_from = was_complete, #convert one column to how many values are in "was_complete"
    values_from = counts) %>% #new column values will be derived from "counts"
  mutate(prop = `TRUE` / (`TRUE` + `FALSE`)) %>% #count the proportion of TRUE statements
  ggplot(., aes(x = chapter_number, y = prop)) +
  geom_col(color = "black", fill = "steelblue3") +
  scale_y_continuous(labels = scales::percent) + #convert y-axis scale from 0-1 to 0-100%
  theme_bw() +
  theme(legend.position = "none") + #remove legends
  xlab("\nChapter number") +
  ylab("Repeat visits (%)") +
  ggtitle("What percentage of total visits to a certain chapter were repeat visits?\n")

page_views %>%
  mutate(dt_accessed = as.numeric( #takes a substring of time of day (0-23) and converts it into a number
    substring(dt_accessed, first = 12, last = 13))) %>% #e.g. "2024-14-03 03:12:25" turns into "03" and then 3.
  group_by(dt_accessed) %>% #group by time of day
  count() %>%
  ggplot(., aes(x = dt_accessed, y = n)) +
  geom_col(fill = "steelblue3", color = "black") +
  xlab("Hour") +
  ylab("Number of visits") +
  ggtitle("Number of visits by students at different times of day.\n") +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20, 23)) + #manually insert x-axis ticks
  theme_bw() +
  theme(legend.position = "none")

page_views %>%
  group_by(chapter_number) %>%
  distinct(., student_id, .keep_all = TRUE) %>% #keep unique entries of "student_id", preserve other columns
  count() %>%
  ggplot(., aes(x = chapter_number, y = n)) +
  geom_col(color = "black", fill = "steelblue3") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("\nChapter number") +
  ylab("Number of students") +
  ggtitle("Number of students engaging with each book chapter.\n") +
  scale_x_continuous(breaks = seq(from = 1, to = 16, by = 1)) #create ticks from 1 to 16 in increments of 1

###---###---###---###---###---###---
# Plot EOC by chapter --------------
###---###---###---###---###---###---

ggplot(eoc, aes(x = chapter_number, y = EOC, group = chapter_number)) +
  geom_violin(color = "black", fill = "steelblue3") +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("\nChapter number") +
  ylab("EOC\n") +
  ggtitle("EOC (%) by chapter in College / Advanced Statistics and Data Science (ABCD)\n") +
  scale_x_continuous(breaks = seq(from = 1, to = 16, by = 1)) +
  scale_y_continuous(labels = scales::percent)


###---###---###---###---###---###---
# Data wrangling for full_join -----
###---###---###---###---###---###---

#In this block, each full dataset will be trimmed down to only the most essential columns
#so when they are concatenated the end product will be as small as possible

eoc_final <- eoc %>%
  select(student_id, chapter_number, EOC)

pulse_final <- pulse %>%
  select(institution_id, student_id, chapter_number, construct, response) %>%
  group_by(institution_id, student_id, chapter_number, construct) %>%
  summarise(response = mean(response, na.rm = T)) %>%
  pivot_wider(names_from = construct, values_from = response)

pages_final <- page_views %>%
  group_by(institution_id, student_id, chapter_number, was_complete) %>%
  summarise(engaged = sum(engaged, na.rm = T)/3600000) %>% #from milliseconds to  hours
  mutate(engaged = case_when(is.na(engaged) | is.nan(engaged) ~ 0, #if NA or NaN, then 0
                             TRUE ~ engaged)) %>%
  na.omit() %>%
  pivot_wider(names_from = was_complete, values_from = engaged) %>%
  mutate(visit = `FALSE`,
         revisit = `TRUE`) %>%
  select(-c(`FALSE`, `TRUE`)) %>%
  mutate(visit = case_when(is.na(visit) ~ 0,
                           TRUE ~ visit),
         revisit = case_when(is.na(revisit) ~ 0,
                             TRUE ~ revisit))

responses_final <- responses %>%
  select(student_id, chapter_number, item_type, points_possible, points_earned) %>%
  group_by(student_id, chapter_number, item_type) %>%
  filter(points_possible != 0) %>%
  summarise(prop = sum(points_earned, na.rm = T)/sum(points_possible, na.rm=T)) %>%
  pivot_wider(names_from = item_type, values_from = prop) %>%
  mutate(learnosity_activity = case_when(is.nan(`learnosity-activity`) ~ NA,
                                         TRUE ~ `learnosity-activity`)) %>%
  select(-`learnosity-activity`)

media_final <- media %>%
  filter(!is.na(proportion_video)) %>%
  group_by(student_id, chapter_number) %>%
  summarise(mean_proportion_video = mean(proportion_video, na.rm = TRUE))

lrn_final <- responses %>%
  filter(item_type == "learnosity" & points_possible > 0) %>%
  group_by(student_id, chapter_number, lrn_type) %>%
  summarise(prop = sum(points_possible, na.rm = T)) %>%
  group_by(student_id, chapter_number) %>%
  mutate(prop = prop/sum(prop)) %>%
  pivot_wider(names_from = lrn_type, values_from = prop) %>%
  mutate(across(.cols = c(mcq, plaintext, #mutate each of these columns in one go
                          choicematrix,
                          shorttext,
                          association,
                          clozeassociation,
                          imageclozeassociation),
                function(x) ifelse(is.na(x), 0, x))) #if column value is NA, make it 0

df <- full_join(eoc_final, #this will be joined last
                full_join(media_final,
                          full_join(lrn_final,
                                    full_join(responses_final,
                                              full_join(pages_final, #starts from here
                                                        pulse_final))))) %>%
  filter(!is.na(EOC)) %>% #since we are predicting EOC, columns w/o this are useless
  transmute(institution_id = institution_id, #note 'transmute' instead of 'mutate'
            student_id = student_id,
            chapter = chapter_number,
            visit = visit,
            revisit = revisit,
            media = mean_proportion_video,
            EOC = EOC,
            Cost = Cost,
            Expectancy = Expectancy,
            Intrinsic = `Intrinsic Value`,
            Utility = `Utility Value`,
            code = code,
            learnosity = learnosity,
            mcq = mcq,
            plaintext = plaintext,
            choicematrix = choicematrix,
            shorttext = shorttext,
            association = association,
            clozeassociation = clozeassociation,
            imageclozeassociation = imageclozeassociation)

###---###---###---###---###---###---
## Filtering outliers --------------
###---###---###---###---###---###---

#Since there are few values which are not representative of the whole population
#these will be filtered if the absolute standardised value is over 4

df <- df %>%
  mutate(std_visit = scale(visit, #create new standardised visit value
                           scale = T,
                           center = T),
         std_revisit = scale(revisit,
                             scale = T,
                             center = T),
         outlier_revisit = case_when(abs(std_revisit) < 4 ~ FALSE, #is the column an outlier
                                     TRUE ~ TRUE),
         outlier_visit = case_when(abs(std_visit) < 4 ~ FALSE,
                                   TRUE ~ TRUE)) %>%
  filter(std_visit < 4 & std_revisit < 4) #filter columns which are outliers in both

###---###---###---###---###---###---
## Summary statistics --------------
###---###---###---###---###---###---

df %>%
  mutate(chapter = chapter-1) %>%
  summarise(Cost = paste0(sprintf("%.2f", mean(Cost, na.rm = T)), " (", sprintf("%.2f", sd(Cost, na.rm = T)), ")"),
            Utility = paste0(sprintf("%.2f", mean(Utility, na.rm = T)), " (", sprintf("%.2f", sd(Utility, na.rm = T)), ")"),
            Expectancy = paste0(sprintf("%.2f", mean(Expectancy, na.rm = T)), " (", sprintf("%.2f", sd(Expectancy, na.rm = T)), ")"),
            Intrinsic = paste0(sprintf("%.2f", mean(Intrinsic, na.rm = T)), " (", sprintf("%.2f", sd(Intrinsic, na.rm = T)), ")"))

###---###---###---###---###---###---
## Model building ------------------
###---###---###---###---###---###---

mdl <- lmer(EOC ~ 1 + visit + revisit #fixed effects
            + (1 | institution_id/student_id), data = df, #random effects
            control = lmerControl("nloptwrap", #computational instructions for the optimizer
                                  boundary.tol = 2e-10,
                                  optCtrl = list(maxfun = 2e10)))

r2(mdl) #produces marginal and conditional R^2 for models
mdlBS <- bootstrap(mdl, #model that is bootstrapped
                   .f = fixef, #takes fixed effect values from the model
                   type = "case", #case-dependent bootstrapping
                   B = 2000, #number of samples
                   resample = c(FALSE, #institutions will NOT be resampled
                                TRUE, #students WILL be resampled
                                FALSE)) #EOC values will NOT be resampled

#Extract 95% confidence intervals from the model
mdlconf <- as.data.frame(confint(mdlBS,
                                 type = "basic",
                                 level = 0.95)[,1:4])

mdl2 <- lmer(EOC ~ 1 + visit + revisit + Expectancy + Intrinsic + Utility +
                + (1 | institution_id/student_id), data = df,
                control = lmerControl("nloptwrap", boundary.tol = 2e-10, optCtrl = list(maxfun = 2e10)))

mdl3 <- lmer(EOC ~ 1 + visit + revisit + media + Expectancy + Intrinsic + Utility + 
               (1 | institution_id/student_id), data = df2,
             control = lmerControl("nloptwrap", boundary.tol = 2e-10, optCtrl = list(maxfun = 2e10)))

plot_model(mdl3, type = "est") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  ylim(-0.1, 0.1) +
  theme_bw() +
  ggtitle("")

as.data.frame(mdlconf) %>%
  kbl(digits = 2,
      escape = F,
      align = "c",
      col.names = c("Coefficient", "Estimate", "0.5 %", "99.5 %"),
      caption = "Bootstrapped model coefficients with 99 %  Confidence Intervals.",
      booktabs = T,
      linesep = "") %>%
  kable_paper(c("hover", "responsive")) %>%
  kable_styling(font_size = 10,
                latex_options = "HOLD_position")

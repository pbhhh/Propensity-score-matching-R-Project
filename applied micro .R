rm(list = ls()) # remove all objects, start from scratch
cat("\014")

# Loading Useful Packages
install.packages("MatchIt")
library(tidyverse) 
library(huxtable) 
library(haven) 
library(gridExtra)
library(lfe) 
library(dplyr)
library(MatchIt)
setwd("/Users/peibihong/Desktop") 
books <- read_stata("books_data.dta")
books_summary <- books %>% 
  # this will only keep the first observation of each book (books are indexed by id). 
  # The exact year depends on how the data is sorted
  distinct(id, .keep_all = TRUE) %>% #.keep_all means "keep variables other than id"
  group_by(field, brp) %>%
  summarise(number_books = n()) %>%
  mutate(brp = recode(brp,  `0` = "non_BRP", `1` = "BRP"))

books_cities <- books %>% 
  distinct(id, .keep_all = TRUE) %>% #.keep_all means "keep variables other than id"
  filter(is_empty(city)==FALSE) %>%
  group_by(city) %>%
  summarise(number_books = n())
#a. The total number of BRP and Swiss books.
books_brp <- books %>% 
  distinct(id, .keep_all = TRUE) %>% 
  filter(brp==1) %>% 
  summarise(number_books = n())
books_switzerland <- books %>% 
  distinct(id, .keep_all = TRUE) %>% #.keep_all means "keep variables other than id"
  filter(brp==0) %>%
  summarise(number_books = n())
#brp=291; swiss=486;

#b The total number of citations to BRP and Swiss books.
books_citations_brp <- books %>% 
  #distinct(id, .keep_all = TRUE) %>% 
  filter(brp==1) %>% 
  summarise(number_books = sum(cit_year))
books_citations_swiss <- books %>% 
  #distinct(id, .keep_all = TRUE) %>% 
  filter(brp==0) %>% 
  summarise(number_books = sum(cit_year))
#The total number of citations to BRP and Swiss books
# i. Before 1942
books_citations_brp_before <- books %>% 
  #distinct(id, .keep_all = TRUE) %>% 
  filter(year_c<1942) %>% 
  group_by(brp) %>% 
  summarise(number_books = sum(cit_year))
# i. After 1942
books_citations_brp_after <- books %>% 
  #distinct(id, .keep_all = TRUE) %>% 
  filter(year_c>=1942) %>% 
  group_by(brp) %>% 
  summarise(number_books = sum(cit_year))
#ii. By English-language authors and by authors publishing in other languages 
books_citations_english<- books %>% 
  #distinct(id, .keep_all = TRUE) %>% 
  group_by(brp) %>% 
  summarise(cit_eng= sum(count_eng),
            cit_noneng = sum(count_noeng))
#2.Begin to examine this question by plotting, by year, the number of citations in English to BRP
#books and the number of citations to BRP books by authors publishing in other languages
#(replication of Figure 1 in the paper)
plot_1_data <- books %>%
  filter(brp == 1)%>%
  group_by(year_c) %>%
  summarise(Eng = sum(count_eng)/n(),
            Other = sum(count_noeng)/n()) %>% 
  filter(year_c >=1930)

ggplot(plot_1_data) +
  geom_line(aes(x=year_c, y=Eng, colour = "darkorange2"), size = .8) +
  #geom_point(aes(x=year_c, y=Eng, colour = "darkorange2"), size = 1.2) +
  geom_line(aes(x=year_c, y=Other, colour = "royalblue"), size = .8) +
  #geom_point(aes(x=year_c, y=Other, colour = "royalblue"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.8) +
  geom_text(aes(x=1937, label="Introduction of BRP", y=0.8), 
            colour="black", 
            angle=0) +
  scale_color_identity(name = "", 
                       breaks = c("darkorange2", "royalblue"), 
                       labels = c("English", 
                                  "Other"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year") +
  ylab("Citations per book and year") +
  ggtitle("FIGURE 1 –Citations to BRP Books - 
           FROM NEW WORK IN ENGLISH VERSUS OTHER LANGUAGES") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.text = element_text(size = 10),legend.background = element_rect(fill="white",
                                                                                                         size=0.5, linetype="solid", 
                                                                                                           colour ="black"))
  
#b
reg_data <- books %>%
  group_by(id,year_c) %>% 
  mutate(id_new = cur_group_id()) %>% 
  select(-c(count_gerlang, count_other, english)) %>% 
  pivot_longer(cols= starts_with("count"), names_to = "english") %>% 
  rename(count = value) %>% 
  mutate(english=replace(english, english=="count_eng", 1)) %>%
  mutate(english=replace(english, english=="count_noeng",0)) %>%
  mutate(english=as.numeric(english), post=as.numeric(post))%>%
  mutate(english_post = english*post)
reg1_data <- reg_data %>%
  filter(brp==1)
reg1 <- felm(count  ~  english + english_post | id  + year_c| 0 , data=reg1_data)
reg1_cluster <- felm(count  ~  english + english_post | id  + year_c| 0 | id, data=reg1_data)
tidy(reg1, conf.int = TRUE, se.type = "robust")
tidy(reg1_cluster, conf.int = TRUE, se.type = "cluster")
huxreg(reg1, reg1_cluster,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       # coefs = c("Treated X post-1800" = "post_treat", # renaming variables
       #           "Post-1800" = "post",
       #           "Treated" = "treated"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("State FE", "Yes", "Yes"), #note! you need more "yes" if you have >3 models
                 c("Year FE", "Yes", "Yes"),
                 c("Pre-1942 mean", "0.269", "0.269")),
           # copy_cell_props = FALSE,
           after = c(nrow(.) - 3))%>% 
quick_docx("report_reg1.docx")

#b extra credit
reg_data_extra <-reg1_data%>%
filter(year_c <=1942)
mean(reg_data_extra$count)
#3(a)
plot_2_data_a <- books %>%
  group_by(year_c, id) %>%
  filter(year_c > 1929, brp ==1) %>%
  select(year_c, count_eng) %>%
  group_by(year_c) %>%
  mutate(total = sum(count_eng), count = n())%>%
  summarize(citations_BRP = total/count) %>%
  unique()


plot_2_data_b <- books %>%
  group_by(year_c, id) %>%
  filter(year_c > 1929, brp==0) %>%
  select(year_c, count_eng) %>%
  group_by(year_c) %>%
  mutate(total = sum(count_eng), count = n()) %>%
  summarize(citations_SWISS = total/count) %>%
  unique()

plot_2 <- merge(plot_2_data_a, plot_2_data_b, by = c("year_c"))

figure_2 <- ggplot(plot_2) +
  geom_line(aes(x=year_c, y=citations_BRP, colour = "darkorange2"), size = .8) +
  #geom_point(aes(x=year_c, y=Eng, colour = "darkorange2"), size = 1.2) +
  geom_line(aes(x=year_c, y=citations_SWISS, colour = "royalblue"), size = .8, 
            linetype = "dashed") +
  #geom_point(aes(x=year_c, y=Other, colour = "royalblue"), size = 1.2) +
  geom_vline(xintercept = 1942, linetype = "dashed", size = 0.5) +
  geom_text(aes(x=1940, label="Introduction of BRP", y=1), 
            colour="black", 
            angle=0) +
  scale_color_identity(name = "", 
                       breaks = c("darkorange2", "royalblue"), 
                       labels = c("BRP", 
                                  "Swiss"), 
                       guide = "legend") +
  theme_bw() +
  xlab("Year of citations") +
  ylab("Citations per book and year") +
  ggtitle("CITATIONS TO A MATCHED SAMPLE OF BRP AND SWISS BOOKS") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.text = element_text(size = 8))
figure_2

#3(b)
#solution1:
reg2_data <- books %>%
  group_by(year_c,id)%>%
  #filter(english==1) %>%
  #in this line add filter for books that are either BRP or come from swiss cities
  mutate(brp_post = brp*post)
reg2 <- felm(count_eng  ~ brp_post | id  + year_c| 0 , data=reg2_data)
reg2_cluster <- felm(count_eng  ~  brp_post  | id  + year_c| 0 | id, data=reg2_data)
tidy(reg2, conf.int = TRUE, se.type = "robust")
tidy(reg2_cluster, conf.int = TRUE, se.type = "cluster")
summary(reg2)
#solution2:
reg3_data <- reg_data %>%
  filter(english==1) %>%
  #in this line add filter for books that are either BRP or come from swiss cities
  mutate(brp_post = brp*post)
reg3 <- felm(count  ~ brp_post | id  + year_c| 0 , data=reg3_data)
reg3_cluster <- felm(count ~  brp_post  | id  + year_c| 0 | id, data=reg3_data)
tidy(reg3, conf.int = TRUE, se.type = "robust")
tidy(reg3_cluster, conf.int = TRUE, se.type = "cluster")
summary(reg3)
huxreg(reg3, reg3_cluster,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       # coefs = c("Treated X post-1800" = "post_treat", # renaming variables
       #           "Post-1800" = "post",
       #           "Treated" = "treated"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("State FE", "Yes", "Yes"), #note! you need more "yes" if you have >3 models
                 c("Year FE", "Yes", "Yes")),
           # copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>%
  quick_docx("report_reg2.docx")

#3(c)
#4(a)

#4(b)
prop_score_data <- books %>%
  filter(brp==0 |
           brp==1) %>%
  group_by(id, year_c)  %>%
  summarise(count_eng = sum(count_eng),
            count_noeng = sum(count_noeng),
            field=first(field),
            brp=first(brp),
            pre=1-post)  %>%
  group_by(id, pre) %>%
  mutate(count_noeng_pre = sum(count_noeng)) 


prop_score_data_collapsed <- prop_score_data %>%
  group_by(id)  %>%
  arrange(-pre)  %>%
  summarise(count_noeng_pre = first(count_noeng_pre),
            field=first(field),
            brp=first(brp))

prop_score_model <- glm(brp ~ field + count_noeng_pre,
                        family = binomial(), data = prop_score_data_collapsed)
summary(prop_score_model)

prop_score_prediction <- data.frame(pr_score = predict(prop_score_model, type = "response"),
                                    brp = prop_score_model$model$brp,
                                    id =prop_score_data_collapsed$id)

prop_score_data_collapsed <-prop_score_data_collapsed[!(prop_score_data_collapsed$field==""),]
prop_score_data_collapsed  <- prop_score_data_collapsed  %>% mutate(field = factor(field))

mod_match <- matchit(brp ~ field + count_noeng_pre,
                     distance = "mahalanobis", 
                     exact = "field",
                     data = prop_score_data_collapsed,
                     replace = TRUE)
data_matched <-get_matches(mod_match,
                           id = "id2")

#the variable subclass here identifies the matched pairs

data_matched <- data_matched   %>%
  arrange(subclass)

#Now what I do is add a dummy variable called "match" to the initial dataset
#equal to 1 for the id codes that the algorithm has matched

matched_ids <- data_matched$id

books_mat  <- books %>%
  mutate(match = ifelse(id %in% matched_ids, 1,0 ))

test <-books_mat %>% 
  filter(match == 1) %>% 
  filter(year_c > 1929) %>% 
  group_by(brp) %>% 
  summarise(brp = n_distinct(id))
View(test)
#c
plot_3_data <- books_mat %>%
  group_by(year_c,id)%>%
  filter(match == 1 , year_c>1929) %>%
  select(id, year_c, count_eng, brp) %>%
  group_by(year_c, brp) %>%
  mutate(total = sum(count_eng), count = n_distinct(id)) %>%
  summarise(citations_BRP = total/count) %>%
  unique()
figure_3 <- ggplot(plot_3_data) +
  geom_line(aes(x=year_c, y=citations_BRP, color=factor(brp)), size = .8)+
    #geom_point(aes(x=year_c, y=Eng, colour = "darkorange2"), size = 1.2) +
    #geom_point(aes(x=year_c, y=Other, colour = "royalblue"), size = 1.2) +
 # geom_line(aes(x=year_c, y=citations_BRP[0], color=factor(brp)), size = .8)+
    geom_vline(xintercept = 1942, linetype = "dashed", size = 0.5) +
    geom_text(aes(x=1940, label="Introduction of BRP", y=1), 
              colour="black", 
              angle=0) +
  #  scale_color_identity(name = "", 
     #                    breaks = c("darkorange2", "royalblue"), 
      #                   labels = c("BRP", 
       #                             "Swiss"), 
        #                 guide = "legend") +
    theme_bw() +
    xlab("Year of citations") +
    ylab("Citations per book and year") +
    ggtitle("CITATIONS TO A MATCHED SAMPLE OF BRP AND SWISS BOOKS") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
  figure_3

#regression
reg_data_matched <- books_mat %>%
  group_by(id,year_c) %>% 
  mutate(id_new = cur_group_id()) %>% 
  select(-c(count_gerlang, count_other, english)) %>% 
  pivot_longer(cols= starts_with("count"), names_to = "english") %>% 
  rename(count = value) %>% 
  mutate(english=replace(english, english=="count_eng", 1)) %>%
  mutate(english=replace(english, english=="count_noeng",0)) %>%
  mutate(english=as.numeric(english), post=as.numeric(post))%>%
  mutate(english_post = english*post) %>%
  filter(match==1)

reg4_data <- reg_data_matched %>%
  filter(english==1) %>%
  #in this line add filter for books that are either BRP or come from swiss cities
  mutate(brp_post = brp*post)

reg4 <- felm(count  ~ brp_post | id  + year_c| 0 , data=reg4_data)
reg4_cluster <- felm(count ~  brp_post  | id  + year_c| 0 | id, data=reg4_data)
tidy(reg4, conf.int = TRUE, se.type = "robust")
tidy(reg4_cluster, conf.int = TRUE, se.type = "cluster")
summary(reg4)
huxreg(reg4, reg4_cluster,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       # coefs = c("Treated X post-1800" = "post_treat", # renaming variables
       #           "Post-1800" = "post",
       #           "Treated" = "treated"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("State FE", "Yes", "Yes"), #note! you need more "yes" if you have >3 models
                 c("Year FE", "Yes", "Yes")),
           # copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>%
  quick_docx("report_reg4.docx")
#question_5
reg5_data <- books %>%
  group_by(id,year_c) %>% 
  mutate(id_new = cur_group_id()) %>% 
  select(-c(count_gerlang, count_other, english)) %>% 
  pivot_longer(cols= starts_with("count"), names_to = "english") %>% 
  rename(count = value) %>% 
  mutate(english=replace(english, english=="count_eng", 1)) %>%
  mutate(english=replace(english, english=="count_noeng",0)) %>%
  mutate(english=as.numeric(english), post=as.numeric(post))%>%
  mutate(english_post = english*post)%>%
  mutate(english_brp_post = english*brp*post)%>%
  mutate(brp_post = brp*post)%>%
  mutate(english_brp=english*brp)
reg5 <- felm(count  ~  english + english_brp + english_post + brp_post + english_brp_post | id  + year_c| 0 , data=reg5_data)
reg5_cluster <- felm(count  ~  english + english_brp + english_post + brp_post + english_brp_post | id  + year_c| 0 | id, data=reg5_data)
summary(reg5)
summary(reg5_cluster)
huxreg(reg5, reg5_cluster,
       stars = c(`*` = 0.1, `**` = 0.05, `***` = 0.01),
       # coefs = c("Treated X post-1800" = "post_treat", # renaming variables
       #           "Post-1800" = "post",
       #           "Treated" = "treated"),
       statistics = c("N" = "nobs", 
                      "R^2" = "r.squared")) %>%
  add_rows(rbind(c("State FE", "Yes", "Yes"), #note! you need more "yes" if you have >3 models
                 c("Year FE", "Yes", "Yes")),
           # copy_cell_props = FALSE,
           after = c(nrow(.) - 3)) %>%
  quick_docx("report_reg5.docx")

reg4_data <- books %>%
  group_by(id,year_c) %>% 
  select(-c(count_gerlang, count_other, english)) %>% 
  pivot_longer(cols= starts_with("count"), names_to = "english") %>% 
  rename(count = value) %>% 
  mutate(english=replace(english, english=="count_eng", 1)) %>%
  mutate(english=replace(english, english=="count_noeng",0)) %>%
  mutate(english=as.numeric(english), post=as.numeric(post))%>%
  mutate(english_post = english*post) %>% 
  mutate(english_brp = english*brp) %>% 
  mutate(brp_post = brp*post) %>% 
  mutate(english_brp_post = english*brp*post)

reg4 <- felm(count  ~  english + english_brp + english_post + brp_post + english_brp_post | id  + year_c| 0 , data=reg4_data)
reg4_cluster <- felm(count  ~  english + english_brp + english_post + brp_post + english_brp_post | id  + year_c| 0 | id, data=reg4_data)
summary(reg4)
summary(reg4_cluster)

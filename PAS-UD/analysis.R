library(tidyverse)
library(lme4)

###########
d = read_csv("PAS-UD/args_output.csv") %>%
  filter(pos == "VERB", deprel == "root",
         grepl("_", subj_type) == F, # exclude when multiple subj or obj
         grepl("_", obj_type) == F,
         has_comp == F #exclude comp or not
         ) %>% 
  group_by(lang) %>%
  mutate(langn = n()) %>%
  filter(langn > 1000, grepl("PUD", lang) == F) %>%
  mutate(subj_type_cat = case_when(grepl("modified", subj_type) ~ 4,
                                   grepl("det", subj_type) ~ 3,
                                   grepl("NOUN", subj_type) ~ 3,
                                   grepl("PROPN", subj_type) ~ 2,
                                   grepl("PRON", subj_type) ~ 1,
                                   is.na(subj_type) ~ 0,
                                   TRUE ~ 3),
         obj_type_cat = case_when(grepl("modified", obj_type) ~  4,
                                   grepl("det", obj_type) ~ 3,
                                   grepl("NOUN", obj_type) ~ 3, 
                                   grepl("PROPN", obj_type) ~ 2, 
                                   grepl("PRON", obj_type) ~ 1,
                                   is.na(obj_type) ~ 0,
                                   TRUE ~ 3))

d = separate(d, lang, into=c("lang", "corpus"), sep="-") %>%
  mutate(lang = gsub("UD_", "", lang))

ggplot(d, aes(x=subj_type_cat)) + geom_histogram()
ggplot(d, aes(x=obj_type_cat)) + geom_histogram()


with(d[d$has_obj == T, ], cor(subj_type_cat, obj_type_cat, method='spearman'))


d.sum = group_by(ungroup(d), has_obj, subj_type_cat, lang) %>%
  summarise(n=n()) %>%
  group_by(has_obj, lang) %>%
  mutate(pct=n/sum(n)) %>%
  select(-n) %>%
  ungroup() %>%
  mutate(sa = ifelse(has_obj == F, "S", "A"))  %>%
  select(lang, sa, pct, subj_type_cat)

d.sum = spread(d.sum, sa, pct) %>%
  mutate(diff = A - S)

o = filter(d, has_obj == T) %>%
  group_by(obj_type_cat, lang) %>%
  summarise(n=n()) %>%
  group_by(lang) %>%
  mutate(pct=n/sum(n)) %>%
  select(lang, obj_type_cat, pct) %>%
  rename(O = pct, subj_type_cat = obj_type_cat)

d.o = left_join(d.sum, o) %>%
  rename(nom = subj_type_cat)

d.o$diff.A.O = d.o$A - d.o$O


d.o$diff > 0

# abstract plot
ggplot(d.o, aes(x=as.factor(nom), y=diff)) + geom_jitter(width=.1) + 
  geom_smooth() + 
  ylab("<- More Freq. in S      More Freq. in A ->") + 
  xlab("argument type") + theme_bw(13) +
  scale_x_discrete(labels = c("empty", "pronoun", "proper", "not modified", "modified")) +
  geom_hline(yintercept=0, colour='red', alpha=.6)
ggsave("abstract_plot.png", height=4, width=7)
  

ggplot(d.o, aes(x=nom, y=A)) + geom_point()


# S vs A on a scale
l = lmer(data=d, subj_type_cat ~ has_obj + (has_obj | lang))
summary(l)

# S vs A categorical
d$Nonlex.subj = d$subj_type_cat <= 1
l.s.a.cat = glmer(data=d, family="binomial",
                  Nonlex.subj~ has_obj + (has_obj|lang))
summary(l.s.a.cat)







d.o.g = gather(d.o, variable, value, -nom, -diff, -lang) %>%
  filter(variable %in% c("A", "O", "S"))
ggplot(filter(d.o.g, nom != 0 | variable != "O") 
              , aes(x=as.factor(nom), y=value, fill=variable)) +
  geom_boxplot()

d.nonlex.subj = mutate(d, nonlex.subj = subj_type_cat <= 1) %>%
  group_by(lang, has_obj) %>%
  summarise(m=mean(nonlex.subj, na.rm=T)) %>%
  spread(has_obj, m) %>%
  mutate(diff=`TRUE` - `FALSE` > 0) %>%
  rename(A.nonlex=`TRUE`, S.nonlex=`FALSE`)
mean(d.nonlex.subj$diff)

d.nonlex.obj = mutate(filter(d, has_obj == T),
                      nonlex.obj = obj_type_cat <= 1) %>%
  group_by(lang) %>%
  summarise(m=mean(nonlex.obj, na.rm=T)) %>%
  rename(O.nonlex = m)

# assess whehter, when O is lexical, A is less likely to be
# m is probability of nonlexical subject
d.nonlex.obj2 = mutate(filter(d, has_obj == T),
                      nonlex.obj = obj_type_cat <= 1,
                      nonlex.subj = subj_type_cat <= 1) %>%
  group_by(lang, nonlex.obj) %>%
  summarise(m=mean(nonlex.subj, na.rm=T)) %>%
  spread(nonlex.obj, m) %>%
  rename(lex.obj = `FALSE`, nonlex.obj = `TRUE`)
mean(d.nonlex.obj2$lex.obj > d.nonlex.obj2$nonlex.obj, na.rm=T)

a = left_join(d.nonlex.obj, d.nonlex.subj)

mean(a$A.nonlex > a$S.nonlex)
mean(a$S.nonlex > a$O.nonlex)
mean(a$A.nonlex > a$O.nonlex)
mean(a$A.nonlex > a$S.nonlex & a$S.nonlex > a$O.nonlex)
a[a$A.nonlex < a$S.nonlex, ]
a[a$S.nonlex < a$O.nonlex, ]

spread(d.o.g, variable, value) %>%
  mutate(A.O.diff = A - O, 
         A.S.diff = A - S,
         S.O.diff = S - O) %>%
  group_by(nom) %>%
  summarise(mean(A.O.diff > 0, na.rm=T),
            mean(A.S.diff > 0, na.rm=T),
            mean(S.O.diff > 0, na.rm=T))
  

d.lang.sum = group_by(d.o.g, variable) %>% 
  summarise(m=mean(value),
            se=sd(value)/sqrt(n()),
            upper = m + 1.96*se,
            lower = m- 1.96*se)
ggplot(d.lang.sum, aes(x=variable, y=m))  +
  geom_point() + geom_errorbar(aes(x=variable, ymin=lower, ymax=upper, width=.4)) +
  geom_point(data=d.o.g, aes(x=variable, y=value), alpha=.1) + theme_bw(25) + 
  theme(axis.title.y = element_text(angle=0)) + 
  xlab("arg type") + ylab("prob. arg is lexical")  + ggtitle("Argument counts in Universal Deps")








##### reproduce binary
d.sum =   mutate(d, subj_type_cat = ifelse(subj_type_cat <= 1, 0, 1)) %>%
  group_by(has_obj, subj_type_cat, lang) %>%
  summarise(n=n()) %>%
  group_by(has_obj, lang) %>%
  mutate(pct.lex=n/sum(n)) %>%
  select(-n) %>%
  ungroup() %>%
  mutate(sa = ifelse(has_obj == F, "S", "A"),
         lex.subj = ifelse(subj_type_cat == 1, "lex", "nonlex"))  %>%
  select(lang, sa, pct.lex, lex.subj) %>%
  spread(sa, pct.lex) %>%
  mutate(diff = A - S) %>%
  filter(lex.subj == "lex")

o = filter(d, has_obj == T) %>%
  mutate(obj_type_cat = ifelse(obj_type_cat <= 1, 0, 1)) %>%
  group_by(obj_type_cat, lang) %>%
  summarise(n=n()) %>%
  group_by(lang) %>%
  mutate(pct=n/sum(n)) %>%
  select(lang, obj_type_cat, pct) %>%
  rename(O = pct) %>%
  filter(obj_type_cat == 1)

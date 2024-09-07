library(tidyverse)
library(readr)
library(lmerTest)
library(report)
fn <- list.files()
all_data <-
  data.frame(
    conditions = NA,
    var = NA,
    value = NA,
    speed = NA,
    ID = NA
  )
for (k in fn) {
  df <-
    read_delim(
      k,
      delim = "\t",
      escape_double = FALSE,
      col_names = FALSE,
      trim_ws = TRUE
    )[, -1]
  tofind <- paste(c("C1", "C2", "C3", "C4", "C5"), collapse = "|")
  conditions <- str_extract_all(df[1, ], tofind)
  conditions <- unlist(conditions)
  
  gast_act <-
    data.frame(conditions = conditions,
               var =  unlist(df[2, ]),
               value = unlist(df[6, ]))
  gast_act$speed <- NA
  gast_act[which(gast_act$var == 'Speed'), 4] <-
    gast_act[which(gast_act$var == 'Speed'), 3]
  for (i in 1:nrow(gast_act)) {
    if (is.na(gast_act[i, 4])) {
      gast_act[i, 4] <- gast_act[(i - 1), 4]
    }
  }
  
  RS <- which(gast_act$var == 'RS_precent_active')
  NA_act <- which(is.na(gast_act$value))
  
  rm_row <- RS[RS %in% NA_act] - 2
  gast_act_rm <- gast_act
  if (length(rm_row) != 0) {
    gast_act_rm <- gast_act[-c(rm_row, (rm_row + 1), (rm_row + 2)), ]
  }
  final <- gast_act_rm
  neg_per <- which(gast_act_rm$value < 0)
  for (i in neg_per) {
    insert_row <- gast_act_rm[i, ]
    if (insert_row$var == 'RG_precent_active') {
      insert_row[, 2] <- 	'R_gastrocnemius_medialis_int'
    } else {
      insert_row[, 2] <- 'R_soleus_int'
    }
    insert_row[, 3] <- 0
    final <- rbind(final, insert_row)
  }
  final$value[final$value < 0] <- 0
  final$ID <- k
  all_data <- rbind(all_data, final)
}




gast_per <- all_data %>% filter(var == 'R_soleus_int')
gast_per$speed <- as.numeric(gast_per$speed)
gast_per$value <- as.numeric(gast_per$value)
gast_per$speed2 <- gast_per$speed ^ 2
ggplot(gast_per, aes(x = speed, y = as.numeric(value))) + geom_smooth(method = 'gam', size = 1.5) +
  theme_bw() + xlim(c(0.1, 2)) + ylim(c(0, 0.00004)) + 
  ylab('Integrated EMG of gastrocnemius') + geom_line(aes(group = ID),
                                                        stat = "smooth",
                                                        size = 0.5, alpha = 0.1)
#model1 = lm(value ~ speed, data=gast_per) 
#model2 = lm(value ~ poly(speed,2, raw=T), data=gast_per)    
#model3 = lm(value ~ poly(speed,3, raw=T), data=gast_per) 
#report(model1)
#AIC(model1)
#report(model2)
#AIC(model2)
#anova(model1, model2)


library(lme4)

model1 <- lmer(value ~ poly(speed, 2, raw = T)  + (1 | ID),
               data = gast_per)
#model3 <- lmer(value ~ poly(speed, 2, raw = T)  + (speed | ID),
               #data = gast_per)
model4 <- lmer(value ~ poly(speed, 2, raw = T)  + (poly(speed, 2) |
                                                     ID),
               data = gast_per)
#model5 <- lmer(value ~ poly(speed, 2, raw = T), data = gast_per)

summary(model1)
summary(model4)
report(model1)
report(model4)
anova(model1, model4)
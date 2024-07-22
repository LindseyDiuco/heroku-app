age.pyramid <- function(tbl){
  
  pyramid.data <- data.frame(age=tbl$age_estimateyears, sex=tbl$sex, outcome=tbl$outcome)
  pyramid.data <- subset(pyramid.data, sex == "1" | sex == "2")
  pyramid.data <- pyramid.data[!is.na(pyramid.data$outcome), ]
  # pyramid.data$outcome[is.na(pyramid.data$outcome)] = "LUC"
  pyramid.data$`age group` <- cut(pyramid.data$age, c(seq(0,100, by=5), Inf), include.lowest=T)
  pyramid.data$sex <- as.factor(pyramid.data$sex)
  
  levels(pyramid.data$sex) <- c('Male', 'Female')
  
  ap <- age_pyramid(pyramid.data, `age group`, split_by=sex, stack_by = outcome)
  ap
}

outcomes.by.admission.date <- function(tbl){
  outcomes.data <- data.frame(date=tbl$dsstdat, outcome=tbl$outcome)
  outcomes.data$outcome[is.na(outcomes.data$outcome)] <- "LUC"
  outcomes.data <- outcomes.data[!is.na(outcomes.data$date),] #added
  outcomes.data <- outcomes.data[order(outcomes.data$date),]

  week <- as.numeric(outcomes.data$date - outcomes.data$date[1]) %/% 7
  outcomes.data$week <- week

  unique.week <- unique(outcomes.data$week)
  unique.outcome <- unique(outcomes.data$outcome)
  
  stack.data <- data.frame(
    week = rep(unique.week, each = length(unique.outcome)),
    outcome = rep(unique.outcome, times=length(unique.week))
  )
  
  count <- c()
  
  for(i in unique.week){
    for (j in unique.outcome){
      count <- c(count, sum(outcomes.data$week == i & outcomes.data$outcome == j))
    }
  }
  stack.data$count <- count
  
  # Check for NA or zero counts (added)
  stack.data <- stack.data[!is.na(stack.data$count) & stack.data$count > 0,]

  plt <- ggplot(stack.data, aes(fill=outcome, x=week, y=count)) + geom_bar(position='stack', stat='identity')
  plt + xlab("Week") + ylab("Count")
}

#updated
violin.age.func <- function(tbl) {
  subj_id <- tbl$subjid
  dsst_dat <- as.Date(tbl$dsstdat, format="%m/%d/%Y") 
  dsst_dtc <- as.Date(tbl$dsstdtc, format="%m/%d/%Y")
  age <- tbl$age_estimateyears
  
  entry <- data.frame(subj_id, age, dsst_dat)
  exit <- data.frame(subj_id, dsst_dtc)
  
  entry <- entry[!is.na(entry$dsst_dat),]
  entry <- entry[!is.na(entry$age),]
  
  exit <- exit[!is.na(exit$dsst_dtc),]
  
  join_df <- merge(entry, exit, by="subj_id")
  
  entry <- join_df$dsst_dat
  exit <- join_df$dsst_dtc
  
  time_difference <- c()
  
  for (i in 1:nrow(join_df)) {
    difference <- difftime(exit[i], entry[i], units = "days")
    if (difference > 30) {
      difference <- 30
    }
    time_difference <- c(time_difference, difference)
  }
  
  age <- join_df$age
  
  `age group` <- cut(age, c(seq(0,100, by=20), Inf), include.lowest=T)
  
  violin.data <- data.frame(age, `age group`, time_difference)
  
  p <- ggplot(violin.data, aes(x=`age group`, y=time_difference, fill = `age group`)) + geom_violin(size=0.5) + 
    stat_summary(fun.y="mean", geom="point", shape=23, size=2) + geom_boxplot(width=0.1, fill="white") + theme_classic()
  
  p + xlab("Age Group") + ylab("Length of Stay")
}

#updated
violin.sex.func <- function(tbl) {
  tbl <- backup.data
  subj_id <- tbl$subjid
  dsst_dat <- as.Date(tbl$dsstdat, format="%m/%d/%Y") 
  dsst_dtc <- as.Date(tbl$dsstdtc, format="%m/%d/%Y")
  sex <- tbl$sex
  
  entry <- data.frame(subj_id, sex, dsst_dat)
  exit <- data.frame(subj_id, dsst_dtc)
  
  entry <- entry[!is.na(entry$dsst_dat),]
  entry <- entry[!is.na(entry$sex),]
  entry <- entry[entry$sex <= 2, ]
  
  exit <- exit[!is.na(exit$dsst_dtc),]
  
  join_df <- merge(entry, exit, by="subj_id")
  
  entry <- join_df$dsst_dat
  exit <- join_df$dsst_dtc
  
  time_difference <- c()
  
  for (i in 1:nrow(join_df)) {
    difference <- difftime(exit[i], entry[i], units = "days")
    if (difference > 30) {
      difference <- 30
    }
    time_difference <- c(time_difference, difference)
  }
  
  sex <- join_df$sex
  sex <- as.factor(sex)
  
  violin.data <- data.frame(sex, time_difference)
  
  p <- ggplot(violin.data, aes(x = sex, y = time_difference, fill = sex)) + 
    geom_violin(size = 0.5) + 
    stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 2) + 
    geom_boxplot(width = 0.1, fill = "white") + 
    theme_classic()
  
  p + xlab("Sex") + ylab("Length of Stay")
}

#updated
comorbidity_counter <- function(tbl){
  count_vector <- c()
  name_vector <- c()
  
  comorbidity.data <- tbl
  
  choniccard_mhyn.data <- comorbidity.data$chroniccard_mhyn
  choniccard_mhyn.data[is.na(choniccard_mhyn.data)] <- 2
  
  choniccard_mhyn.count <- 0
  for (i in 1:length(comorbidity.data$chroniccard_mhyn)){
    cond <- choniccard_mhyn.data[i] == 1
    if (cond){
      choniccard_mhyn.count = choniccard_mhyn.count + 1
    }
  }
  count_vector <- c(count_vector, choniccard_mhyn.count)
  name_vector <- c(name_vector, "Chronic Cardiac Disease")
  
  hypertension_mhyn.data <- comorbidity.data$hypertension_mhyn
  hypertension_mhyn.data[is.na(hypertension_mhyn.data)] <- 2
  
  hypertension_mhyn.count <-0
  for (i in 1:length(hypertension_mhyn.data)) {
    cond <- hypertension_mhyn.data[i] == 1
    if (cond){
      hypertension_mhyn.count = hypertension_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, hypertension_mhyn.count)
  name_vector <- c(name_vector, "Hypertension")
  
  chronicpul_mhyn.data <- comorbidity.data$chronicpul_mhyn
  chronicpul_mhyn.data[is.na(chronicpul_mhyn.data)] <- 2
  
  chronicpul_mhyn.count <- 0
  for (i in 1:length(chronicpul_mhyn.data)) {
    cond <- chronicpul_mhyn.data[i] == 1
    if (cond) {
      chronicpul_mhyn.count = chronicpul_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, chronicpul_mhyn.count)
  name_vector <- c(name_vector, "Chronic Pulmonary Disease")
  
  asthma_mhyn.data <- comorbidity.data$asthma_mhyn
  asthma_mhyn.data[is.na(asthma_mhyn.data)] <- 2
  
  asthma_mhyn.count <- 0
  for (i in 1:length(asthma_mhyn.data)) {
    cond <- asthma_mhyn.data[i] == 1
    if (cond) {
      asthma_mhyn.count = asthma_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, asthma_mhyn.count)
  name_vector <- c(name_vector, "Asthma")
  
  renal_mhyn.data <- comorbidity.data$renal_mhyn
  renal_mhyn.data[is.na(renal_mhyn.data)] <- 2
  
  renal_mhyn.count <- 0
  for (i in 1:length(renal_mhyn.data)) {
    cond <- renal_mhyn.data[i] == 1
    if (cond) {
      renal_mhyn.count = renal_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, renal_mhyn.count)
  name_vector <- c(name_vector, "Chronic Kidney Disease")
  
  obesity_mhyn.data <- comorbidity.data$obesity_mhyn
  obesity_mhyn.data[is.na(obesity_mhyn.data)] <- 2
  
  obesity_mhyn.count <- 0
  for (i in 1:length(obesity_mhyn.data)) {
    cond <- obesity_mhyn.data[i] == 1
    if (cond) {
      obesity_mhyn.count = obesity_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, obesity_mhyn.count)
  name_vector <- c(name_vector, "Obesity")
  
  chronicneu_mhyn.data <- comorbidity.data$chronicneu_mhyn
  chronicneu_mhyn.data[is.na(chronicneu_mhyn.data)] <- 2
  
  chronicneu_mhyn.count <- 0
  for (i in 1:length(chronicneu_mhyn.data)) {
    cond <- chronicneu_mhyn.data[i] == 1
    if (cond) {
      chronicneu_mhyn.count = chronicneu_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, chronicneu_mhyn.count)
  name_vector <- c(name_vector, "Chronic Neurological Disorder")
  
  malignantneo_mhyn.data <- comorbidity.data$malignantneo_mhyn
  malignantneo_mhyn.data[is.na(malignantneo_mhyn.data)] <- 2
  
  malignantneo_mhyn.count <- 0
  for (i in 1:length(malignantneo_mhyn.data)) {
    cond <- malignantneo_mhyn.data[i] == 1
    if (cond) {
      malignantneo_mhyn.count = malignantneo_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, malignantneo_mhyn.count)
  name_vector <- c(name_vector, "Malignant Neoplasm")
  
  chronhaemo_mhyn.data <- comorbidity.data$chronhaemo_mhyn
  chronhaemo_mhyn.data[is.na(chronhaemo_mhyn.data)] <- 2
  
  chronhaemo_mhyn.count <- 0
  for (i in 1:length(chronhaemo_mhyn.data)) {
    cond <- chronhaemo_mhyn.data[i] == 1
    if (cond) {
      chronhaemo_mhyn.count = chronhaemo_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, chronhaemo_mhyn.count)
  name_vector <- c(name_vector, "Chronic Hermatologic Disease")
  
  aidshiv_mhyn.data <- comorbidity.data$aidshiv_mhyn
  aidshiv_mhyn.data[is.na(aidshiv_mhyn.data)] <- 2
  
  aidshiv_mhyn.count <- 0
  for (i in 1:length(aidshiv_mhyn.data)) {
    cond <- aidshiv_mhyn.data[i] == 1
    if (cond) {
      aidshiv_mhyn.count = aidshiv_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, aidshiv_mhyn.count)
  name_vector <- c(name_vector, "AIDS/HIV")
  
  dementia_mhyn.data <- comorbidity.data$dementia_mhyn
  dementia_mhyn.data[is.na(dementia_mhyn.data)] <- 2
  
  dementia_mhyn.count <- 0
  for (i in 1:length(dementia_mhyn.data)) {
    cond <- dementia_mhyn.data[i] == 1
    if (cond) {
      dementia_mhyn.count = dementia_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, dementia_mhyn.count)
  name_vector <- c(name_vector, "Dementia")
  
  #added malnutrition_mhyn
  malnutrition_mhyn.data <- comorbidity.data$malnutrition_mhyn
  malnutrition_mhyn.data[is.na(malnutrition_mhyn.data)] <- 2
  
  malnutrition_mhyn.count <- 0
  for (i in 1:length(malnutrition_mhyn.data)) {
    cond <- malnutrition_mhyn.data[i] == 1
    if (cond) {
      malnutrition_mhyn.count = malnutrition_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, malnutrition_mhyn.count)
  name_vector <- c(name_vector, "Malnutrition")
  
  smoking_mhyn.data <- comorbidity.data$smoking_mhyn
  smoking_mhyn.data[is.na(smoking_mhyn.data)] <- 2
  
  smoking_mhyn.count <- 0
  for (i in 1:length(smoking_mhyn.data)) {
    cond <- smoking_mhyn.data[i] == 1
    if (cond) {
      smoking_mhyn.count = smoking_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, smoking_mhyn.count)
  name_vector <- c(name_vector, "Smoking")
  
  other_mhyn.data <- comorbidity.data$other_mhyn
  other_mhyn.data[is.na(other_mhyn.data)] <- 2
  
  other_mhyn.count <- 0
  for (i in 1:length(other_mhyn.data)) {
    cond <- other_mhyn.data[i] == 1
    if (cond) {
      other_mhyn.count = other_mhyn.count + 1
    }
  }
  
  count_vector <- c(count_vector, other_mhyn.count)
  name_vector <- c(name_vector, "Other")
  
  pregnancy.data <- comorbidity.data$pregnancy 
  pregnancy.data[is.na(pregnancy.data)] <- 2
  
  pregnancy.count <- 0
  for (i in 1:length(pregnancy.data)) {
    cond <- pregnancy.data[i] == 1
    if (cond) {
      pregnancy.count = pregnancy.count + 1
    }
  }
  
  count_vector <- c(count_vector, pregnancy.count)
  name_vector <- c(name_vector, "Pregnancy")
  
  liver.disease.data <- comorbidity.data$liver.disease
  liver.disease.data[is.na(liver.disease.data)] <- 2
  
  liver.disease.count <- 0
  for (i in 1:length(liver.disease.data)) {
    cond <- liver.disease.data[i] == 1
    if (cond) {
      liver.disease.count = liver.disease.count + 1
    }
  }
  
  count_vector <- c(count_vector, liver.disease.count)
  name_vector <- c(name_vector, "Liver Disease")
  
  diabetes.data <- comorbidity.data$diabetes
  diabetes.data[is.na(diabetes.data)] <- 2
  
  diabetes.count <- 0
  for (i in 1:length(diabetes.data)) {
    cond <- diabetes.data[i] == 1
    if (cond) {
      diabetes.count = diabetes.count + 1
    }
  }
  
  count_vector <- c(count_vector, diabetes.count)
  name_vector <- c(name_vector, "Diabetes")
  
  counter.data <- data.frame(name_vector, count_vector)
  counter.data
}

comorbidity.prevalence.plot <- function(tbl){
  
  counter.data <- comorbidity_counter(tbl)
  
  ggp <- ggplot(counter.data, aes(x = reorder(name_vector, count_vector), y = count_vector)) +   
    geom_bar(stat = "identity", fill="steelblue")
  
  ggp <- ggp + coord_flip()
  ggp + ylab("Count") + xlab ("Comorbidities")
  
}

#updated
comorbidities.upset <- function(tbl){
  fields <- comorbidities$field
  commorbidity_count <- cbind(comorbidity_counter(tbl), fields)
  reordered_data <- commorbidity_count[order(commorbidity_count$count_vector, decreasing = T), ]
  name <- reordered_data$name_vector
  count <- reordered_data$count_vector
  field <-reordered_data$fields
  
  commorbidity_count <- tbl[, field[1:4]]
  commorbidity_count[is.na(commorbidity_count)] <- 2
  commorbidity_count[commorbidity_count != 1] <- 2

  commorbidity_count[commorbidity_count == 1] <- T
  commorbidity_count[commorbidity_count == 2] <- F

  commorbidity_count[] <- lapply(commorbidity_count[], as.integer)
  colnames(commorbidity_count) <- name[1:4]
  df <- as.data.frame(commorbidity_count)
  plot <- UpSetR::upset(data = df, sets = name[1:4], order.by = 'freq', line.size = 1, point.size = 3, text.scale = 1.5)
  plot
}
  
symptom.counter <- function(tbl) {
  count_vector <- c()
  name_vector <- c()
  
  symptom.data <- tbl
  
  for (i in 1:length(admission.symptoms$field)){
    field <- admission.symptoms$field[i]
    label <- admission.symptoms$label[i]
    
    temp.data <- symptom.data[field]
    temp.data[is.na(temp.data)] <- 2
    
    count_vector <- c(count_vector, sum(temp.data[field] != 2, na.rm=T))
    name_vector <- c(name_vector, label)
  }
  
  counter.data <- data.frame(count_vector, name_vector)
  counter.data
}

symptom.prevalence.plot <- function (tbl) {
  
  counter.data <- symptom.counter(tbl)
  ggp <- ggplot(counter.data, aes(x = reorder(name_vector, count_vector), y = count_vector)) +   
    geom_bar(stat = "identity", fill="purple")
  
  ggp <- ggp +  coord_flip()
  ggp + ylab("Count") + xlab("Symptoms")
}

symptoms.upset <- function(tbl) {
  fields <- admission.symptoms$field
  symptom_count <- cbind(symptom.counter(tbl), fields)
  
  reordered_data <- symptom_count[order(symptom_count$count_vector, decreasing = T), ]
  name <- reordered_data$name_vector
  count <- reordered_data$count_vector
  field <-reordered_data$fields
  
  df <- tbl[, field[1:4]]
  df[is.na(df)] <- 2
  df[df != 1] <- 2
  
  df[df == 1] <- T
  df[df== 2] <- F
  
  df[] <- lapply(df[], as.integer)
  colnames(df) <- name[1:4]
  df <- as.data.frame(df)
  plot <- UpSetR::upset(data = df, sets = name[1:4], order.by = 'freq', line.size = 1, point.size = 3, text.scale = 1.5)
  plot
}
treatment.counter <- function(tbl) {
  treatments$label[treatments$field == "oxygen_cmoccur"] <- "Oxygen Therapy"
  treatments$label[treatments$field == "noninvasive_proccur"] <- "Non-Invasive Ventillation"
  treatments$label[treatments$field == "invasive_proccur"] <- "Invasive Ventilation"
  treatments$label[treatments$field == "pronevent_prtrt"] <- "Prone Ventilation"
  treatments$label[treatments$field == "other_cmyn"] <- "Other Intervention"
  treatments$field[treatments$field == "other_cmyn"] <- "othertx_cmyn"
  count_vector <- c()
  name_vector <- c()
  
  treatment.data <- tbl
  
  for (i in 1:length(treatments$field)){
    field <- treatments$field[i]
    label <- treatments$label[i]
    
    temp.data <- treatment.data[field]
    temp.data[is.na(temp.data)] <- 2
    
    count_vector <- c(count_vector, sum(temp.data[field] != 2, na.rm=T))
    name_vector <- c(name_vector, label)
  }
  
  counter.data <- data.frame(count_vector, name_vector)
  counter.data
}

treatment.use.plot <- function(tbl) {
  counter.data <- treatment.counter(tbl)
  ggp <- ggplot(counter.data, aes(x = reorder(name_vector, count_vector), y = count_vector)) +   
    geom_bar(stat = "identity", fill="violet")
  
  ggp <- ggp +  coord_flip()
  ggp + ylab("Count") + xlab("Treatments")
}

treatment.upset <- function(tbl) {
  treatments$label[treatments$field == "oxygen_cmoccur"] <- "Oxygen Therapy"
  treatments$label[treatments$field == "noninvasive_proccur"] <- "Non-Invasive Ventillation"
  treatments$label[treatments$field == "invasive_proccur"] <- "Invasive Ventilation"
  treatments$label[treatments$field == "pronevent_prtrt"] <- "Prone Ventilation"
  treatments$label[treatments$field == "other_cmyn"] <- "Other Intervention"
  treatments$field[treatments$field == "other_cmyn"] <- "othertx_cmyn"
  fields <- treatments$field
  treatment_count <- cbind(treatment.counter(tbl), fields)
  
  reordered_data <- treatment_count[order(treatment_count$count_vector, decreasing = T), ]
  name <- reordered_data$name_vector
  count <- reordered_data$count_vector
  field <-reordered_data$fields
  
  df <- tbl[, field[1:4]]
  df[is.na(df)] <- 2
  df[df != 1] <- 2
  
  df[df == 1] <- T
  df[df== 2] <- F
  
  df[] <- lapply(df[], as.integer)
  colnames(df) <- name[1:4]
  df <- as.data.frame(df)
  plot <- UpSetR::upset(data = df, sets = name[1:4], order.by = 'freq', line.size = 1, point.size = 3, text.scale = 1.5)
  plot
}

onset.adm.plot <- function(tbl) {
  dsstdat <- tbl$dsstdat
  dsstdat <- dsstdat[!is.na(dsstdat)]
  cestdat <- tbl$cestdat
  cestdat <- cestdat[!is.na(cestdat)]
  
  time_difference = c()
  
  for (i in 1:nrow(tbl)){
    if(!is.na(dsstdat[i]) && !is.na(cestdat[i])){
      difference = abs(as.numeric(dsstdat[i] - cestdat[i]))
      cond = difference <= 31
      if(cond){
        time_difference = c(time_difference, difference)
      }
    }
  }
  
  df = data.frame(time_difference)
  
  ggplot(data = df, aes(x = time_difference)) + geom_density(color="steelblue", size=1) + xlab("Length of Stay") + geom_vline(xintercept=mean(df$time_difference), linetype="dashed")
}

adm.outcome.plot <- function(tbl) {
  
  library(ggplot2)
  library(dplyr)
  
  tbl <- backup.data
  
  subj_id <- tbl$subjid
  dsst_dat <- as.Date(tbl$dsstdat, format = "%m/%d/%Y")
  dsst_dtc <- as.Date(tbl$dsstdtc, format = "%m/%d/%Y")
  
  entry <- data.frame(subj_id, dsst_dat)
  exit <- data.frame(subj_id, dsst_dtc)
  
  entry <- entry[!is.na(entry$dsst_dat),]
  
  exit <- exit[!is.na(exit$dsst_dtc),]
  
  join_df <- merge(entry, exit, by="subj_id")
  
  entry <- join_df$entry
  exit <- join_df$exit
  
  for (i in 1:nrow(join_df)){
    temp <- entry[i]
    entry[i] <- paste(c(substr(temp,1,4),substr(temp,5,6), substr(temp,7,8)), collapse="-")
    
    temp <- exit[i]
    exit[i] <- paste(c(substr(temp,1,4),substr(temp,5,6), substr(temp,7,8)), collapse="-")
  }
  
  time_difference <- c()
  
  for (i in 1:nrow(join_df)) {
    difference <- abs(join_df$dsst_dtc[i] - join_df$dsst_dat[i])
    if (difference <= 31){
      time_difference <- c(time_difference, difference)
    }
  }
  
  df = data.frame(time_difference)
  
  ggplot(data = df, aes(x = time_difference)) + geom_density(color="steelblue", size=1) + xlab("Length of Stay") + geom_vline(xintercept=mean(df$time_difference), linetype="dashed")
  
}

admission.lstm <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(admission.forecast) + 1)
  reverse_forecast[1] <- daily.admission$Freq[length(daily.admission$Freq)]
  
  for (i in 1:length(admission.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + admission.forecast[i]
    #cat(reverse_forecast[i], " + ", admission.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.admission$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.admission$Date, count = daily.admission$Freq, label = "Actual", 
                    lo80 = daily.admission$Freq, hi80=daily.admission$Freq, 
                    lo95=daily.admission$Freq, hi95=daily.admission$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

positive.lstm <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(positive.forecast) + 1)
  reverse_forecast[1] <- daily.positive$Freq[length(daily.positive$Freq)]
  
  for (i in 1:length(positive.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + positive.forecast[i]
    #cat(reverse_forecast[i], " + ", positive.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.positive$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.positive$Date, count = daily.positive$Freq, label = "Actual", 
                    lo80 = daily.positive$Freq, hi80=daily.positive$Freq, 
                    lo95=daily.positive$Freq, hi95=daily.positive$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

active.lstm <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(active.forecast) + 1)
  reverse_forecast[1] <- daily.active$Freq[length(daily.active$Freq)]
  
  for (i in 1:length(active.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + active.forecast[i]
    #cat(reverse_forecast[i], " + ", active.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.active$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.active$Date, count = daily.active$Freq, label = "Actual", 
                    lo80 = daily.active$Freq, hi80=daily.active$Freq, 
                    lo95=daily.active$Freq, hi95=daily.active$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

death.rnn <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(death.forecast) + 1)
  reverse_forecast[1] <- daily.death$Freq[length(daily.death$Freq)]
  
  for (i in 1:length(death.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + death.forecast[i]
    #cat(reverse_forecast[i], " + ", death.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.death$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.death$Date, count = daily.death$Freq, label = "Actual", 
                    lo80 = daily.death$Freq, hi80=daily.death$Freq, 
                    lo95=daily.death$Freq, hi95=daily.death$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

bed.lstm <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(bed.forecast) + 1)
  reverse_forecast[1] <- daily.bed$Freq[length(daily.bed$Freq)]
  
  for (i in 1:length(bed.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + bed.forecast[i]
    #cat(reverse_forecast[i], " + ", bed.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.bed$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.bed$Date, count = daily.bed$Freq, label = "Actual", 
                    lo80 = daily.bed$Freq, hi80=daily.bed$Freq, 
                    lo95=daily.bed$Freq, hi95=daily.bed$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

icu.lstm <- function(tbl) {
  
  #reverse differencing
  reverse_forecast <- numeric(length = length(icu.forecast) + 1)
  reverse_forecast[1] <- daily.icu$Freq[length(daily.icu$Freq)]
  
  for (i in 1:length(icu.forecast)) {
    reverse_forecast[i + 1] <- reverse_forecast[i] + icu.forecast[i]
    #cat(reverse_forecast[i], " + ", icu.forecast[i], " = ", reverse_forecast[i + 1], "\n")
  }
  reverse_forecast <- reverse_forecast[-1]
  reverse_forecast
  
  future_dates <- seq(max(daily.icu$Date) + 1, length.out = 30, by = "day")
  
  df1 <- data.frame(date = daily.icu$Date, count = daily.icu$Freq, label = "Actual", 
                    lo80 = daily.icu$Freq, hi80=daily.icu$Freq, 
                    lo95=daily.icu$Freq, hi95=daily.icu$Freq)
  df2 <- data.frame(date = future_dates, count = reverse_forecast, label = "Forecast",
                    lo80 = reverse_forecast, hi80 = reverse_forecast, 
                    lo95 = reverse_forecast, hi95 = reverse_forecast)
  
  dff <- rbind(df1, df2)
  
  dff %>% 
    ggplot(aes(x=date, y=count, color=label)) + geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lo80, ymax=hi80), alpha=0.3) +
    geom_ribbon(aes(ymin=lo95, ymax=hi95), alpha = 0.1) +
    xlab("Time") + 
    ylab("Count") +
    scale_x_date(date_breaks="4 months", date_labels = "%Y-%m") +
    theme(legend.position="none")
}

#COVID-19 mortality
imp.death1 <- function(tbl) {
  glm_sv
  
  sv_importance(glm_sv$`1`, fill = "darkred") +
  theme_gray() + 
    labs(color = "Predictor Value", x = "SHAP Value (Impact on COVID-19 Mortality Risk)")
}

imp.death2 <- function(tbl) {
  glm_sv
  
  sv_importance(glm_sv$`1`, kind = "bee") + scale_color_gradient(low = "darkblue", high = "red", breaks = c(0, 1), labels = c("Low/No", "High/Yes")) + 
    theme_gray() + 
    labs(color = "Predictor Value", x = "SHAP Value (Impact on COVID-19 Mortality Risk)")
}

#COVID-19 length of hospital stay
imp.los1 <- function(tbl) {
  lm_sv
  
  sv_importance(lm_sv, fill = "darkred") +
    theme_gray() + 
    labs(color = "Predictor Value",  x = "SHAP Value (Impact on Length of Hospital Stay)")
}

imp.los2 <- function(tbl) {
  lm_sv
  
  sv_importance(lm_sv, kind = "bee") + scale_color_gradient(low = "darkblue", high = "red", breaks = c(0, 1), labels = c("Low/No", "High/Yes")) + 
    theme_gray() + 
    labs(color = "Predictor Value",  x = "SHAP Value (Impact on Length of Hospital Stay)")
}

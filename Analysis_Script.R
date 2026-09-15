library(tidyverse)    # Core data manipulation and visualization (ggplot2, dplyr, tidyr, readr, etc.)
library(dplyr)        # Loaded explicitly to ensure dplyr verbs take precedence over masking conflicts
library(lme4)         # Linear and generalized linear mixed-effects models: lmer(), glmer()
library(easystats)    # Model diagnostics and performance metrics: check_model(), model_performance()
library(nlme)         # Nonlinear and linear mixed-effects models: nlme(), groupedData()
library(nlraa)        # Extends nlme with predict_nlme() for confidence intervals on fitted curves
library(lmerTest)     # Adds Satterthwaite-approximated p-values and df to lmer() output
library(ggeffects)    # Computes marginal effects and model predictions for ggplot2: ggpredict()
library(flextable)    # Publication-ready tables: flextable(), autofit(), theme_vanilla()

df <- read.csv2("Dataset1.csv", sep=";")
df <- read.csv2("Dataset2.csv", sep=";")

# Functions -------------------------------------------------------------------
fill_probe_info <- function(data) {
  n <- nrow(data)
  
  # Initialize outputs
  filled_responses <- rep(NA, n)
  filled_probe_number <- rep(NA, n)
  enriched_probe <- as.character(data$Trial_Probe)  # copy original labels
  
  # Indices of probe trials
  probe_idx <- which(data$Trial_Probe == "True")
  
  if (length(probe_idx) == 0) {
    # No probes: return with NA fills and original labels
    data$Probe_Response <- filled_responses
    if (!("Probe_Number" %in% names(data))) {
      data$Probe_Number <- filled_probe_number
    } else {
      data$Probe_Number <- filled_probe_number
    }
    data$Trial_Probe <- enriched_probe
    return(data)
  }
  
  # Enrich Trial_Probe with pre-probe labels for 9 trials before each probe
  for (i in seq_along(probe_idx)) {
    current_probe <- probe_idx[i]
    
    # Mark probe explicitly
    enriched_probe[current_probe] <- "True"
    
    # Pre-probe windows (as in your original logic)
    pre5_idx <- (current_probe - 4):(current_probe - 1)
    pre10_idx <- (current_probe - 9):(current_probe - 5)
    
    pre5_idx <- pre5_idx[pre5_idx > 0]
    pre10_idx <- pre10_idx[pre10_idx > 0]
    
    if (length(pre5_idx) > 0) enriched_probe[pre5_idx] <- "Pre-probe_5"
    if (length(pre10_idx) > 0) enriched_probe[pre10_idx] <- "Pre-probe_10"
  }
  
  # Propagate Probe_Number and Probe_Response ONLY within the 9-trial pre-probe window + the probe trial
  for (i in seq_along(probe_idx)) {
    current_probe <- probe_idx[i]
    start_window <- max(1, current_probe - 9)
    end_window <- current_probe
    idx_window <- start_window:end_window
    
    filled_probe_number[idx_window] <- data$Probe_Number[current_probe]
    filled_responses[idx_window] <- data$Probe_Response[current_probe]
  }
  
  # Assign outputs
  data$Probe_Response <- filled_responses
  data$Probe_Number <- filled_probe_number
  data$Trial_Probe <- enriched_probe
  
  return(data)
}

# Function to calculate trial-wise learning index (10-trial rolling window)
calculate_trial_learning_index <- function(data) {
  data$Learning_Index_Continuous <- NA
  
  for (r in 1:nrow(data)) {
    pid <- data$Participant[r]
    current_trial <- data$Trial[r]
    
    in_window <- data$Participant == pid &
      data$Trial >= (current_trial - 9) &
      data$Trial <= current_trial
    
    window_data <- data[in_window, ]
    
    low_rts  <- window_data$RT[window_data$Bigram_Type == "Low_Probability"]
    high_rts <- window_data$RT[window_data$Bigram_Type == "High_Probability"]
    
    if (length(low_rts) > 0 && length(high_rts) > 0) {
      data$Learning_Index_Continuous[r] <- mean(low_rts, na.rm = TRUE) - mean(high_rts, na.rm = TRUE)  # Index = mean(Low_P RT) - mean(High_P RT); positive values indicate a learning effect
    }
  }
  
  return(data)
}

# Function to calculate block-level learning index (applied to all trials in block)
calculate_block_learning_index <- function(data) {
  result <- data %>%
    group_by(Participant, Block) %>%
    mutate(
      Mean_Low_Prob_RT = mean(RT[Bigram_Type == "Low_Probability"], na.rm = TRUE),
      Mean_High_Prob_RT = mean(RT[Bigram_Type == "High_Probability"], na.rm = TRUE),
      N_Low_Prob = sum(Bigram_Type == "Low_Probability", na.rm = TRUE),
      N_High_Prob = sum(Bigram_Type == "High_Probability", na.rm = TRUE),
      Learning_Index_Block = ifelse(
        N_Low_Prob == 0 | N_High_Prob == 0, 
        NA,                                   # Return NA if either condition is absent in the block
        Mean_Low_Prob_RT - Mean_High_Prob_RT  # Index = mean(Low_P RT) - mean(High_P RT); positive values indicate a learning effect
      )
    ) %>%
    ungroup() %>%
    select(-Mean_Low_Prob_RT, -Mean_High_Prob_RT, -N_Low_Prob, -N_High_Prob)  # Drop intermediate columns used for computation
  
  return(result)
}

# Function to select trials fitting a time window before the probe
flag_probe_time_window <- function(data) {
  data$Time_Window <- NA
  
  # Identify probe rows
  probe_rows <- which(data$Trial_Probe == "True")
  
  for (r in probe_rows) {
    pid <- data$Participant[r]
    # Probe starts when the response to the probe trial is registered
    probe_start_time <- data$Timestamps[r] + data$RT[r]
    
    # Flag trials for this participant within 5000ms before the probe start
    in_time_window <- data$Participant == pid & 
      data$Timestamps >= (probe_start_time - 5000) & 
      data$Timestamps <= probe_start_time
    
    data$Time_Window[in_time_window] <- "True"
  }
  
  return(data)
}

# Function to filter upper bound values
is_not_outlier_IQR <- function(x, iqr = 3) {
  if(all(is.na(x))) return(rep(TRUE, length(x)))
  x <= (quantile(x, 0.75, na.rm = T) + iqr * IQR(x, na.rm = T))  # Returns TRUE for values at or below Q3 + iqr*IQR (one-sided upper fence)
}

# Function to automatically report p value
format_p <- function(p) {
  if (p < .001) {
    return("< .001")
  } else if (p < .01) {
    return("< .01")
  } else if (p < .05) {
    return("< .05")
  } else {
    return(paste("=", round(p, 2)))  # Returns exact value rounded to 2 decimal places if p >= .05
  }
}

# Function to convert scientific notation to superscript format
convert_to_superscript <- function(values) {
  # Format numbers
  formatted <- ifelse(abs(values) < 0.01 & values != 0, 
                      sprintf("%.2e", values),   # Scientific notation for small values
                      sprintf("%.2f", values))   # Fixed notation otherwise
  
  # Convert scientific notation exponents to superscripts
  result <- formatted
  has_e <- grepl("e", formatted)
  
  if(any(has_e)) {
    # Process each scientific notation
    sci_parts <- strsplit(formatted[has_e], "e")
    bases <- sapply(sci_parts, "[", 1)
    exponents <- sapply(sci_parts, "[", 2)
    
    # Convert exponents to superscripts
    super_exponents <- sapply(exponents, function(exp) {
      exp_chars <- strsplit(exp, "")[[1]]
      paste(superscripts_map[exp_chars], collapse = "")
    })
    
    result[has_e] <- paste0(bases, super_exponents)
  }
  
  return(result)
}

# Pre-analysis ----------------------------------------------------------------
## Sample description (pre-cleaning) ---------------------------------------------------------
# Individual description
Descriptive_participants <- df %>%
  mutate(Probe_Response = as.numeric(Probe_Response),
         Participant = as.factor(Participant)) %>%
  group_by(Participant) %>%
  dplyr::summarise(
    N_total = n(),
    N_Women = mean(Gender %in% c("F", "f")),
    N_Men = mean(Gender %in% c("M", "m", "H")),
    Perc_Women = mean(Gender %in% c("F", "f")) * 100,
    Perc_Men = mean(Gender %in% c("M", "m", "H")) * 100,
    Age = mean(Age, na.rm = TRUE),
    Accuracy = mean(Response_Correctness == "Correct") * 100,
    RT = mean(as.numeric(RT), na.rm = TRUE),
    Online_Response = sum(as.numeric(Probe_Response) <= 5, na.rm = TRUE),   # Number of probes rated on-task (1–5)
    Offline_Response = sum(as.numeric(Probe_Response) > 5, na.rm = TRUE),   # Number of probes rated off-task (6–10)
    Probe_Response = mean(as.numeric(Probe_Response), na.rm = TRUE),
    if (unique(Dataset) == "Dataset1") { 
      Detection_Rate = sum(!is.na(First_Saccade_Quadrant) & First_Saccade_Quadrant != "") / n() * 100; # First saccade detection rate over the whole experiment (in %)
      First_Saccade_Latency = mean(as.numeric(First_Saccade_Latency), na.rm = TRUE);
      First_Saccade_Duration = mean(as.numeric(First_Saccade_Duration), na.rm = TRUE);
      Saccade_Count = mean(as.numeric(Saccade_Count), na.rm = TRUE);
      Saccade_Duration = mean(as.numeric(Saccade_Count), na.rm = TRUE);
      Freq_Expected = sum(ifelse(First_Saccade_Quadrant_Category=="Expected",1,0))/N_total;  # Proportion of first saccades directed to the expected quadrant
      Freq_Unexpected = sum(ifelse(First_Saccade_Quadrant_Category == "Unexpected", 1, 0)) / N_total;  # Proportion of first saccades directed to the two unexpected quadrants
      Freq_Previous = sum(ifelse(First_Saccade_Quadrant_Category == "Previous", 1, 0)) / N_total;  # Proportion of first saccades directed to the previous quadrant
      Total_freq = Freq_Expected + Freq_Unexpected + Freq_Previous}
    )

# General description
Descriptive_general <- Descriptive_participants %>%
  dplyr::summarise(
    N_total = n(),
    N_Women = sum(Perc_Women)/ 100,
    N_Men = sum(Perc_Men)/ 100,
    Perc_Women = mean(Perc_Women),
    Perc_Men = mean(Perc_Men),
    if (unique(df$Dataset) == "Dataset1") { 
    across(c(Age, Accuracy, RT, Probe_Response, Detection_Rate,
             Freq_Expected, Freq_Unexpected, Freq_Previous),
           list(average = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}")
    } else {
      across(c(Age, Accuracy, RT, Probe_Response),
             list(average = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}")
    } 
  )

## Data cleaning --------------------------------------------------------------
df_clean <- df %>% 
  fill_probe_info() %>%   # Propagate probe metadata to the 9 trials preceding each probe
  mutate(Probe_Response = as.numeric(Probe_Response),
         Participant = as.factor(Participant)) %>%
  group_by(Participant) %>%
  mutate(
    RT = as.numeric(as.character(RT)),
    Z_RT=(RT-mean(RT, na.rm=TRUE))/sd(RT, na.rm=TRUE),                                                                          # Within-participant z-scored RT
    Block = as.factor(as.character(Block)),
    Quadrant_Type = as.factor(as.character(Quadrant_Type)),
    Trial = as.numeric(as.character(Trial)),
    Probe_Number = as.numeric(as.character(Probe_Number)),
    Z_Probe_Response = round((Probe_Response-mean(Probe_Response,na.rm=TRUE))/sd(Probe_Response,na.rm=TRUE),3),                  # Within-participant z-scored mind-wandering rating
    Probe_Response = as.numeric(as.character(Probe_Response)),
    Probe_Response_type = ifelse(as.numeric(Probe_Response) > 5,"Off_task","On_task"),                                          # Dichotomize probe ratings: > 5 = off-task
    Bigram_Type = as.factor(as.character(Bigram_Type)),
    Bigram = as.factor(as.character(Bigram)),
    Bin_10 = as.numeric(Bin_10),
    Response = as.numeric(as.character(Response)),
    Response_Correctness = ifelse(Response_Correctness == "Correct", 1, 0),
    if (unique(df$Dataset) == "Dataset1") { 
      First_Saccade_Latency = as.numeric(First_Saccade_Latency);
      First_Saccade_Duration = as.numeric(First_Saccade_Duration);
      First_Saccade_Amplitude = as.numeric(First_Saccade_Amplitude);
      First_Saccade_Velocity = as.numeric(First_Saccade_Velocity);
      First_Saccade_Quadrant_type = as.factor(First_Saccade_Quadrant_type);
      Saccade_Count = as.numeric(Saccade_Count);
      Saccade_Duration = as.numeric(Saccade_Duration);
      Saccade_Amplitude = as.numeric(Saccade_Amplitude);
      Saccade_Velocity = as.numeric(Saccade_Velocity);
      Fixation_Count = as.numeric(Fixation_Count);
      Fixation_Duration = as.numeric(Fixation_Duration);
      Blink_Count = as.numeric(Blink_Count);
      Blink_Duration = as.numeric(Blink_Duration);
    },
    SD_X = as.numeric(SD_X),
    SD_Y = as.numeric(SD_Y),
    BCEA = as.numeric(BCEA)
  ) %>% 
  filter(
    Participant != "09SK20M", # Participant removed because they reported constant mind wandering score across the experiment
  ) %>%
  filter(RT > 200 & RT <= (quantile(RT, 0.75, na.rm = T) + 3 * IQR(RT, na.rm = T))) %>%  # Exclude anticipations (< 200 ms) and upper RT outliers (Q3 + 3×IQR)
  ungroup() %>% 
  mutate(Participant_Number = dense_rank(Participant)) %>%  # Assign sequential numeric IDs after exclusion
  calculate_trial_learning_index() %>%
  calculate_block_learning_index() %>% 
  flag_probe_time_window()


## Sample description (post-cleaning) ---------------------------------------------------------
# Individual description
Descriptive_participants <- df_clean %>%
  mutate(Probe_Response = as.numeric(Probe_Response),
         Participant = as.factor(Participant)) %>%
  group_by(Participant) %>%
  dplyr::summarise(
    n = n(),
    N_Women = mean(Gender %in% c("F", "f")),
    N_Men = mean(Gender %in% c("M", "m", "H")),
    Perc_Women = mean(Gender %in% c("F", "f")) * 100,
    Perc_Men = mean(Gender %in% c("M", "m", "H")) * 100,
    Age = mean(Age, na.rm = TRUE),
    Accuracy = sum(as.numeric(as.character(Response_Correctness))) / n() * 100,
    RT = mean(as.numeric(RT), na.rm = TRUE),
    Online_Response = sum(as.numeric(Probe_Response) <= 5, na.rm = TRUE),   # Number of probes rated on-task (1–5)
    Offline_Response = sum(as.numeric(Probe_Response) > 5, na.rm = TRUE),   # Number of probes rated off-task (6–10)
    Probe_Response = mean(as.numeric(Probe_Response), na.rm = TRUE),
    if (unique(Dataset) == "Dataset1") { 
      Detection_Rate = sum(!is.na(First_Saccade_Quadrant) & First_Saccade_Quadrant != "") / n() * 100; # First saccade detection rate over the whole experiment (in %)
      First_Saccade_Latency = mean(as.numeric(First_Saccade_Latency), na.rm = TRUE);
      First_Saccade_Duration = mean(as.numeric(First_Saccade_Duration), na.rm = TRUE);
      Saccade_Count = mean(as.numeric(Saccade_Count), na.rm = TRUE);
      Saccade_Duration = mean(as.numeric(Saccade_Count), na.rm = TRUE);
      Freq_Expected = sum(ifelse(First_Saccade_Quadrant_Category=="Expected",1,0))/N_total;  # Proportion of first saccades directed to the expected quadrant
      Freq_Unexpected = sum(ifelse(First_Saccade_Quadrant_Category == "Unexpected", 1, 0)) / N_total;  # Proportion of first saccades directed to the two unexpected quadrants
      Freq_Previous = sum(ifelse(First_Saccade_Quadrant_Category == "Previous", 1, 0)) / N_total;  # Proportion of first saccades directed to the previous quadrant
      Total_freq = Freq_Expected + Freq_Unexpected + Freq_Previous}
  )

# General description
Descriptive_general <- Descriptive_participants %>%
  dplyr::summarise(
    N_total = n(),
    N_Women = sum(Perc_Women)/ 100,
    N_Men = sum(Perc_Men)/ 100,
    Perc_Women = mean(Perc_Women),
    Perc_Men = mean(Perc_Men),
    if (unique(df_clean$Dataset) == "Dataset1") { 
      across(c(Age, Accuracy, RT, Probe_Response, Detection_Rate,
               Freq_Expected, Freq_Unexpected, Freq_Previous),
             list(average = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}")
    } else {
      across(c(Age, Accuracy, RT, Probe_Response),
             list(average = mean, sd = sd, min = min, max = max), .names = "{.col}_{.fn}")
    } 
  )

# Mind wandering by time on task
df_lme <- df_clean %>%
  filter(Probe_Number != "") %>%
  mutate(Block = as.numeric(as.character(Block))) %>%
  dplyr::select("Block","Probe_Response","Z_Probe_Response","Probe_Number","Trial","Participant","Block") %>%
  mutate(Probe_Response = Probe_Response)

fit <- lmer(Probe_Response ~ Block + (1|Participant), data = df_lme)

summary(fit)
confint(fit)
model_performance(fit)


# Analyzes --------------------------------------------------------------------
## RT by time on task ---------------------------------------------------------
# ---- 1. Non linear Analysis ----
df_nlme <- df_clean %>% 
  filter(Bigram_Type != '') %>%  # Exclude first trial of each block (no bigram defined)
  dplyr::select("RT", "Trial","Bigram_Type", "Participant","Bin_10")  

grouped_data <- groupedData(RT ~ Trial|Bigram_Type, data = df_nlme)  # Required format for nlme grouped structure
grouped_data$Bigram_Type <- relevel(factor(grouped_data$Bigram_Type, 
                                           ordered = FALSE), 
                                    ref = "Low_Probability")  # Set Low_Probability as reference level so High_Probability effects are expressed as differences

# Fit Constrained model
initVals <- stats::getInitial(RT ~ SSasymp(Trial, yf, y0, log_alpha),  # SSasymp: asymptotic exponential; yf = asymptote, y0 = starting value, log_alpha = rate
                              data = subset(grouped_data, Bigram_Type == "High_Probability"))

control_params <- nlmeControl(
  maxIter = 400,        # outer iterations
  pnlsMaxIter = 150,    # PNLS iterations
  msMaxIter = 200,      # variance-component iterations
  msMaxEval = 800,
  tolerance = 1e-6,     # tighter FE tolerance
  pnlsTol = 1e-4,       # tighter PNLS tolerance
  optimMethod = "nlminb",
  verbose = TRUE        # prints progress so you can see what's happening
)

fit <- nlme(
  method = "ML",        # Maximum likelihood estimation (enables LRT-based model comparison)
  RT ~ SSasymp(Trial, yf, y0, log_alpha),
  data = grouped_data,
  fixed = list(yf ~ Bigram_Type, y0 ~ 1, log_alpha ~ Bigram_Type),  # Bigram_Type modifies all three curve parameters
  random = yf + y0 ~ 1 | Participant,                                          # Asymptote and starting value vary randomly across participants
  start = c(
    yf_Intercept = initVals["yf"],
    yf_Bigram_TypeLow_Probability = 0,
    y0_Intercept = initVals["y0"],
    log_alpha_Intercept = initVals["log_alpha"],
    log_alpha_Bigram_TypeLow_Probability = 0
  ),
  control = control_params
)

summary(fit)

# Fit unconstrained model
initVals <- stats::getInitial(RT ~ SSasymp(Trial, yf, y0, log_alpha),  
                              data = grouped_data)

control_params <- nlmeControl(
  maxIter = 400,       
  pnlsMaxIter = 150,    
  msMaxIter = 200,      
  msMaxEval = 800,
  tolerance = 1e-6,     
  pnlsTol = 1e-4,      
  optimMethod = "nlminb",
  verbose = TRUE        
)

fit_unconstrained <- nlme(
  method = "ML",        
  RT ~ SSasymp(Trial, yf, y0, log_alpha),
  data = grouped_data,
  fixed = list(yf ~ Bigram_Type, y0 ~ Bigram_Type, log_alpha ~ Bigram_Type),  
  random = yf + y0 ~ 1 | Participant,                                          
  start = c(
    yf_Intercept = initVals["yf"],
    yf_Bigram_TypeLow_Probability = 0,
    y0_Intercept = initVals["y0"],
    y0_Bigram_TypeLow_Probability = 0,
    log_alpha_Intercept = initVals["log_alpha"],
    log_alpha_Bigram_TypeLow_Probability = 0
  ),
  control = control_params
)

summary(fit_unconstrained)

anova(fit_unconstrained, fit)

# ---- 2. Graphical representation ----
predictions <- df_nlme %>%
  predict_nlme(fit, newdata = ., interval = 'conf', level = .95)  # Model-predicted values with 95% CI for each observation

df_nlme <- cbind(df_nlme, predictions)

temp <- df_nlme %>% 
  group_by(Bin_10, Bigram_Type) %>%
  dplyr::summarise(RT_Mean = mean(RT, na.rm = TRUE)) %>% 
  mutate(Bin_10 = as.numeric(Bin_10),
         Trial = Bin_10 * 10 - 5,                                                          # Map bin index to trial midpoint
         Trial = ifelse(Bigram_Type=="Low_Probability", Trial-1,Trial))  # Slight horizontal offset to avoid overplotting

df_graph <- left_join(df_nlme, temp, c("Trial","Bigram_Type"))

graph <- df_graph %>% 
  ggplot(aes(x = Trial, color = Bigram_Type, fill = Bigram_Type))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),alpha = 0.3, linewidth=0.3, colour = NA) +  # 95% CI ribbon
  geom_point(aes(y=RT_Mean),size =1.5) +                                                  # Observed bin means
  geom_line(aes(x = Trial, y = Estimate, col = Bigram_Type), linewidth=1) +               # Model-fitted curve
  scale_fill_manual(values=c("High_Probability" = "#3551A3", "Low_Probability" = "#51A335"), labels = c("High probability", "Low probability")) +
  scale_color_manual(values = c("High_Probability" = "#3551A3", "Low_Probability" = "#51A335"), labels = c("High probability", "Low probability")) +
  xlab("Trial") + ylab("RT") + labs(colour = "Location", fill = "Location") +
  labs(title = "Evolution of RTs with time on task") +
  theme_classic() +
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=18,face="bold"), 
        legend.text=element_text(size=16))

graph

ggsave("./Graphs/nlm_RT_Time.png", plot = graph, dpi = 300)  

# ---- 3. Linear analysis ----
df_lme <- df_clean %>% 
  dplyr::select("RT","Trial", "Bigram_Type","Participant", "Bin_10") %>% 
  filter(Bigram_Type!='')

df_lme$Bigram_Type <- droplevels(df_lme$Bigram_Type)

fit_linear <- lmer(RT~ Trial * Bigram_Type + (1|Participant), data=df_lme,REML = FALSE)

check_model(fit_linear)
options(scipen = 999)
summary(fit_linear)
confint(fit_linear)
model_performance(fit_linear)

tapply(df_lme$RT, df_lme$Bigram_Type, mean, na.rm=TRUE)
tapply(df_lme$RT, df_lme$Bigram_Type, sd, na.rm=TRUE)

# Model comparison between linear and nonlinear fit
AIC(fit, fit_linear)
BIC(fit, fit_linear)

## Learning index Analysis ----------------------------------------------------
# ---- 1. Effect of time on task ----
df_lme <- df_clean %>% 
  mutate(Learning_Index = Learning_Index_Block) %>% 
  filter(Learning_Index != "",
  ) %>%
  dplyr::select("Learning_Index", "Participant","Probe_Response","Block") %>% 
  group_by(Participant, Block) %>% 
  dplyr::summarise(Learning_Index = mean(Learning_Index, na.rm=TRUE),
                   Probe_Response = mean(Probe_Response, na.rm=TRUE),
                   Block = mean(as.numeric(as.character(Block))))  # Collapse to one row per participant × block

fit <- lmer(Learning_Index ~ Block + (1|Participant), data=df_lme)  # Block-level learning index regressed on block number; random intercept per participant

check_model(fit)
options(scipen = 999)  # Suppress scientific notation in output
summary(fit)
confint(fit)
model_performance(fit)

# Select time window for testing if learning index differs from zero
df_late <- df_lme %>%
  filter(Block > 10) %>% # 0 5 10 15
  group_by(Participant) %>%
  dplyr::summarise(
    Learning_Index = mean(Learning_Index, na.rm = TRUE),
    .groups = "drop"
  )

# Wilcoxon test
wilcox.test(
  df_late$Learning_Index,
  mu = 0,
  alternative = "greater",
  exact = FALSE
)

mean(df_late$Learning_Index, na.rm = TRUE)
median(df_late$Learning_Index, na.rm = TRUE)

# ---- 2. Graphical representation ----
predictions <- ggpredict(fit, terms = c("Block [all]"))  # Marginal predictions across all observed block values

predictions$Block = predictions$x

df_graph <- left_join(df_lme, predictions, c("Block"))

temp <- df_lme %>% 
  group_by(Block) %>%
  dplyr::summarise(Mean_Index = mean(Learning_Index, na.rm = TRUE))  # Observed block means for plotting

df_graph <- left_join(df_graph, temp, "Block")

graph <-df_graph %>% 
  ggplot(aes(x = Block, ymin = predicted-std.error, ymax = predicted+std.error)) +
  geom_point(aes(y=Mean_Index), size=2.7,color = "#3551A3") +   # Observed block means
  geom_line(aes(x = Block, y = predicted), linewidth=1) +       # Model-predicted trend
  geom_ribbon(alpha = 0.3, linewidth=0.3) +                     # ±1 SE ribbon
  xlab("Block") + ylab("Learning bias") +
  labs(title = "Evolution of learning bias \nduring the experiment") +
  theme_classic() +
  theme(plot.title=element_text(size=16,face="bold",hjust=0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=18,face="bold"), 
        legend.text=element_text(size=16))

graph

ggsave("./Graphs/lm_Learning_Time.png", plot = graph, dpi = 300)  
# ---- 3. Effect of mind wandering ----
df_lme <- df_clean %>% 
  droplevels() %>% 
  mutate(Learning_Index = Learning_Index_Continuous,
         Probe_Response = as.numeric(as.character(Z_Probe_Response)),  # Use within-participant z-scored MW ratings
         Block = as.numeric(as.character(Block))) %>% 
  filter(Learning_Index != "",
         Trial_Probe != "",      # Retain only the 10 trials before the probe
         #Trial_Probe != "Pre-probe_10", # Retain only the 5 trials before the probe
         #Time_Window == "True" # Retain only trials within the pre-probe window
  ) %>%
  dplyr::select(
    "Trial_Probe","Learning_Index","Probe_Response","Z_Probe_Response","Probe_Number", 
    "Trial", "Participant","Bin_10","Block") %>% 
  group_by(Block,Participant) %>% 
  dplyr::summarise(Probe_Response=mean(as.numeric(as.character(Probe_Response)), na.rm=TRUE),
                   Learning_Index=mean(as.numeric(as.character(Learning_Index)), na.rm=TRUE),
                   Block = mean(as.numeric(as.character(Block))),
  )  # Collapse to one row per participant × block


fit <- lmer(Learning_Index ~ Probe_Response + Block + (1|Participant), data=df_lme)  # Continuous learning index predicted by MW rating, controlling for block; random intercept per participant

check_model(fit)
summary(fit)

# ---- 4. Graphical representation ----
predictions <- ggpredict(fit, terms = "Probe_Response [all]")  # Marginal predictions across all observed MW values, holding Block at its mean

predictions$Probe_Response = predictions$x

df_graph <- left_join(df_lme, predictions, "Probe_Response")

temp <- df_lme %>% 
  group_by(Probe_Response) %>%
  dplyr::summarise(Mean_Index = mean(Learning_Index, na.rm = TRUE),
                   sd_Index = sd(Learning_Index, na.rm = TRUE))  # Observed means and SD per unique probe rating

df_graph <- left_join(df_graph, temp, "Probe_Response")

graph <- df_graph %>% 
  ggplot(aes(x = Probe_Response, ymin = predicted-std.error, ymax = predicted+std.error)) +
  geom_point(aes(y=Mean_Index), size=2.1,color = "#3551A3") +  # Observed means per probe rating
  geom_ribbon(alpha = 0.3, linewidth=0.3) +                    # ±1 SE ribbon
  geom_line(aes(x = Probe_Response, y = predicted, 
  ), linewidth=1) +                                            # Model-predicted trend
  xlab("Mind Wandering (z score)") + ylab("Learning bias") + 
  labs(title = "Evolution of learning bias with mind wandering") +
  theme_classic() +
  theme(plot.title=element_text(size=18,face="bold",hjust=0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=16))

graph

ggsave("./Graphs/lme_Learning_Z_MW.png", plot = graph, dpi = 300)  

## First saccade probability analysis (Dataset 1 only) -----------------------------------------
# ---- 1. Effect of time on task ----
# First saccade toward expected location
target_variable <- "Expected"
graph_label <- tolower(target_variable)

df_logistic <- data %>% 
  filter(!is.na(First_Saccade_Quadrant) & First_Saccade_Quadrant != "") %>% # Exclude trials with no recorded first saccade
  dplyr::select("Participant","First_Saccade_Quadrant_type", "First_Saccade_Quadrant_Category", "Bin_10", "Trial", "Block") %>% 
  mutate(First_Saccade_Quadrant_type = as.factor(ifelse(First_Saccade_Quadrant_Category == target_variable, 1, 0)), # Binary outcome: 1 = first saccade toward expected quadrant
         Trial_scaled = datawizard::standardize(Trial)) # Standardize trial number to improve model convergence

fit_expected <- glmer(First_Saccade_Quadrant_type ~ Trial_scaled + (1|Participant), 
             data = df_logistic, family = binomial)  # Logistic mixed-effects model: probability of expected first saccade as a function of trial; random intercept per participant

check_model(fit_expected)
performance::check_overdispersion(fit_expected)
options(scipen = 999)  # Suppress scientific notation in output
summary(fit_expected)
confint(fit_expected, parm = "Trial_scaled")
model_performance(fit_expected)

# Descriptive statistics by Block
tapply(as.numeric(as.character(df_logistic$First_Saccade_Quadrant_type)), 
       as.numeric(df_logistic$Block), mean, na.rm=TRUE)  # Mean probability of expected first saccade per block
tapply(as.numeric(as.character(df_logistic$First_Saccade_Quadrant_type)), 
       as.numeric(df_logistic$Block), sd, na.rm=TRUE)    # SD of expected first saccade probability per block

target_variable <- "Previous"

df_logistic <- data %>% 
  filter(First_Saccade_Quadrant != "") %>% 
  dplyr::select("Participant","First_Saccade_Quadrant_type", "First_Saccade_Quadrant_Category", "Bin_10", "Trial", "Block") %>% 
  mutate(First_Saccade_Quadrant_type = as.factor(ifelse(First_Saccade_Quadrant_Category == target_variable, 1, 0)),
         Trial_scaled = datawizard::standardize(Trial))

fit_previous <- glmer(First_Saccade_Quadrant_type ~ Trial_scaled + (1 | Participant), 
                      data = df_logistic, family = binomial)

check_model(fit_previous)
performance::check_overdispersion(fit_previous)
options(scipen = 999) 
summary(fit_previous)
confint(fit_previous, parm = "Trial_scaled")
model_performance(fit_previous)

# Descriptive statistics by Block
tapply(as.numeric(as.character(df_logistic$First_Saccade_Quadrant_type)), 
       as.numeric(df_logistic$Block), mean, na.rm=TRUE)  # Mean probability of expected first saccade per block
tapply(as.numeric(as.character(df_logistic$First_Saccade_Quadrant_type)), 
       as.numeric(df_logistic$Block), sd, na.rm=TRUE)    # SD of expected first saccade probability per block



df_expected_vs_unexpected <- data %>%
  filter(
    First_Saccade_Quadrant != "",
    First_Saccade_Quadrant_Category %in% c("Expected", "Unexpected")
  ) %>%
  mutate(
    Expected_vs_Unexpected = as.numeric(First_Saccade_Quadrant_Category == "Expected"),
    Trial_scaled = datawizard::standardize(Trial)
  )

fit_expected_advantage <- glmer(
  Expected_vs_Unexpected ~ Trial_scaled + (1 | Participant),
  data = df_expected_vs_unexpected,
  family = binomial
)

summary(fit_expected_advantage)
confint(fit_expected_advantage, parm = "Trial_scaled")

# ---- 2. Graphical representation ----
df_logistic$Predicted_Prob <- predict(fit, newdata = df_logistic, type = "response", re.form = NA) # Predict probabilities

df_graph <- cbind(df_logistic, predict(fit, newdata = df_logistic, type = "link",
                                       se = TRUE, re.form = NA))  # Predictions on the log-odds scale with SE, ignoring random effects (population-level)

df_graph <- within(df_graph, {
  PredictedProb <- plogis(fit) # explicitly the column "fit"
  LL <- plogis(fit - 1.96 * se.fit)  # Lower bound of 95% CI back-transformed to probability scale
  UL <- plogis(fit + 1.96 * se.fit)  # Upper bound of 95% CI back-transformed to probability scale
})


temp <- df_logistic %>% 
  group_by(Bin_10) %>%
  dplyr::summarise(Mean_Saccade = mean(as.numeric(as.character(First_Saccade_Quadrant_type)), na.rm = TRUE)) %>%  # Observed proportion of expected first saccades per 10-trial bin
  mutate(Bin_10 = as.numeric(Bin_10),
         Trial = Bin_10 * 10 - 5)  # Map bin index to trial midpoint

df_graph$Trial <- as.numeric(df_graph$Trial)
df_graph <- left_join(df_graph, temp, c("Trial"))

graph <- df_graph %>% 
  ggplot(aes(x = Trial, y = PredictedProb)) + 
  geom_point(aes(y=Mean_Saccade), size=2.7,position = position_dodge(width = 0.4),color = "#3551A3") +  # Observed bin proportions
  geom_ribbon(aes(ymin = LL,ymax = UL), alpha = 0.2) +  # 95% CI ribbon
  geom_line(linewidth=1) +                               # Model-predicted probability curve
  xlab("Trial") + ylab("Probability") + 
  labs(title = paste("Probability of a first saccade \ntoward", graph_label, "quadrant")) +
  theme_classic() +
  theme(plot.title=element_text(size=20,face="bold",hjust=0.5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=18,face="bold"), 
        legend.text=element_text(size=16))

graph 

ggsave("./Graphs/proba_expected_time.png", plot = graph, dpi = 300)  

# ---- 3. Effect of mind wandering ----
target_variable <- "Expected"
graph_label <- tolower(target_variable)

df_logistic <- df_clean %>% 
  mutate(Probe_Response=Z_Probe_Response) %>%  # Use within-participant z-scored MW ratings
  filter(Trial_Probe != "",
         First_Saccade_Quadrant !="",          # Exclude trials with no recorded first saccade
  ) %>%
  dplyr::select("Participant","First_Saccade_Quadrant_type","First_Saccade_Quadrant_Category","Bin_10","Probe_Response","Bin_10","Block") %>% 
  group_by(Block,Participant) %>% 
  mutate(First_Saccade_Quadrant_type = as.factor(ifelse(First_Saccade_Quadrant_Category==target_variable,1,0)),  # Binary outcome: 1 = first saccade toward expected quadrant
         Block = mean(as.numeric(as.character(Block))))

fit <- glmer(First_Saccade_Quadrant_type ~ Probe_Response + Block + (1|Participant),
             data = df_logistic, family = binomial)  # Logistic mixed-effects model: probability of expected first saccade predicted by MW rating, controlling for block

check_model(fit)
performance::check_overdispersion(fit)
options(scipen = 999)  # Suppress scientific notation in output
summary(fit)

model_performance(fit)

# Model predictions
df_logistic$Predicted_Prob <- predict(fit, newdata = df_logistic, type = "response", re.form = NA)  # Probabilities

df_graph <- cbind(df_logistic, predict(fit, newdata = df_logistic, type = "link",
                                       se = TRUE, re.form = NA))  # Predictions on the log-odds scale with SE, ignoring random effects (population-level)
df_graph <- within(df_graph, {
  PredictedProb <- plogis(fit) # explicitly the column "fit"
  LL <- plogis(fit - 1.96 * se.fit)  # Lower bound of 95% CI back-transformed to probability scale
  UL <- plogis(fit + 1.96 * se.fit)  # Upper bound of 95% CI back-transformed to probability scale
})


temp <- df_logistic %>% 
  group_by(Probe_Response) %>%
  dplyr::summarise(Prob = mean(as.numeric(as.character(First_Saccade_Quadrant_type)), na.rm = TRUE),  # Observed proportion of expected first saccades per MW rating
                   n = as.integer(n()/5))

df_graph <- left_join(df_graph, temp, c("Probe_Response"))

graph <- df_graph %>% 
  ggplot(aes(x = Probe_Response, y = PredictedProb)) + 
  geom_point(aes(y=Prob), size=2.7,position = position_dodge(width = 0.4),color = "#3551A3") +  # Observed proportions per MW rating
  geom_ribbon(aes(ymin = LL,ymax = UL), alpha = 0.2) +  # 95% CI ribbon
  geom_line(linewidth=1) +                               # Model-predicted probability curve
  xlab("Mind wandering (z score)") + ylab("Probability") +
  labs(title = paste("Probability of a first saccade toward the\n", graph_label, "quadrant with mind wandering ")) +
  theme_classic() +
  theme(plot.title=element_text(size=16,face="bold",hjust=0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=20,face="bold"), 
        legend.text=element_text(size=16))

graph

ggsave("./Graphs/glme_Saccade_MW2.png", plot = graph, dpi = 300) 

## Oculometric profile of MW --------------------------------------------------
# ---- 1. Analysis across all variables ----
### Variables selection
if (unique(df_clean$Dataset) == 'Dataset1') {
  variables <- c("First_Saccade_Duration","First_Saccade_Amplitude","First_Saccade_Velocity","First_Saccade_Latency", 
                 "Saccade_Count", "Saccade_Duration", "Saccade_Amplitude", "Saccade_Velocity", 
                 "Fixation_Count", "Fixation_Duration", "SD_X", "SD_Y", "BCEA")
} else {
  variables <- c("SD_X", "SD_Y", "BCEA") 
}

results_list <- vector("list", length(variables))  # Pre-allocate list to store one result tibble per variable
### Main loop
for (i in seq_along(variables)) {
  var_name <- variables[i]
  var_sym <- rlang::sym(var_name)
  
  # Build filtered + summarized dataframe for the current variable
  df_lme_single <- df_clean %>%
    group_by(Participant) %>%
    filter(
      Trial_Probe != "",
      
      #Use one-sided raw filter for stability/durations
      case_when(
        var_name %in% c("BCEA", "SD_X", "SD_Y") ~
          is_not_outlier_IQR(.data[[var_name]]),  # Apply IQR outlier filter for gaze dispersion variables
        TRUE ~ is_not_outlier_IQR(.data[[var_name]])  # Apply IQR outlier filter for all other oculomotor variables
      )
    ) %>%
    ungroup() %>%
    dplyr::select("Z_Probe_Response", "Probe_Number", "Trial", "Participant", "Bin_10", "Block", "Fixation_Cross_Duration", all_of(var_name)) %>%
    # Corrected transformation - using mutate with ifelse
    mutate(
      Probe_Response = Z_Probe_Response,
      !!var_name := if (var_name %in% c("BCEA", "SD_X", "SD_Y")) {
        log(.data[[var_name]])  # Log-transform gaze dispersion variables to reduce skew
      } else {
        .data[[var_name]]
      }
    ) %>%
    group_by(Participant, Probe_Number) %>%
    dplyr::summarise(
      N = n(),
      Probe_Response = mean(Probe_Response, na.rm = TRUE),
      !!var_name := mean(.data[[var_name]], na.rm = TRUE),
      Block = mean(as.numeric(as.character(Block))),
      Fixation_Cross_Duration = mean(as.numeric(as.character(Fixation_Cross_Duration))),
      .groups = "drop"
    )
  
  # Build formula and fit model (try-catch to avoid loop crash)
  if (var_name %in% c("BCEA", "SD_X", "SD_Y")) {
    formula_str <- paste0(var_name, " ~ Probe_Response + Block + datawizard::standardize(Fixation_Cross_Duration) + (1|Participant)")  # Fixation cross duration added as covariate for gaze dispersion variables
  } else {
    formula_str <- paste0(var_name, " ~ Probe_Response + Block + (1|Participant)")
  }
  
  fit <- tryCatch(
    lmer(as.formula(formula_str), data = df_lme_single),
    error = function(e) { warning(sprintf("lmer failed for %s: %s", var_name, e$message)); NULL }  # Return NULL on convergence failure; loop continues
  )
  
  if (is.null(fit)) {
    # Save NA row if model failed
    results_list[[i]] <- tibble(
      Variable = var_name,
      Beta = NA_real_,
      CI_lower = NA_real_,
      CI_upper = NA_real_,
      t_value = NA_real_,
      df = NA_real_,
      p_value_formatted = NA_character_
    )
    next
  }
  
  # Extract coefficients and CI (safely)
  coefs <- summary(fit)$coefficients
  # guard: ensure Probe_Response row exists
  if (!("Probe_Response" %in% rownames(coefs))) {
    results_list[[i]] <- tibble(
      Variable = var_name,
      Beta = NA_real_,
      CI_lower = NA_real_,
      CI_upper = NA_real_,
      t_value = NA_real_,
      df = NA_real_,
      p_value_formatted = NA_character_
    )
    next
  }
  
  conf <- tryCatch(
    confint(fit, parm = "Probe_Response", method = "Wald"),  # Wald CI for speed; use "profile" for greater accuracy if needed
    error = function(e) matrix(c(NA_real_, NA_real_), nrow = 1, dimnames = list("Probe_Response", c("2.5 %", "97.5 %")))
  )
  
  beta     <- coefs["Probe_Response", "Estimate"]
  t_val    <- coefs["Probe_Response", "t value"]
  df_val   <- coefs["Probe_Response", "df"]
  p_val    <- coefs["Probe_Response", "Pr(>|t|)"]
  ci_low   <- conf["Probe_Response", "2.5 %"]
  ci_high  <- conf["Probe_Response", "97.5 %"]
  
  results_list[[i]] <- tibble(
    Variable = var_name,
    Beta = beta,
    CI_lower = ci_low,
    CI_upper = ci_high,
    t_value = round(t_val,2),
    df = round(df_val,0),
    p_value_formatted = format_p(p_val)
  )
}

# ---- 2. Table creation ----
# Define superscript mapping
superscripts_map <- c("0"="⁰", "1"="¹", "2"="²", "3"="³", "4"="⁴", "5"="⁵", "6"="⁶", "7"="⁷", "8"="⁸", "9"="⁹", "-"="⁻", "+"="")

# Process results table
results_table_display <- bind_rows(results_list) %>%
  mutate(
    # Clean variable names
    Variable = gsub("_", " ", Variable),
    
    # Format p-values with stars
    `p value` = {
      p_clean <- gsub("[=_]", "", p_value_formatted)
      p_num <- suppressWarnings(as.numeric(gsub("<", "", p_clean)))
      case_when(
        grepl("< .001", p_clean) | p_num < 0.001 ~ "< .001***",
        grepl("< .01", p_clean)  | p_num < 0.01  ~ paste0(p_clean, "**"),
        grepl("< .05", p_clean)  | p_num < 0.05  ~ paste0(p_clean, "*"),
        TRUE ~ p_clean
      )
    }
  ) %>%
  # Apply superscript formatting
  mutate(
    Beta = convert_to_superscript(as.numeric(Beta)),
    `95% CI` = paste0("[", 
                      convert_to_superscript(as.numeric(CI_lower)), 
                      ", ", 
                      convert_to_superscript(as.numeric(CI_upper)), 
                      "]")
  ) %>%
  # Clean column names and order
  rename_with(~ gsub("_", " ", .)) %>%
  select(Variable, Beta, `95% CI`, `t value`, df, `p value`)

# Create flextable
results_table_display %>%
  flextable::flextable() %>%
  flextable::set_caption("Regression Results for Oculomotor Variables") %>%
  flextable::autofit() %>%
  flextable::theme_vanilla() %>%
  flextable::bold(i = ~ grepl("\\*", `p value`), j = "p value")  # Bold rows with significant p-values
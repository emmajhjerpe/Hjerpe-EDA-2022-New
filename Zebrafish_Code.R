library(tidyverse, ggplot, readxl)

# xlsx files
zf_data <- read_excel("ZebrafishQuantData_test.xlsx") %>%
  rename(
    total_length = TL,
    yolk_height = YolkHt,
    tail_length = "Lttailfromtipmsc(mm)",
    body_cavity_lenth = BodyCavLen,
    trunk_total = TrunkTotal,
    head_depth = HeadDpth,
    caudal_fin_height = CaudHt,
    trunk_and_fin = "Trunk+Fin",
    dorsal_fin_height = DorsalFin,
    dorsal_fin_length = DorsalFinLen,
    anal_fin_length = AnalFinLen,
    eye_height = EyeHt,
    eye_length = EyeLen,
    hatch_time_hours = "hatchtime(hours)",
    average_velocity = VelAvg,
    max_velocity = VelMax,
    cue_type = W1A2
  ) %>% 
  print()

summary(zf_data)
length(zf_data$max_velocity)

#Summarizing / grouping by cue type for future code/error bars
zf_group<-group_by(zf_data, cue_type)
zf_group

#Calculating T score
alpha = 0.05
degrees.freedom = length(zf_data) - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)
#Error is t

#Comparing Hatch Time and Cue Type
ggplot(data = zf_data) +
  geom_histogram(mapping = aes(x = hatch_time_hours), binwidth = 6,
                 color = "purple4", fill = "purple4") +
  facet_wrap(~ cue_type, scales = "free_y") +
  theme_gray() +
  labs(
    x = "Hatch Time (Hours)",
    y = "Number of Fish",
  )

#Hatch time T-test
alarm_hatch <- c(rnorm(75, mean = 39.88571429, sd = 43.525))
water_hatch <- c(rnorm(75, mean = 5.875029054, sd = 5.84846024))

t.test(alarm_hatch, water_hatch, paired = TRUE)

#Average length comparison
zf_summary_tl <-
  summarize(
    zf_group, 
    mean_length = mean(total_length),
    sem_length = sd(total_length) / sqrt(n()),
    ci_upper_limit = mean_length + t.score * sem_length,
    ci_lower_limit = mean_length - t.score * sem_length)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = total_length, color = hatch_time_hours), width = 0.3) +
  geom_point(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "black", size = 3 ) + scale_color_gradient(low = "purple4", high = "darkorange")+
  geom_linerange(
    data = zf_summary_tl, 
    mapping = aes(x = cue_type, y = mean_length, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "black", size = 2) +
  labs(
    x = "Cue Type",
    y = "Total Length",
    color = "Hatch Time (Hours)"
  ) +
  theme_gray()

#Total length T-Test
alarm_tl <- c(rnorm(75, mean = 3.10E+00, sd = 0.21999453))
water_tl <- c(rnorm(75, mean = 3.191625, sd = 0.187474947))

t.test(alarm_tl, water_tl, paired = TRUE)

#Max Velocity
zf_summary_vmax <-
  summarize(
    zf_group, 
    mean_vmax = mean(max_velocity),
    sem_vmax = sd(max_velocity) / sqrt(n()),
    ci_upper_limit = mean_vmax + t.score * sem_vmax,
    ci_lower_limit = mean_vmax - t.score * sem_vmax)

ggplot(data = zf_data) +
  geom_jitter(mapping = aes(x = cue_type, y = max_velocity, color = hatch_time_hours),
              width = 0.3, size=2)+
  geom_point(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "black", size = 3)+scale_color_gradient(low = "purple4", high = "darkorange")+
  geom_linerange(
    data = zf_summary_vmax, 
    mapping = aes(x = cue_type, y = mean_vmax, ymax = ci_upper_limit, 
                  ymin = ci_lower_limit),
    color = "black", size=2) +
  labs(
    x = "Cue Type",
    y = "Max Velocity",
    color = "Hatch Time (Hours)"
  ) +
  theme_gray()

#Max velocity T-test
alarm_mv <- c(rnorm(75, mean = 3.263073714, sd = 4.4816925))
water_mv <- c(rnorm(75, mean = 1.757685417, sd = 1.794126178))

t.test(alarm_mv, water_mv, paired = TRUE)
# Colour value data analysis 
#two-way annova of color value (lightness) (treatment*factor)
lightness_anova <- aov(Lightness~Treatment * Day, data = colour_value)
summary(lightness_anova)
# cheking assumptions of the annova for Lightness values of color data set
shapiro.test(residuals(lightness_anova))
# residual plots of heme iron data ( residuals vs fitted) 
par(mfrow = c(1, 2))
qqnorm(residuals(lightness_anova))
qqline(residuals(lightness_anova))
plot(lightness_anova, which = 1)
# Homogeneity of variance 
leveneTest(Lightness ~ Treatment * Day, data = colour_value)
#Treatments within Day → Uppercase letters
letters_trt <- cld(
  emmeans(lightness_anova, ~ Treatment | Day),
  Letters = LETTERS,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(lower = .group)
# Days within Treatment → lowercase letters
letters_day <- cld(
  emmeans(lightness_anova, ~ Day | Treatment),
  Letters = letters,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(upper = .group)
# Summary statistics (Mean ± SD)
summary_stats <- colour_value |> 
  group_by(Day, Treatment) |> 
  summarise(
    mean = mean(Lightness),
    sd   = sd(Lightness),
    sem  = sd / sqrt(n()),
    .groups = "drop"
  )
#Merging statistics with letters
final_long_lightness <- summary_stats |> 
  left_join(letters_trt, by = c("Day", "Treatment")) |> 
  left_join(letters_day, by = c("Day", "Treatment"))
# Creating Mean ± SD cell values
final_long_lightness <- final_long_lightness |> 
  mutate(
    cell = sprintf("%.2f ± %.2f%s%s",
                   mean, sd, lower, upper)
  )
# Create main table (Mean ± SD)
main_table_lightness <- final_long_lightness |> 
  dplyr::select(Day, Treatment, cell) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = cell
  )
#Create SEM COLUMN (precision by Day)
sem_column <- final_long_lightness |> 
  group_by(Day) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# creat SEM row ( precision by treatment)
sem_row <- final_long_lightness |> 
  group_by(Treatment) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# Make SEM row character
sem_row_wide <- sem_row |> 
  mutate(
    Day = "SEM",
    SEM = NA_real_
  ) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(
    across(-Day, ~ sprintf("%.3f", .))
  )
# Bind rows safely
Final_table <- main_table_lightness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(SEM = sprintf("%.3f", SEM))
# Add SEM row
sem_row_wd <- sem_row |> 
  dplyr::mutate(Day = "SEM") |> 
  tidyr::pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  )
#Prepare main table (force character)
final_table <- main_table_lightness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(across(everything(), as.character))

# Prepare SEM row (force character)
sem_row_w<- sem_row |> 
  mutate(Day = "SEM") |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(across(everything(), as.character))
# final table format
Final_table_lightness <- bind_rows(final_table, sem_row_w)
# Creating the gt table
gt_tbl_lightness <- gt(Final_table_lightness, rowname_col = "Day") |> 
  tab_header(
    title = md("**Table 6.** Effects of several antioxidants on color values of raw chiken patties over storage days") 
  ) |> 
  tab_source_note(
    source_note = md(
      "*Different lowercase letters indicate significant differences among treatments within the same day; uppercase letters indicate differences among days within the same treatment (P < 0.05).*"
    )
  )
gtsave(gt_tbl_lightness, "Table6_lightness.docx")
# line plot visualization of lightness values
ggplot(final_long_lightness, aes(x = Day, y = mean, group = Treatment, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.15,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  ) +
  labs(
    x = "Storage Days",
    y = "Lightness Value",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
# save the colour value (lightness) line plot
ggsave(
  filename = "lightness_line.tiff",  
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)

# Bar plot for lightness values visulaization
final_long_lightness$lower <- gsub(" ", "", final_long_lightness$lower)
final_long_lightness$upper <- gsub(" ", "", final_long_lightness$upper)
final_long_lightness$signif <- paste0(final_long_lightness$lower, final_long_lightness$upper)
p1 <- ggplot(final_long_lightness, aes(x = Day, y = mean, fill = Treatment)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9,
    color = "black",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.9),
    width = 0.25,
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = signif, y = mean + sd + (0.05 * max(mean + sd))),
    position = position_dodge(width = 0.9),
    size = 2,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" )
  ) +
  labs(
    x = "Storage Days",
    y = "Lightness(L*)",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 8, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 8, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
#saving lightness bar chart 
ggsave(
  "Fig_lightness_bar.tiff",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
# Colour value (Redness) data analysis 
#two-way annova of Heme Iron (treatment*factor)
Redness_anova <- aov(Redness~Treatment * Day, data = colour_value )
summary(Redness_anova)
# cheking assumptions of the annova for Heme Iron data set
shapiro.test(residuals(Redness_anova))
# residual plots of heme iron data ( residuals vs fitted) 
par(mfrow = c(1, 2))
qqnorm(residuals(Redness_anova))
qqline(residuals(Redness_anova))
plot(Redness_anova, which = 1)
# Homogeneity of variance 
leveneTest(Redness ~ Treatment * Day, data = colour_value)
##Post-hoc comparisons (from two-way model)
#Treatments within Day → Uppercase letters
letters_trt <- cld(
  emmeans(Redness_anova, ~ Treatment | Day),
  Letters = LETTERS,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(lower = .group)
# Days within Treatment → lowercase letters
letters_day <- cld(
  emmeans(Redness_anova, ~ Day | Treatment),
  Letters = letters,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(upper = .group)
# Summary statistics (Mean ± SD)
summary_stats <- colour_value |> 
  group_by(Day, Treatment) |> 
  summarise(
    mean = mean(Redness),
    sd   = sd(Redness),
    sem  = sd / sqrt(n()),
    .groups = "drop"
  )
#Merging statistics with letters
final_long_redness <- summary_stats |> 
  left_join(letters_trt, by = c("Day", "Treatment")) |> 
  left_join(letters_day, by = c("Day", "Treatment"))
# Create Mean ± SD cell values
final_long_redness<- final_long_redness |> 
  mutate(
    cell = sprintf("%.2f ± %.2f%s%s",
                   mean, sd, lower, upper)
  )
# Create main table (Mean ± SD)
main_table_redness <- final_long_redness |> 
  dplyr::select(Day, Treatment, cell) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = cell
  )
#Create SEM COLUMN (precision by Day)
sem_column <- final_long_redness |> 
  group_by(Day) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# creat SEM row ( precision by treatment)
sem_row <- final_long_redness |> 
  group_by(Treatment) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# Make SEM row character
sem_row_wide <- sem_row |> 
  mutate(
    Day = "SEM",
    SEM = NA_real_
  ) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(
    across(-Day, ~ sprintf("%.3f", .))
  )
# Bind rows safely
Final_table <- main_table_redness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(SEM = sprintf("%.3f", SEM))
# Add SEM row
sem_row_wd <- sem_row |> 
  dplyr::mutate(Day = "SEM") |> 
  tidyr::pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  )
#Prepare main table (force character)
final_table <- main_table_redness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(across(everything(), as.character))
# Prepare SEM row (force character)
sem_row_w<- sem_row |> 
  mutate(Day = "SEM") |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(across(everything(), as.character))
# final table format
Final_table_redness <- bind_rows(final_table, sem_row_w)
# Creating the gt table
gt_tbl_redness <- gt(Final_table_redness, rowname_col = "Day") |> 
  tab_header(
    title = md("**Table 6.** Effects of several antioxidants on redness value of raw chiken patties over storage days") 
  ) |> 
  tab_source_note(
    source_note = md(
      "*Different lowercase letters indicate significant differences among treatments within the same day; uppercase letters indicate differences among days within the same treatment (P < 0.05).*"
    )
  )
gtsave(gt_tbl_redness, "Table6_redness.docx")

# line plot visualization of colour (redness) values
ggplot(final_long_redness, aes(x = Day, y = mean, group = Treatment, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.15,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  ) +
  labs(
    x = "Storage Days",
    y = "Redness(a*)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
# save the colour value (Redness) line plot
ggsave(
  filename = "redness_line.tiff",  
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)
# Bar plot for Rednessness values visulaization
final_long_redness$lower <- gsub(" ", "", final_long_redness$lower)
final_long_redness$upper <- gsub(" ", "", final_long_redness$upper)
final_long_redness$signif <- paste0(final_long_redness$lower, final_long_redness$upper)
p2 <- ggplot(final_long_redness, aes(x = Day, y = mean, fill = Treatment)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9,
    color = "black",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.9),
    width = 0.25,
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = signif, y = mean + sd + (0.05 * max(mean + sd))),
    position = position_dodge(width = 0.9),
    size = 2,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" )
  ) +
  labs(
    x = "Storage Days",
    y = "Redness(a*)",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 8, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 8, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )

#saving colour(Redness) bar chart 
ggsave(
  "Fig_Redness_bar.tiff",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)

# Colour value (Yellowness) data analysis 
#two-way annova of colour value (treatment*factor)
Yellowness_anova <- aov(Yellowness~Treatment * Day, data = colour_value )
summary(Yellowness_anova)
# cheking assumptions of the annova for yellowness data set
shapiro.test(residuals(Yellowness_anova))
# residual plots of Yellowness data ( residuals vs fitted) 
par(mfrow = c(1, 2))
qqnorm(residuals(Yellowness_anova))
qqline(residuals(Yellowness_anova))
plot(Yellowness_anova, which = 1) 
# Homogeneity of variance 
leveneTest(Yellowness ~ Treatment * Day, data = colour_value)
##Post-hoc comparisons (from two-way model)
#Treatments within Day → Uppercase letters
letters_trt <- cld(
  emmeans(Yellowness_anova, ~ Treatment | Day),
  Letters = LETTERS,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(lower = .group)
# Days within Treatment → lowercase letters
letters_day <- cld(
  emmeans(Yellowness_anova, ~ Day | Treatment),
  Letters = letters,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(upper = .group)
# Summary statistics (Mean ± SD)
summary_stats <- colour_value |> 
  group_by(Day, Treatment) |> 
  summarise(
    mean = mean(Yellowness),
    sd   = sd(Yellowness),
    sem  = sd / sqrt(n()),
    .groups = "drop"
  )
#Merging statistics with letters
final_long_yellowness <- summary_stats |> 
  left_join(letters_trt, by = c("Day", "Treatment")) |> 
  left_join(letters_day, by = c("Day", "Treatment"))
# Create Mean ± SD cell values
final_long_yellowness<- final_long_yellowness |> 
  mutate(
    cell = sprintf("%.2f ± %.2f%s%s",
                   mean, sd, lower, upper)
  )
# Create main table (Mean ± SD)
main_table_yellowness <- final_long_yellowness |> 
  dplyr::select(Day, Treatment, cell) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = cell
  )
#Create SEM COLUMN (precision by Day)
sem_column <- final_long_yellowness |> 
  group_by(Day) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# creat SEM row ( precision by treatment)
sem_row <- final_long_yellowness |> 
  group_by(Treatment) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# Make SEM row character
sem_row_wide <- sem_row |> 
  mutate(
    Day = "SEM",
    SEM = NA_real_
  ) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(
    across(-Day, ~ sprintf("%.3f", .))
  )
# Bind rows safely
Final_table <- main_table_yellowness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(SEM = sprintf("%.3f", SEM))
# Add SEM row
sem_row_wd <- sem_row |> 
  dplyr::mutate(Day = "SEM") |> 
  tidyr::pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  )
#Prepare main table (force character)
final_table <- main_table_yellowness |> 
  left_join(sem_column, by = "Day") |> 
  mutate(across(everything(), as.character))
# Prepare SEM row (force character)
sem_row_w<- sem_row |> 
  mutate(Day = "SEM") |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(across(everything(), as.character))
# final table format
Final_table_yellowness <- bind_rows(final_table, sem_row_w)
# Creating the gt table
gt_tbl_yellowness <- gt(Final_table_yellowness, rowname_col = "Day") |> 
  tab_header(
    title = md("**Table 7.** Effects of several antioxidants on yellowness value of raw chiken patties over storage days") 
  ) |> 
  tab_source_note(
    source_note = md(
      "*Different lowercase letters indicate significant differences among treatments within the same day; uppercase letters indicate differences among days within the same treatment (P < 0.05).*"
    )
  )
gtsave(gt_tbl_yellowness, "Table7_yellowness.docx")
# line plot visualization of color (yellowness) values
ggplot(final_long_yellowness, aes(x = Day, y = mean, group = Treatment, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.15,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  ) +
  labs(
    x = "Storage Days",
    y = "Yelloness(b*)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
# save the colour value (Yellowness) line plot
ggsave(
  filename = "yellowness_line.tiff",  
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)

# Bar plot for yellowness values visualization
final_long_yellowness$lower <- gsub(" ", "", final_long_yellowness$lower)
final_long_yellowness$upper <- gsub(" ", "", final_long_yellowness$upper)
final_long_yellowness$signif <- paste0(final_long_yellowness$lower, final_long_yellowness$upper)
p3 <- ggplot(final_long_yellowness, aes(x = Day, y = mean, fill = Treatment)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9,
    color = "black",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.9),
    width = 0.25,
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = signif, y = mean + sd + (0.05 * max(mean + sd))),
    position = position_dodge(width = 0.9),
    size = 2,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" )
  ) +
  labs(
    x = "Storage Days",
    y = "Yellowness(b*)",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 8, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 8, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )

#saving color(Yellowness) bar chart 
ggsave(
  "Fig_Yellowness_bar.tiff",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
# Color value (Chroma) data analysis 
#two-way annova of color value (treatment*factor)
Chroma_anova <- aov(Chroma~Treatment * Day, data = colour_value )
summary(Chroma_anova)
# Cheking assumptions of the annova for yellowness data set
shapiro.test(residuals(Chroma_anova))
# residual plots of Yellowness data ( residuals vs fitted) 
par(mfrow = c(1, 2))
qqnorm(residuals(Chroma_anova))
qqline(residuals(Chroma_anova))
plot(Chroma_anova, which = 1) 
# Homogeneity of variance 
leveneTest(Chroma ~ Treatment * Day, data = colour_value)
##Post-hoc comparisons (from two-way model)
#Treatments within Day → Uppercase letters
letters_trt <- cld(
  emmeans(Chroma_anova, ~ Treatment | Day),
  Letters = LETTERS,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(lower = .group)
# Days within Treatment → lowercase letters
letters_day <- cld(
  emmeans(Chroma_anova, ~ Day | Treatment),
  Letters = letters,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(upper = .group)
# Summary statistics (Mean ± SD)
summary_stats <- colour_value |> 
  group_by(Day, Treatment) |> 
  summarise(
    mean = mean(Chroma),
    sd   = sd(Chroma),
    sem  = sd / sqrt(n()),
    .groups = "drop"
  )
#Merging statistics with letters
final_long_Chroma <- summary_stats |> 
  left_join(letters_trt, by = c("Day", "Treatment")) |> 
  left_join(letters_day, by = c("Day", "Treatment"))
# Create Mean ± SD cell values
final_long_Chroma <- final_long_Chroma |> 
  mutate(
    cell = sprintf("%.2f ± %.2f%s%s",
                   mean, sd, lower, upper)
  )
# Create main table (Mean ± SD)
main_table_Chroma <- final_long_Chroma |> 
  dplyr::select(Day, Treatment, cell) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = cell
  )
#Create SEM COLUMN (precision by Day)
sem_column <- final_long_Chroma |> 
  group_by(Day) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# creat SEM row ( precision by treatment)
sem_row <- final_long_Chroma |> 
  group_by(Treatment) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# Make SEM row character
sem_row_wide <- sem_row |> 
  mutate(
    Day = "SEM",
    SEM = NA_real_
  ) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(
    across(-Day, ~ sprintf("%.3f", .))
  )
# Bind rows safely
Final_table <- main_table_Chroma |> 
  left_join(sem_column, by = "Day") |> 
  mutate(SEM = sprintf("%.3f", SEM))
# Add SEM row
sem_row_wd <- sem_row |> 
  dplyr::mutate(Day = "SEM") |> 
  tidyr::pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  )
#Prepare main table (force character)
final_table <- main_table_Chroma |> 
  left_join(sem_column, by = "Day") |> 
  mutate(across(everything(), as.character))
# Prepare SEM row (force character)
sem_row_w<- sem_row |> 
  mutate(Day = "SEM") |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(across(everything(), as.character))
# final table format
Final_table_Chroma <- bind_rows(final_table, sem_row_w)
# Creating the gt table
gt_tbl_Chroma <- gt(Final_table_Chroma, rowname_col = "Day") |> 
  tab_header(
    title = md("**Table 8.** Effects of several antioxidants on Chroma value of raw chiken patties over storage days") 
  ) |> 
  tab_source_note(
    source_note = md(
      "*Different lowercase letters indicate significant differences among treatments within the same day; uppercase letters indicate differences among days within the same treatment (P < 0.05).*"
    )
  )
gtsave(gt_tbl_Chroma, "Table8_Chroma.docx")
# line plot visualization of colour (Chroma) values
ggplot(final_long_Chroma, aes(x = Day, y = mean, group = Treatment, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.15,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  ) +
  labs(
    x = "Storage Days",
    y = "Chroma(c*)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
# save the colour value (Chroma) line plot
ggsave(
  filename = "chroma_line.tiff",  
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)

# Bar plot for Chroma values visualization
final_long_Chroma$lower <- gsub(" ", "", final_long_Chroma$lower)
final_long_Chroma$upper <- gsub(" ", "", final_long_Chroma$upper)
final_long_Chroma$signif <- paste0(final_long_Chroma$lower, final_long_Chroma$upper)
p4 <- ggplot(final_long_Chroma, aes(x = Day, y = mean, fill = Treatment)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9,
    color = "black",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.9),
    width = 0.25,
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = signif, y = mean + sd + (0.05 * max(mean + sd))),
    position = position_dodge(width = 0.9),
    size = 2,
    family = "serif"
  ) +
  scale_fill_manual(
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" )
  ) +
  
  labs(
    x = "Storage Days",
    y = "Chroma(c*)",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 8, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 8, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )

#saving colour(Chroma) bar chart 
ggsave(
  "Fig_Chroma_bar.tiff",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
rm(main_table_Hue)
# Colour value (Hue_Angle) data analysis 
#two-way annova of colour value (treatment*factor)
Hue_anova <- aov(Hue_Angle~Treatment * Day, data = colour_value )
summary(Hue_anova)
# cheking assumptions of the annova for yellowness data set
shapiro.test(residuals(Hue_anova))
# residual plots of Yellowness data ( residuals vs fitted) 
par(mfrow = c(1, 2))
qqnorm(residuals(Hue_anova))
qqline(residuals(Hue_anova))
plot(Hue_anova, which = 1) 
# Homogeneity of variance 
leveneTest(Hue_Angle ~ Treatment * Day, data = colour_value)
##Post-hoc comparisons (from two-way model)
#Treatments within Day → Uppercase letters
letters_trt <- cld(
  emmeans(Hue_anova, ~ Treatment | Day),
  Letters = LETTERS,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(lower = .group)
# Days within Treatment → lowercase letters
letters_day <- cld(
  emmeans(Hue_anova, ~ Day | Treatment),
  Letters = letters,
  adjust  = "tukey"
) |> 
  as.data.frame() |> 
  dplyr::select(Day, Treatment, .group) |> 
  dplyr::rename(upper = .group)
# Summary statistics (Mean ± SD + SEM)
summary_stats <- colour_value |> 
  group_by(Day, Treatment) |> 
  summarise(
    mean = mean(Hue_Angle),
    sd   = sd(Hue_Angle),
    sem  = sd / sqrt(n()),
    .groups = "drop"
  )
#Merging statistics with letters
final_long_Hue <- summary_stats |> 
  left_join(letters_trt, by = c("Day", "Treatment")) |> 
  left_join(letters_day, by = c("Day", "Treatment"))
# Create Mean ± SDᵃᴬ cell values
final_long_Hue <- final_long_Hue |> 
  mutate(
    cell = sprintf("%.2f ± %.2f%s%s",
                   mean, sd, lower, upper)
  )
# Create main table (Mean ± SD)
main_table_Hue <- final_long_Hue |> 
  dplyr::select(Day, Treatment, cell) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = cell
  )
#Create SEM COLUMN (precision by Day)
sem_column <- final_long_Hue |> 
  group_by(Day) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# creat SEM row ( precision by treatment)
sem_row <- final_long_Hue |> 
  group_by(Treatment) |> 
  summarise(SEM = mean(sem), .groups = "drop")
# Make SEM row character
sem_row_wide <- sem_row |> 
  mutate(
    Day = "SEM",
    SEM = NA_real_
  ) |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(
    across(-Day, ~ sprintf("%.3f", .))
  )
# Bind rows safely
Final_table <- main_table_Hue |> 
  left_join(sem_column, by = "Day") |> 
  mutate(SEM = sprintf("%.3f", SEM))
# Add SEM row
sem_row_wd <- sem_row |> 
  dplyr::mutate(Day = "SEM") |> 
  tidyr::pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  )
#Prepare main table (force character)
final_table <- main_table_Hue |> 
  left_join(sem_column, by = "Day") |> 
  mutate(across(everything(), as.character))
# Prepare SEM row (force character)
sem_row_w<- sem_row |> 
  mutate(Day = "SEM") |> 
  pivot_wider(
    names_from  = Treatment,
    values_from = SEM
  ) |> 
  mutate(across(everything(), as.character))
# final table format
Final_table_Hue <- bind_rows(final_table, sem_row_w)
# Creating the gt table
gt_tbl_Hue <- gt(Final_table_Hue, rowname_col = "Day") |> 
  tab_header(
    title = md("**Table 9.** Effects of several antioxidants on Hue angle value of raw chiken patties over storage days") 
  ) |> 
  tab_source_note(
    source_note = md(
      "*Different lowercase letters indicate significant differences among treatments within the same day; uppercase letters indicate differences among days within the same treatment (P < 0.05).*"
    )
  )
gtsave(gt_tbl_Hue, "Table8_Hue_angle.docx")
# line plot visualization of colour (Hue angle) values
ggplot(final_long_Hue, aes(x = Day, y = mean, group = Treatment, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    width = 0.15,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
  ) +
  labs(
    x = "Storage Days",
    y = "Hue angle(h°)",
    color = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
# save the colour value (Hue angle) line plot
ggsave(
  filename = "Hue_angle_line.tiff",  
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)

# Bar plot for Hue angle values visulaization
final_long_Hue$lower <- gsub(" ", "", final_long_Hue$lower)
final_long_Hue$upper <- gsub(" ", "", final_long_Hue$upper)
final_long_Hue$signif <- paste0(final_long_Hue$lower, final_long_Hue$upper)
p5 <- ggplot(final_long_Hue, aes(x = Day, y = mean, fill = Treatment)) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9,
    color = "black",
    linewidth = 0.3
  ) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(width = 0.9),
    width = 0.25,
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = signif, y = mean + sd + (0.05 * max(mean + sd))),
    position = position_dodge(width = 0.9),
    size = 2,
    family = "serif"
  ) +
  # Colors
  scale_fill_manual(
    values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" )
  ) +
  labs(
    x = "Storage Days",
    y = "Hue angle(h°)",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 8, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 8, family = "serif", face = "bold"),
    axis.text.x  = element_text(family = "serif"),
    axis.text.y  = element_text(family = "serif"),
    legend.title = element_blank(),
    legend.text  = element_text(family = "serif"),
    legend.position = "bottom"
  )
#saving colour(Hue angle) bar chart 
ggsave(
  "Fig_Hue_angle_bar.tiff",
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600,
  compression = "lzw"
)
# merging five colour figure in one figure
install.packages("patchwork")
library(patchwork)
p1 <- p1 + theme(legend.position = "none")
p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")

install.packages("cowplot")
library(cowplot)
legend <- get_legend(
  p1 +
    labs(fill = "Treatment") +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title.position = "top",
      legend.title.align = 0.5,
      legend.text  = element_text(family = "serif", face = "bold"),
      legend.title = element_text(family = "serif", face = "bold")
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        title.position = "top",
        title.hjust = 0.5)  
    )
)


top_part  <- p1 | p2
middle_part <- p3 | p4
bottom_part <- p5 | legend   

combined_plot <-
  top_part /
  middle_part /
  bottom_part 

# saving colour value figure
ggsave("combined_plot.tiff",
       combined_plot,
       device = "tiff",
       type = "cairo",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 600,
       compression = "lzw")

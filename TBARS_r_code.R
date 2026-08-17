#Separating long format TBARS data from original data set
TBARS_long <- df |> 
  dplyr::select(
    Treatment = `Treatment_TBARS`,
    Replicate,
    Day1_TBARS,
    Day5_TBARS,
    Day10_TBARS
  ) |> 
  pivot_longer(
    cols = c(Day1_TBARS, Day5_TBARS, Day10_TBARS),
    names_to = "Day",
    values_to = "TBARS"
  ) |> 
  mutate(
    Day = str_remove(Day, "_TBARS"),
    Treatment = factor(Treatment, levels = c("T0", "T1", "T2", "T3", "T4")),
    Day = factor(Day, levels = c("Day1", "Day5", "Day10")),
    Replicate = factor(Replicate),
    TBARS= as.numeric(TBARS)
  )
#Checking replicate number per treatment, day
TBARS_long |> 
  count(Treatment, Day)
#Two way annova (Treatment*Day)
TBARS_anova <- aov(TBARS ~ Treatment * Day, data = TBARS_long)
summary(TBARS_anova)
# Checking assumptions of the annova for tbars
shapiro.test(residuals(TBARS_anova))
# residual plots ( residuals vs fitted) 
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
qqnorm(residuals(TBARS_anova))
qqline(residuals(TBARS_anova))
plot(TBARS_anova, which = 1)
# Homogeneity of variance 
leveneTest(TBARS ~ Treatment * Day, data = TBARS_long)
#Post-hoc comparisons (from two-way model)
# Treatments within Day (Uppercase letters)
cld_trt <- emmeans(TBARS_anova, ~ Treatment | Day) |> 
  multcomp::cld(Letters = LETTERS, adjust = "tukey") |> 
  as.data.frame() |> 
  mutate(upper = str_remove_all(.group, "\\s+")) |> 
  dplyr::select(Day, Treatment, upper)
# Days within Treatment (lowercase letters)
cld_day <- emmeans(TBARS_anova, ~ Day | Treatment) |> 
  multcomp::cld(Letters = letters, adjust = "tukey") |> 
  as.data.frame() |> 
  mutate(lower = str_remove_all(.group, "\\s+")) |> 
  dplyr::select(Day, Treatment, lower)
#Summary statistics per replicate group
summary_stats <- TBARS_long |> 
  group_by(Treatment, Day) |> 
  summarise(
    Mean = mean(TBARS),
    SD   = sd(TBARS),
    n    = n(),
    .groups = "drop"
  )
#Merge statistics with letters
Cell_data <- summary_stats |> 
  left_join(cld_trt, by = c("Day", "Treatment")) |> 
  left_join(cld_day, by = c("Day", "Treatment")) |> 
  mutate(
    mean_sd = sprintf("%.2f ± %.2f", Mean, SD),
    letters = paste0(upper, lower),
    final_cell = paste0(mean_sd, "<sup>", letters, "</sup>")
  )
# Wide table for GT
final_display <- Cell_data |> 
  dplyr::select(Day, Treatment, final_cell) |> 
  pivot_wider(names_from = Treatment, values_from = final_cell) |> 
  arrange(Day) |> 
  rename(`Storage Day` = Day)
# GT table
gt_table_tbars <- final_display |> 
  gt() |> 
  tab_header(
    title = md(
      "**Table 2.** Effect of *Piper chaba* extract on the TBARS of fresh chicken patties during storage."
    )
  ) |> 
  fmt_markdown(columns = -`Storage Day`) |> 
  cols_align(align = "center", columns = -`Storage Day`) |> 
  cols_align(align = "left", columns = `Storage Day`) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>%
  opt_row_striping() |> 
  tab_source_note(
    source_note = md(
      "Values are expressed as Mean ± SD (n = 3). 
      Different uppercase letters within the same column indicate significant differences among treatments within the same day (p < 0.05). 
      Different lowercase letters within the same row indicate significant differences among days within the same treatment (p < 0.05) according to Tukey’s HSD test."
    )
  ) |> 
  tab_options(
    table.width = pct(100),
    table.font.size = px(12),
    heading.align = "left",
    column_labels.font.weight = "bold",
    data_row.padding = px(4)
  )
# Export gt table to word 
gtsave(gt_table_tbars, filename = "TBARS_table.docx")

##Visualization of line plot and bar plot of CL data
# Line plot visualization for cl data
P_tbars_line <- ggplot(
  summary_stats,
  aes(
    x = Day,
    y = Mean,
    group = Treatment,
    color = Treatment,
    shape = Treatment,
    linetype = Treatment
  )
  ) +
  geom_line(
    linewidth = 0.55,
    position = position_dodge(width = 0.35)
  ) +
  geom_point(
    size = 2.0,
    position = position_dodge(width = 0.35)
  ) +
  geom_errorbar(
    data = summary_stats,
    aes(
      x = Day,
      ymin = Mean - SD,
      ymax = Mean + SD,
      color = Treatment,
      group = Treatment
    ),
    width = 0.25,
    linewidth = 0.65,
    linetype = "solid",
    position = position_dodge(width = 0.35),
    inherit.aes = FALSE
  ) +
  scale_color_viridis_d(
    option = "viridis",
    begin = 0.15,
    end = 0.90,
    name = "Treatment"
  ) +
  scale_shape_manual(
    values = c(
      "T0" = 16,
      "T1" = 17,
      "T2" = 15,
      "T3" = 18,
      "T4" = 8
    )
  ) +
  scale_linetype_manual(
    values = c(
      "T0" = "solid",
      "T1" = "dashed",
      "T2" = "dotdash",
      "T3" = "longdash",
      "T4" = "twodash"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Day1" = "Day 1",
      "Day5" = "Day 5",
      "Day10" = "Day 10"
    )
  ) +
  labs(
    x = "Storage Period",
    y = ("TBARS"),
    color = "Treatment",
    shape = "Treatment",
    linetype = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(size = 10, color = "black", family = "serif"),
    axis.text.y  = element_text(size = 10, color = "black", family = "serif"),
    legend.title = element_text(size = 11, face = "bold", family = "serif"),
    legend.text  = element_text(size = 10, family = "serif"),
    legend.position = "bottom", 
    legend.margin = margin(t = -5, r = 0, b = 10, l = 0, unit = "pt") 
  )

  P_tbars_line

# Save the Tbars plot (tiff/pdf)
ggsave(
  filename = "TBARS_line_plot.tiff",
  plot = P_tbars_line,
  device = "tiff",
  width = 14,
  height = 10,
  dpi = 600,
  units= "cm",
  compression = "lzw"
  )
#pdf fornat for journal submission
ggplot2::ggsave(
  filename = "TBARS_line_plot.pdf",
  plot = P_tbars_line,                
  device = grDevices::cairo_pdf,     
  width = 14,                      
  height = 10,                     
  units = "cm")
#Telling my R where Ghostscript executable placed
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs10.07.1\\bin\\gswin64c.exe")
#permanently locking times new roman in my pdf
extrafont::embed_fonts(
  file = "TBARS_line_plot.pdf",
  outfile = "TBARS_line_plot.pdf"
  )
## for bar plot visualization 
#preparing plot data
plot_data <- Cell_data |> 
  dplyr::mutate(
    Treatment = factor(Treatment, levels = c("T0", "T1", "T2", "T3", "T4")),
    Day = factor(Day, levels = c("Day1", "Day5", "Day10")),
    signif = letters
  )
# Barplot with SD error bars and significance letters
pd <- position_dodge(width = 0.9)
p_tbars_bar <- ggplot( plot_data,
                    aes(x = Day,
                        y = Mean,
                        fill = Treatment
                    )
  ) +
  geom_col(
    position = pd,
    width = 0.85,
    color = "black",
    linewidth = 0.15
  ) +
  geom_errorbar(
    aes(
      ymin = Mean - SD,
      ymax = Mean + SD
    ),
    position = pd,
    width = 0.25,
    linewidth = 0.35,
    color = "black"
  ) +
  geom_text(
    aes(y = Mean + SD + (0.04 * max(Mean + SD)),
        label = signif),
    position = pd,
    size = 3.5,
    family = "serif"
  ) +
  scale_fill_viridis_d(
    option = "viridis",
    begin = 0.60,
    end = 0.85,
    name = "Treatment"
  ) +
  scale_x_discrete(
    labels = c(
      "Day1" = "Day 1",
      "Day5" = "Day 5",
      "Day10" = "Day 10")
  ) +
  labs(
    x = "Storage Period",
    y = "TBARS",
    fill = "Treatment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 12, family = "serif", face = "bold"),
    axis.title.y = element_text(size = 12, family = "serif", face = "bold"),
    axis.text.x  = element_text(size = 11, family = "serif", color = "black"),
    axis.text.y  = element_text(size = 11, family = "serif", color = "black"),
    legend.title = element_text(size = 12, face = "bold", family = "serif"),
    legend.text  = element_text(size = 11, family = "serif"),
    legend.position = "bottom",
    legend.margin = margin(t = -5, r = 0, b = 10, l = 0, unit = "pt")
  )
  p_tbars_bar
# saving the bar plot for TBARS data(tiff/)
ggsave(
  filename = "TBARS_bar_plot.tiff",
  plot = p_tbars_bar,
  device = "tiff",
  width = 19,
  height = 11,
  units = "cm",
  dpi = 600,
  compression = "lzw"
  )
#pdf fornat for journal submission(bar plot)
ggplot2::ggsave(
  filename = "TBARS_bar_plot.pdf",
  plot = p_tbars_bar,                
  device = grDevices::cairo_pdf,     
  width = 19,                      
  height = 11,                     
  units = "cm"
  )
#Telling my R where Ghostscript executable placed
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs10.07.1\\bin\\gswin64c.exe")
#permanently locking times new roman in my pdf
extrafont::embed_fonts(
  file = "TBARS_bar_plot.pdf",
  outfile = "TBARS_bar_plot.pdf"
  )







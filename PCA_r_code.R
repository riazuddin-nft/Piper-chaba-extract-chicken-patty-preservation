install.packages("tidyverse")
install.packages("ggbiplot")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggsci")
install.packages("psych")
install.packages("cowplot")
install.packages("patchwork")
install.packages("extrafont")
library("tidyverse")
library("ggbiplot")
library("tidyr")
library("dplyr")
library("ggplot2")
library("psych")
library("ggsci")
library("cowplot")
library("patchwork")
library("extrafont")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("vegan")
install.packages("janitor")
install.packages("ggrepel")
install.packages("reshape2")
install.packages("pheatmap")
install.packages("RColorBrewer")
library("FactoMineR")
library("factoextra")
library("vegan")
library("ggrepel")
library("reshape2")
library("pheatmap")
library("RColorBrewer")
# Install packages for reading raw data 
install.packages('readxl')
library('readxl')
# read raw data for analysis
data <- read_excel("C:/Users/USER/Downloads/Piper_Chaba_nayim.xlsx", sheet = "Sheet2")
# cleaning data with dyplyr
piper_chaba <- data |> 
  select(where(~ !all(is.na(.))))
# data reshaping and restructuring using dplyr
colnames(piper_chaba) <- c(
  "Treatment_pH", "Day1_pH", "Day5_pH", "Day10_pH",
  "Treatment_CL", "Day1_CL", "Day5_CL", "Day10_CL",
  "Treatment_TBARS", "Day1_TBARS", "Day5_TBARS", "Day10_TBARS",
  "Treatment_DPPH" , "Day1_DPPH", "Day5_DPPH", "Day10_DPPH",
  "Treatment_HI","Day1_HI","Day5_HI", "Day10_HI",
  "Treatment_CVDay1", "Day1_Lightness", "Day1_Redness", "Day1_Yellowness", "Day1_Chroma", "Day1_Hue_Angle",
  "Treatment_CVDay5", "Day5_Lightness", "Day5_Redness", "Day5_Yellowness", "Day5_Chroma", "Day5_Hue_Angle",
  "Treatment_CVDay10", "Day10_Lightness", "Day10_Redness", "Day10_Yellowness", "Day10_Chroma", "Day10_Hue_Angle",
  "Treatment_TVC(log(CFU/g))", "Day1_TVC", "Day5_TVC", "Day10_TVC")

##Standardize by keeping only one main treatment column and all day columns 
df <- piper_chaba
df <- df |> 
  dplyr::mutate( Treatment = Treatment_pH)

# adding replicate coloumn in my data
df <- df |> 
  group_by(Treatment_pH) |> 
  mutate(Replicate = row_number()) |> 
  ungroup()

## Convert each block to LONG format
ph_long <- df |> 
  select(Treatment= Treatment_pH, Replicate,
         Day1_pH, Day5_pH, Day10_pH) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "pH"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

cl_long <- df |> 
  select(Treatment = Treatment_CL, Replicate,
         Day1_CL, Day5_CL, Day10_CL) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Cooking_Loss"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

tbars_long <- df |> 
  select(Treatment = Treatment_TBARS, Replicate,
         Day1_TBARS, Day5_TBARS, Day10_TBARS) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "TBARS"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

dpph_long <- df |> 
  select(Treatment = Treatment_DPPH, Replicate,
         Day1_DPPH, Day5_DPPH, Day10_DPPH) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "DPPH"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

hi_long <- df |> 
  select(Treatment = Treatment_HI, Replicate,
         Day1_HI, Day5_HI, Day10_HI) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Heme_Iron"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

lightness_long <- df |> 
  select(Treatment= Treatment_CVDay1, Replicate,
         Day1_Lightness, Day5_Lightness, Day10_Lightness) |> 
  pivot_longer(
     cols = starts_with("Day"),
     names_to = "Day",
     values_to = "Lightness"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

redness_long <- df |> 
  select(Treatment= Treatment_CVDay1, Replicate,
         Day1_Redness, Day5_Redness, Day10_Redness) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Redness"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

Yellowness_long <- df |> 
  select(Treatment= Treatment_CVDay1, Replicate,
         Day1_Yellowness, Day5_Yellowness, Day10_Yellowness) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Yellowness"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

chroma_long <- df |> 
  select(Treatment= Treatment_CVDay1, Replicate,
         Day1_Chroma, Day5_Chroma, Day10_Chroma) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Chroma"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

hue_long <- df |> 
  select(Treatment= Treatment_CVDay1, Replicate,
         Day1_Hue_Angle, Day5_Hue_Angle, Day10_Hue_Angle) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "Hue_Angle"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))
# merging color values in a single data frame
colour_final <- lightness_long |>
  full_join(redness_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(Yellowness_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(chroma_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(hue_long, by = c("Treatment", "Replicate", "Day"))

tvc_long <- df |> 
  select(Treatment = `Treatment_TVC(log(CFU/g))`, Replicate,
         Day1_TVC, Day5_TVC, Day10_TVC) |> 
  pivot_longer(
    cols = starts_with("Day"),
    names_to = "Day",
    values_to = "TVC"
  ) |> 
  mutate(Day = as.numeric(str_extract(Day, "\\d+")))

## Final data format for pca analysis
  
final_long <- ph_long |> 
  full_join(cl_long, by = c("Treatment", "Replicate", "Day")) |> 
  full_join(tbars_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(dpph_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(hi_long, by = c("Treatment", "Replicate", "Day")) |>
  full_join(colour_final, by = c("Treatment", "Replicate", "Day")) |>
  full_join(tvc_long, by = c("Treatment", "Replicate", "Day"))

# preparing PCA numeric data
pca_data <- final_long |> 
  select(-Treatment, -Replicate, -Day) |> 
  mutate(across(everything(), as.numeric)) |> 
  drop_na()
#PCA data with scaling
pca_res <- prcomp(
  pca_data,
  center = TRUE,
  scale. = TRUE
  )
# Extracting percentage variance for PC1–PC6
eig_val <- pca_res$sdev^2

scree_df <- data.frame(
  PC = paste0("PC ", seq_along(eig_val)),
  Variance = eig_val / sum(eig_val) * 100
  ) |> 
  slice(1:6) |> 
  mutate(
    PC = factor(PC, levels = paste0("PC ", 1:6)),
    Label = paste0(round(Variance, 1), "%")
  )
# generating scree plot
     scree_plot <- ggplot(scree_df, aes(x = PC,  
                                   y = Variance)
                               ) +
          geom_col(width = 0.75, fill = "steelblue" ) +
          geom_line(aes(group = 1), 
                    linewidth = 0.7, 
                    color = "black"
                    ) +
          geom_point(size = 2, 
                     color = "black"
                     ) +
          geom_text(aes(label = Label), 
            vjust = -0.8, 
            size = 4, 
            family = "serif", 
            fontface = "bold"
                    ) +
          labs(title = "A. Scree Plot", 
               x = "Principal Components (PCs)", 
               y = "Explained Variance (%)"
                   ) +
          scale_y_continuous(
            expand = expansion(mult = c(0, 0.275))
                             ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", family = "serif", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", family = "serif", size = 12),
    axis.text = element_text(family = "serif", color = "black", size = 10),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6)
  )
 scree_plot
 
--------------------------------------------------------------------------------
### PCA trait contribution heat map
   
# scaling data for heatmap
   pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE) 
# Extracting my PCA data 
  loadings <- pca_res$rotation
  
# Converting my data into heatmap format
  loading_df <- as.data.frame(loadings) |> 
    rownames_to_column("Variable") |> 
    pivot_longer(
      cols = starts_with("PC"),
      names_to = "PC",
      values_to = "Loading") |> 
    mutate(PC = stringr::str_replace(PC, "PC", "PC.")) 
# creating bubble size
  loading_df <- loading_df |> 
    filter(PC %in% paste0("PC.", 1:6))
  
# ordering variables
  loading_df$Variable <- factor(loading_df$Variable,
                                levels = rev(
                                unique(loading_df$Variable))
                                )
# bubble heat map
    bubble_heatmap <- ggplot(loading_df, 
                                   aes(x = PC, 
                                       y = Variable)
                                   ) +
    geom_tile(fill = "white", 
              color = "grey85", 
              width = 1, 
              height = 1
              ) +
    geom_point(aes(size = abs(Loading), 
                   color = Loading), 
                   shape = 16
               ) +
    scale_color_gradient2(low = "#7A0010", 
                          mid = "#E6B800", 
                          high = "#005A00", 
                          midpoint = 0,
                          limits = c(-1, 1), 
                          breaks = c(-1,-0.5,0,0.5,1),
                          labels = c("-1.0","-0.5","0.0","0.5","1.0"), 
                          oob = scales::squish,
                          guide = guide_colorbar(title = "Loading", 
                                                 direction = "vertical",
                                                 barheight = unit(4, "cm"), 
                                                 barwidth = unit(0.3, "cm"),
                                                 label.position = "right", 
                                                 title.position = "top", 
                                                 ticks = FALSE)
                                                 ) +
       scale_size(range = c(1, 8), 
                  guide = "none") +
       scale_x_discrete(position = "top", 
                        expand = c(0, 0)
                        ) +
       scale_y_discrete(expand = c(0, 0)
                        ) +
       coord_fixed(ratio = 1) +
       labs(title = "B. PCA-Trait Contribution Chart", 
            color = "Loading", 
            size = "Strength"
            ) +
    theme_bw(base_size = 12) +
    theme( 
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", 
                                family = "serif", 
                                size = 14, hjust = 0.5),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, face = "bold", family = "serif", hjust = 0, size = 8),
      axis.text.y = element_text(family = "serif", face = "bold", size = 8),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      legend.key.height = unit(3.5, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      legend.ticks = element_line(color = "black", linewidth = 0.3),
      legend.ticks.length = unit(c(-0.15, 0), "cm"),
      legend.text.position = "right",
      legend.title.position = "top",
      legend.text = element_text(family = "serif", size = 8, margin = margin(l = 4)),
      legend.title = element_text(family = "serif", face = "bold", size = 10),
      legend.spacing.y = unit(0.10, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = - 5)
    )
    bubble_heatmap
  
--------------------------------------------------------------------------------
# Extracting scores and adding an experimental factors
    scores_df <- as.data.frame(pca_res$x) |> 
      bind_cols(
        final_long |> 
          select(Treatment, Replicate, Day)
      )
# for legend modification
    scores_df <- scores_df |> 
      mutate(
        Day = factor(
          Day,
          levels = c(1,5,10),
          labels = c("Day 1","Day 5","Day 10")
        ),
        Treatment = factor(Treatment)
      )
# biplot loading df
    biplot_loadings_df <- as.data.frame(pca_res$rotation[,1:2]) |> 
      rownames_to_column("Trait") |> 
      rename(
        PC1_arrow = PC1,
        PC2_arrow = PC2
      )
# for treatment clarification
    scores_df$Treatment <- factor(
  scores_df$Treatment,
  levels = c("T0","T1","T2","T3","T4")
    )
# day levels
    day_levels <- c("Day 1", "Day 5", "Day 10")
# color palette days 
   ellipse_lines <- setNames(c("#5B9BD5","#F4A340","#D65F5F"), day_levels)
   ellipse_fills <- setNames(c("#5B9BD5","#F4A340","#D65F5F"), day_levels)
#day shapes 
   day_shapes <- setNames(c(16, 17, 15), day_levels)
# arrow scale 
 arrow_scale <- 1.25 * min(
  max(abs(scores_df$PC1)) / max(abs(biplot_loadings_df$PC1)),
  max(abs(scores_df$PC2)) / max(abs(biplot_loadings_df$PC2))
  )
 biplot_loadings_df$PC1_arrow <- biplot_loadings_df$PC1 * arrow_scale
 biplot_loadings_df$PC2_arrow <- biplot_loadings_df$PC2 * arrow_scale

#pca value extraction
    pc1_var <- summary(pca_res)$importance[2,1] * 100
    pc2_var <- summary(pca_res)$importance[2,2] * 100
    
    pc1_lab <- paste0("PC1 (", round(pc1_var, 1), "%)")
    pc2_lab <- paste0("PC2 (", round(pc2_var, 1), "%)")
# biplot 
    biplot <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
      stat_ellipse(data = subset(scores_df, Day == "Day 1"), aes(x = PC1, y = PC2), geom = "polygon",
                   color = ellipse_lines["Day 1"], fill = ellipse_fills["Day 1"],
                   alpha = 0.10, linewidth = 0.75, level = 0.95, inherit.aes = FALSE) +
      stat_ellipse(data = subset(scores_df, Day == "Day 5"), aes(x = PC1, y = PC2), geom = "polygon",
                   color = ellipse_lines["Day 5"], fill = ellipse_fills["Day 5"],
                   alpha = 0.10, linewidth = 0.75, level = 0.95, inherit.aes = FALSE) +
      stat_ellipse(data = subset(scores_df, Day == "Day 10"), aes(x = PC1, y = PC2), geom = "polygon",
                   color = ellipse_lines["Day 10"], fill = ellipse_fills["Day 10"],
                   alpha = 0.10, linewidth = 0.75, level = 0.95, inherit.aes = FALSE) +
      
      geom_point(aes(color = Treatment, shape = Day), size = 2.75, stroke = 0.6) + 
      
      geom_segment(data = biplot_loadings_df,
                   aes(x = 0, y = 0, xend = PC1_arrow, yend = PC2_arrow),
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                   color = "black", linewidth = 0.55, inherit.aes = FALSE) +
      
      geom_text_repel(data = biplot_loadings_df,
                      aes(x = PC1_arrow, y = PC2_arrow, label = Trait),
                      color = "black", family = "serif", fontface = "bold", size = 3.0,
                      inherit.aes = FALSE, max.overlaps = 30, seed = 1,
                      segment.linetype = "dotted", segment.color = "grey40",
                      segment.size = 0.5, min.segment.length = 0,
                      point.padding = 0, box.padding = 0.8) +
      
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.3) +
      
      ggsci::scale_color_npg(name = "Treatment") +
      scale_fill_manual(values = ellipse_fills, guide = "none") +
      scale_shape_manual(values = day_shapes, name = "Storage Period",
                         guide = guide_legend(override.aes = list(
                           shape = c(16, 17, 15),         
                           color = ellipse_lines[day_levels],
                           size = 3
                         ))
                         ) +
      
      labs(title = "C. Biplot", x = pc1_lab, y = pc2_lab) +
      
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", family = "serif", size = 14, hjust = 0.5),
        axis.title = element_text(face = "bold", family = "serif", size = 12),
        axis.text = element_text(family = "serif", color = "black", size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        plot.background = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold", family = "serif", size = 12),
        legend.text = element_text(family = "serif", size = 10),
        legend.key = element_rect(color="black", fill="white", linewidth=0.45),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -5)
      )
    biplot
    
--------------------------------------------------------------------------------
## Final panel plot for PCA figure 
     final_panel <- ( scree_plot | bubble_heatmap) / biplot + 
                        plot_layout(heights = c(1, 1.3))

# Saving PCA plot ( tiff format)
ggsave(
  filename = "PCA_Panel.tiff",
  plot = final_panel,
  width = 19,
  height = 22,
  units = "cm",
  dpi = 600,
  compression = "lzw"
      )
# saving pdf format
ggplot2::ggsave(
  filename = "PCA_Panel.pdf",
  plot = final_panel,
  device = grDevices::cairo_pdf, 
  width = 19,
  height = 22,
  units = "cm"
  )
#Telling my R where Ghostscript executable placed
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs10.07.1/bin/gswin64c.exe")
extrafont::embed_fonts(
  file = "PCA_Panel.pdf",
  outfile = "PCA_Panel.pdf"
  )

################################################################################
############################# Import Libraries #################################
################################################################################

library(tidyverse)
library(ggrepel) 
library(readxl)
library(scales)
library(gridExtra)
library(cowplot)
library(patchwork)
library(zoo)
library(treemapify)

setwd("Set your root directory here")

################################################################################
################################# FIG 3a&d #####################################
################################################################################
# Import data
data <- read_excel("./I-JEDI Model/Employment_Results.xlsx", sheet = "Fig 2")

# Function to generate chart for Jobs_ths
plot_fig_jobs <- function(df, country_name, fig_title) {
  # Filter data
  df_filtered <- df %>%
    filter(Country == country_name)
  
  # Calculate scale for secondary y-axis
  capacity_scale <- max(df_filtered$Jobs_ths, na.rm = TRUE) / max(df_filtered$Capacity_GW, na.rm = TRUE)
  
  # Set height offset for line chart
  line_offset <- max(df_filtered$Jobs_ths, na.rm = TRUE) * 0.05
  
  # Create plot
  ggplot(df_filtered, aes(x = as.factor(Year), group = Type)) +
    geom_bar(aes(y = Jobs_ths, fill = Type), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
    geom_line(aes(y = (Capacity_GW * capacity_scale) + line_offset, color = Type), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
    geom_point(aes(y = (Capacity_GW * capacity_scale) + line_offset, color = Type), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
    scale_y_continuous(
      name = "Jobs (thousands)",
      breaks = pretty_breaks(),
      sec.axis = sec_axis(~ (. - line_offset) / capacity_scale, name = "Newly installed capacity (GW)", breaks = pretty_breaks())
    ) +
    labs(x = "Year", title = fig_title) +
    theme_bw() +
    theme(panel.border = element_rect(linewidth = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.1, 0.9), 
          legend.key.size = unit(0.6, 'cm'),
          legend.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          text = element_text(size = 14, family = "serif", face = "bold")) +
    scale_fill_manual(values = c("#d65d48", "#599CB4")) +
    scale_color_manual(values = c("#EF8B67", "#92B5CA"))
}

# Function to generate chart for Earnings_billion
plot_fig_earnings_pw <- function(df, country_name, fig_title) {
  # Filter data
  df_filtered <- df %>%
    filter(Country == country_name)
  
  # Calculate scale for secondary y-axis
  capacity_scale <- max(df_filtered$Earnings_billion, na.rm = TRUE) / max(df_filtered$Earning_perGW, na.rm = TRUE)
  
  # Set height offset for line chart
  line_offset <- max(df_filtered$Earnings_billion, na.rm = TRUE) * 0.05
  
  # Create plot
  ggplot(df_filtered, aes(x = as.factor(Year), group = Type)) +
    geom_bar(aes(y = Earnings_billion, fill = Type), stat = "identity", position = position_dodge(width = 0.9), alpha = 1) +
    geom_line(aes(y = (Earning_perGW * capacity_scale) + line_offset, color = Type), linetype = "solid", size = 1, position = position_dodge(width = 0.9), show.legend = FALSE) +
    geom_point(aes(y = (Earning_perGW * capacity_scale) + line_offset, color = Type), size = 3, shape = 18, position = position_dodge(width = 0.9), show.legend = FALSE) +
    scale_y_continuous(
      name = "Job earnings (billion US$)",
      breaks = pretty_breaks(),
      sec.axis = sec_axis(~ (. - line_offset) / capacity_scale, name = "Job earnings per capacity (million US$/GW)", breaks = pretty_breaks())
    ) +
    labs(x = "Year", title = fig_title) +
    theme_bw() +
    theme(panel.border = element_rect(linewidth = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.1, 0.7), 
          legend.key.size = unit(0.5, 'cm'),
          legend.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          text = element_text(size = 14, family = "serif", face = "bold")) +
    scale_fill_manual(values = c("#d65d48", "#599CB4")) +
    scale_color_manual(values = c("#EF8B67", "#92B5CA"))
}

# Create chart Fig 2a - China (Jobs_ths)
fig2a <- plot_fig_jobs(data, "China", "China")
fig2a

# Create chart Fig 2b - India (Jobs_ths)
fig2b <- plot_fig_jobs(data, "India", "India")
fig2b

# Create chart Fig 2c - China (Earnings_billion)
fig2c <- plot_fig_earnings_pw(data, "China", "China")
fig2c

# Create chart Fig 2d - India (Earnings_billion)
fig2d <- plot_fig_earnings_pw(data, "India", "India")
fig2d

ggsave("Fig2a.png", fig2a, width = 6.1, height = 4, dpi=600)
ggsave("Fig2b.png", fig2b, width = 6.1, height = 4, dpi=600)
ggsave("Fig2c.png", fig2c, width = 6.1, height = 4, dpi=600)
ggsave("Fig2d.png", fig2d, width = 6.1, height = 4, dpi=600)

################################################################################
################################# FIG 5a&b #####################################
################################################################################

data5_bcr <- read_excel("./Results/Fig 5a-b Benefit to cost ratio & percentage of low-carbon power.xlsx")

names(data5_bcr) <- c("Country",	"RCP",	"SSP",	"Year",	"Value",	"lower",	"upper",	"LowCarbon")

data5_bcr$Year <- as.character(data5_bcr$Year)

china_data <- data5_bcr %>% filter(Country == "China")

india_data <- data5_bcr %>% filter(Country == "India")

custom_labels <- c("SSP1                SSP2", "SSP1                SSP2")

p1 <- china_data %>%
  ggplot(aes(x = Year, y = Value, fill = interaction(RCP, SSP))) +
  geom_bar(stat = "identity", position = position_dodge(width = 1),width = 0.85) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.6, color = "gray40") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.6, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.6, color = "lightblue") +
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.6, color = "gray40") +
  geom_vline(xintercept = 2, linetype = "dashed", size = 0.6, color = "lightblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1), size = 0.6, width = 0.25) +
  geom_point(aes(y = LowCarbon * max(Value, na.rm = TRUE)),
             color = "seagreen", shape = 18, size = 4, position = position_dodge(width = 1), show.legend = FALSE) +
  scale_y_continuous(
    name = "Benefit to cost ratio",
    trans = pseudo_log_trans(base = 10), # Apply pseudo-log transformation
    limits = c(0, 32),
    breaks = c(0, 1, 5, 10, 15),
    sec.axis = sec_axis(~./max(china_data$Value, na.rm = TRUE)*100, 
                        breaks = c(0, 50, 100),
                        name = "Low-carbon power (%)")
  ) +
  scale_fill_manual(values = c("deepskyblue3", "#FF0066","deepskyblue3", "#FF0066"), name = NULL, labels = c("RCP1.9", "RCP2.6", "", "")) +
  scale_x_discrete(labels = custom_labels) +
  labs(x = "China")+
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.14, 0.73), 
        legend.background =  element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.key.size = unit(0.6, 'cm'),
        legend.text = element_text(size=15),
        axis.text = element_text(size = 13),
        text = element_text(size=18, family = "serif", face = "bold", color="black")) + 
  annotate("text", x=1, y = 30,  label = "2030", family = "serif", fontface = "bold", size = 6) +
  annotate("text", x=2, y = 30, label = "2050", family = "serif", fontface = "bold", size = 6) +
  geom_segment(aes(x = 0.8, y = 30, xend = 0.5, yend = 30),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 1.2, y = 30, xend = 1.45, yend = 30),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 1.8, y = 30, xend = 1.55, yend = 30),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 2.2, y = 30, xend = 2.5, yend = 30),
               arrow = arrow(length = unit(0.1, "cm")))

p1

p2 <- india_data %>%
  ggplot(aes(x = Year, y = Value, fill = interaction(RCP, SSP))) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.85) +
  geom_hline(yintercept = 0, linetype = "solid", size = 0.6, color = "gray40") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.6, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", size = 0.6, color = "lightblue") +
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.6, color = "gray40") +
  geom_vline(xintercept = 2, linetype = "dashed", size = 0.6, color = "lightblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1), size = 0.6, width = 0.25) +
  geom_point(aes(y = LowCarbon * max(Value, na.rm = TRUE)),
             color = "seagreen", shape = 18, size = 4, position = position_dodge(width = 1), show.legend = FALSE) +
  scale_y_continuous(
    name = "Benefit to cost ratio",
    trans = pseudo_log_trans(base = 10), # Apply pseudo-log transformation
    limits = c(-2, 15),
    breaks = c(-2, 0, 1, 5, 10),
    sec.axis = sec_axis(~./max(india_data$Value, na.rm = TRUE)*100, 
                        breaks = c(0, 50, 100),
                        name = "Low-carbon power (%)")
  ) +
  scale_fill_manual(values = c("deepskyblue3", "#FF0066","deepskyblue3", "#FF0066"), name = NULL, labels = c("RCP1.9", "RCP2.6", "", "")) +
  scale_x_discrete(labels = custom_labels) +
  labs(x = "India")+
  theme_bw() +
  theme(panel.border = element_rect(linewidth=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.08), 
        legend.background =  element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.key.size = unit(0.6, 'cm'),
        legend.text = element_text(size=15,face = "bold"),
        axis.text = element_text(size = 13),
        text = element_text(size=18, family = "serif", face = "bold",color="black")) + 
  annotate("text", x=1, y = 13, label = "2030", family = "serif", fontface = "bold", size = 6) +
  annotate("text", x=2, y = 13, label = "2050", family = "serif", fontface = "bold", size = 6) +
  geom_segment(aes(x = 0.8, y = 13, xend = 0.5, yend = 13),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 1.2, y = 13, xend = 1.45, yend = 13),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 1.8, y = 13, xend = 1.55, yend = 13),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 2.2, y = 13, xend = 2.5, yend = 13),
               arrow = arrow(length = unit(0.1, "cm"))) 

p2

ggsave("Fig5a.pdf", p1, width = 6.75, height = 3.65, dpi = 600)
ggsave("Fig5b.pdf", p2, width = 6.75, height = 3.65, dpi = 600)

################################################################################
################################# FIG 5e&f #####################################
################################################################################

China_region_colors <- c(
  "North" = "#FF0000",
  "Northeast" = "#FFA500",
  "East" = "#39A432",
  "South central" = "#0077EE",
  "Southwest" = "#552299",
  "Northwest" = "#00BDCD"
)

India_region_colors <- c(
  "North" = "#FF0000",
  "Northeast" = "#FFA500",
  "East" = "#39A432",
  "Central" = "#0077EE",
  "South" = "#552299",
  "West" = "#00BDCD"
)

color_scale <- scale_fill_gradient(low = "gold", high = "orange")

China_bcr <- read_excel("./Results/Fig 5g 中国分省成本效益比.xlsx",
                        sheet = "2050_SSP1_RCP1.9 （画图数据）",
                        range = "B3:T34",  # Adjust this range as needed
                        col_names = c("Province", "ABBR", "Region", "碳排放_2030_SSP1_1.9", 
                                      "碳排放_2020", "减碳量", "碳价", 
                                      "Total power generation in 2020_TPL", "COST", 
                                      "AD_MEAN", "AD_LOWER", "AD_UPPER", "2050_SSP1", 
                                      "BENEFIT_MEAN", "BENEFIT_LOWER", "BENEFIT_UPPER", 
                                      "BCR_MEAN", "BCR_LOWER", "BCR_UPPER"))

China_bcr <- China_bcr %>%
  filter(Region != "China") %>%
  mutate(across(-c(`Province`, `ABBR`, `Region`), as.numeric))

div_by_1000 <- function(x) x / 1000

# Apply the function to selected columns
cols_to_change <- c("COST", grep("^AD_|^BENEFIT_", names(China_bcr), value = TRUE))
China_bcr[cols_to_change] <- lapply(China_bcr[cols_to_change], div_by_1000)

# Creating scatter plot
p <- China_bcr %>%
  ggplot(aes(x = COST, y = BENEFIT_MEAN, color = Region, size = AD_MEAN, label = ABBR)) +
  geom_point(alpha = 1) + 
  geom_text_repel(family = "serif", size = 4, box.padding = 0.35, point.padding = 0.5, max.overlaps = Inf) +  # 使用 geom_text_repel 自动调整标签位置
  scale_size_continuous(range = c(2, 8), 
                        breaks = seq(0, max(China_bcr$AD_MEAN, na.rm = TRUE), by = 15), 
                        name = "Avoided deaths (thousands)") +  
  scale_color_manual(values = China_region_colors) +
  scale_y_continuous(limits = c(0, 125)) +
  scale_x_continuous(limits = c(0, 7)) +
  labs(x = "Excess investment costs (billion US$)", y = "Health benefits (billion US$)") +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.5),         
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.35, 0.8),  # 调整图例位置
        legend.box = "horizontal",  # 控制图例框的大小
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        axis.text = element_text(size = 15),
        text = element_text(size = 16, family = "serif", face = "bold")) +
  guides(color = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.6, "cm"),
                              override.aes = list(label = rep("", length(unique(China_bcr$Region))))),
         size = guide_legend(override.aes = list(color = "gray"))) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  annotate("polygon", 
           x = c(-Inf, Inf, Inf, -Inf), 
           y = c(-Inf, -Inf, 7, 0), 
           fill = "lightblue", 
           alpha = 0.2) +
  annotate("polygon", 
           x = c(-Inf, Inf, Inf, -Inf), 
           y = c(0, 7, Inf, Inf), 
           fill = "lightpink", 
           alpha = 0.2) + 
  annotate("text", x = 6.3, y = 0, label = "Net benefit < 0", color = "skyblue", family = "serif", fontface = "bold", size = 5) +
  annotate("text", x = 6.3, y = 25, label = "Net benefit > 0", color = "pink2", family = "serif", fontface = "bold", size = 5) +
  annotate("text", x = 6.7, y = 124, label = "China", color = "black", family = "serif", fontface = "bold", size = 7)

p

China_bcr_sorted <- China_bcr %>%
  mutate(Region = factor(Region, levels = c("North", "Northeast", "East", "South central", "Southwest", "Northwest"))) %>%
  arrange(Region, desc(BCR_MEAN)) %>%
  mutate(ABBR = factor(ABBR, levels = rev(unique(ABBR))))

separator_positions <- c(5.5, 10.5, 16.5, 23.5, 26.5)
text_positions <- c(2, 7, 12, 18, 25, 28)

# Creating bar plot
a <- China_bcr_sorted %>%
  ggplot(aes(x = BCR_MEAN, y = ABBR, fill = BCR_MEAN)) + 
  geom_col(width = 0.5) + 
  geom_errorbarh(aes(xmin = BCR_LOWER, xmax = BCR_UPPER), height = 0.2) +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  labs(title = "",
       x = "Benefit to cost ratio in China",
       y = "Province") +
  color_scale +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        text = element_text(size = 16, family = "serif", face = "bold")) +
  geom_hline(yintercept = separator_positions, 
             linetype = "dashed", color = "black") +
  annotate("text", x = 54, y = text_positions[6], label = "North", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 54, y = text_positions[5], label = "Northeast", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 54, y = text_positions[4], label = "East", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 54, y = text_positions[3], label = "South central", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 54, y = text_positions[2], label = "Southwest", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 54, y = text_positions[1], label = "Northwest", 
           hjust = 0, size = 4, fontface = "bold", family = "serif")

a

India_bcr <- read_excel("./Results/Fig 5h印度分省成本效益比.xlsx",
                        sheet = "2050_SSP1_RCP2.6",
                        range = "B3:T26",  # Adjust this range as needed
                        col_names = c("ABBR", "Region", "碳排放_2030_SSP1_1.9", 
                                      "碳排放_2020", "减碳量", "碳价", "碳价--来自AIM-2020年",
                                      "Total power generation in 2020_TPL", "COST", 
                                      "AD_MEAN", "AD_LOWER", "AD_UPPER", "2050_SSP1", 
                                      "BENEFIT_MEAN", "BENEFIT_LOWER", "BENEFIT_UPPER", 
                                      "BCR_MEAN", "BCR_LOWER", "BCR_UPPER"))

India_bcr <- India_bcr %>%
  filter(Region != "India") %>%
  mutate(across(-c(`ABBR`, `Region`), as.numeric))

div_by_1000 <- function(x) x / 1000

# Apply the function to selected columns
cols_to_change <- c("COST", grep("^AD_|^BENEFIT_", names(India_bcr), value = TRUE))
India_bcr[cols_to_change] <- lapply(India_bcr[cols_to_change], div_by_1000)

# Creating scatter plot
q <- India_bcr %>%
  ggplot(aes(x = COST, y = BENEFIT_MEAN, color = Region, size = AD_MEAN, label = ABBR)) +
  geom_point(alpha = 1) +
  geom_text_repel(family = "serif", size = 4, box.padding = 0.35, point.padding = 0.5, max.overlaps = Inf) +  # 使用 geom_text_repel 自动调整标签位置
  scale_size_continuous(range = c(2, 8), 
                        breaks = seq(0, max(India_bcr$AD_MEAN, na.rm = TRUE), by = 20), 
                        name = "Avoided deaths (thousands)") +  
  scale_color_manual(values = India_region_colors) +
  scale_y_continuous(limits = c(0, 25)) +
  scale_x_continuous(limits = c(0, 6.5)) +
  labs(x = "Excess investment costs (billion US$)", y = "Health benefits (billion US$)") +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.5),         
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.35, 0.8),
        legend.box = "horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.margin = margin(0, 0, 0, 0),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = NA),
        axis.text = element_text(size = 15),
        text = element_text(size = 16, family = "serif", face = "bold")) +
  guides(color = guide_legend(keywidth = unit(0.6, "cm"), keyheight = unit(0.6, "cm"),
                              override.aes = list(label = rep("", length(unique(India_bcr$Region))))),
         size = guide_legend(override.aes = list(color = "gray"))) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  annotate("polygon", 
           x = c(-Inf, Inf, Inf, -Inf), 
           y = c(-Inf, -Inf, 6.5, 0), 
           fill = "lightblue", 
           alpha = 0.2) +
  annotate("polygon", 
           x = c(-Inf, Inf, Inf, -Inf), 
           y = c(0, 6.5, Inf, Inf), 
           fill = "lightpink", 
           alpha = 0.2) + 
  annotate("text", x = 5.8, y = 0, label = "Net benefit < 0", color = "skyblue", family = "serif", fontface = "bold", size = 5) +
  annotate("text", x = 5.8, y = 7.3, label = "Net benefit > 0", color = "pink2", family = "serif", fontface = "bold", size = 5) +
  annotate("text", x = 6.2, y = 24.7, label = "India", color = "black", family = "serif", fontface = "bold", size = 7)

q

India_bcr_sorted <- India_bcr %>%
  mutate(Region = factor(Region, levels = c("North", "Northeast", "East", "Central", "South", "West"))) %>%
  arrange(Region, desc(BCR_MEAN)) %>%
  mutate(ABBR = factor(ABBR, levels = rev(unique(ABBR))))

separator_positions <- c(3.5, 7.5, 11.5, 15.5, 17.5)
text_positions <- c(1.5, 4.5, 8.5, 12.5, 16.5, 18.5)

# 生成条形图
b <- India_bcr_sorted %>%
  ggplot(aes(x = BCR_MEAN, y = ABBR, fill = BCR_MEAN)) +
  geom_col(width = 0.5) +
  geom_errorbarh(aes(xmin = BCR_LOWER, xmax = BCR_UPPER), height = 0.2) +
  scale_x_reverse() +
  scale_y_discrete(position = "right") +
  labs(title = "",
       x = "Benefit to cost ratio in India",
       y = "State") +
  color_scale +
  theme_bw() +
  theme(panel.border = element_rect(linewidth = 1.5), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12),
        text = element_text(size = 16, family = "serif", face = "bold")) +
  geom_hline(yintercept = separator_positions, 
             linetype = "dashed", color = "black") +
  annotate("text", x = 8.5, y = text_positions[6], label = "North", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") +
  annotate("text", x = 8.5, y = text_positions[5], label = "Northeast", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 8.5, y = text_positions[4], label = "East", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 8.5, y = text_positions[3], label = "Central", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 8.5, y = text_positions[2], label = "South", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") + 
  annotate("text", x = 8.5, y = text_positions[1], label = "West", 
           hjust = 0, size = 4, fontface = "bold", family = "serif") 

b

pa <- p + (a / NULL /NULL) + plot_layout(widths = c(2, 1))

pa

qb <- q + (b / NULL /NULL) + plot_layout(widths = c(2, 1))

qb

ggsave("Fig5g.png", pa, width = 10, height = 6, dpi = 600)
ggsave("Fig5h.png", qb, width = 10, height = 6, dpi = 600)

################################################################################
################################## FIG S3 ######################################
################################################################################

data_tree <- read_excel("./Results/附件-premature deaths of China and India-分健康终端-2030-2050年.xlsx", sheet = "SSP1-RCP2.6") %>%
  mutate(`Premature deaths` = round(`Premature deaths` / 1000)) %>%
  filter(Country != "")

create_treemap <- function(data, country) {
  ggplot(data %>% filter(Country == country), 
         aes(area = `Premature deaths`, 
             fill = factor(`Disease-specific`), 
             label = paste(`Disease-specific`, "\n(", `Premature deaths`, ")", sep = ""))) +
    geom_treemap(color = "black", size = 2) +
    geom_treemap_text(fontface = 'bold', colour = 'white', 
                      place = 'centre', grow = FALSE, reflow = TRUE,
                      size = 15) +
    ggtitle(country) +
    scale_fill_manual(values = c("#99b6db", "#e38485", "#90c3aa", "tan", "#cba6c9")) +
    theme_void() +
    theme(panel.border = element_rect(linewidth = 2), 
          legend.position = "none",
          text = element_text(size = 15, family = "Arial", face = "bold"),
          plot.title = element_text(hjust = 0, size = 20),
          plot.margin = margin(2, 2, 2, 2))
}

p1 <- create_treemap(data_tree, "China")
p2 <- create_treemap(data_tree, "India")

combined_tree <- p1 + p2 + plot_layout(widths = c(1, 1))

ggsave("FigS3.png", combined_tree, width = 8, height = 4, dpi = 600)

################################################################################
################################## FIG S4 ######################################
################################################################################

# Read and process data
data5_ad_age <- read_excel("./Results/附件-comparison of 避免死亡人数_各情景_分年龄结构.xlsx", 
                           range = "A2:P17",
                           col_names = c("Country",	"RCP",	"SSP",	"Year",	
                                         "age_2564_mean",	"age_2564_lower",	"age_2564_upper",
                                         "age_6579_mean",	"age_6579_lower",	"age_6579_upper",
                                         "age_80plus_mean",	"age_80plus_lower",	"age_80plus_upper",
                                         "Sum_mean", "Sum_lower", "Sum_upper")) %>%
  pivot_longer(cols = -c(Country, RCP, SSP, Year),
               names_to = c("Type", "Statistic"),
               names_pattern = "(age_2564|age_6579|age_80plus|Sum)_(mean|lower|upper)",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(Type = case_when(
    Type == "age_2564" ~ "Age 25-64",
    Type == "age_6579" ~ "Age 65-79",
    Type == "age_80plus" ~ "Age >80",
    Type == "Sum" ~ "Sum"
  ),
  Year = as.character(Year),
  SSP_RCP = paste(SSP, RCP, sep = "_"),
  across(c(mean, lower, upper), ~round(.x/1000))) %>%
  select(Country, SSP_RCP, Year, Type, mean, lower, upper)

# Function to create plot
create_plot <- function(data, country_name) {
  data_no_sum <- data %>% filter(Type != "Sum")
  data_sum <- data %>% filter(Type == "Sum")
  
  ggplot() +
    geom_bar(data = data_no_sum, 
             aes(x = SSP_RCP, y = mean, fill = Type), 
             stat = "identity", position = "stack", width = 0.6) +
    geom_errorbar(data = data_sum,
                  aes(x = SSP_RCP, ymin = lower, ymax = upper), 
                  position = position_dodge(width = 0.6), width = 0.2, size = 0.8, color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 1) +
    geom_vline(xintercept = 2.5, linetype = "dashed", color = "skyblue", size = 1) +
    scale_fill_manual(values = c("#CEDFEF", "#78B7C9", "#46788E")) +
    scale_y_continuous(name = "Avoided premature deaths (thousands)") +
    facet_grid(. ~ Year, scales = "free_x", space = "free_x") +
    labs(x = country_name, fill = "Age Group") +
    theme_bw(base_size = 15) +
    theme(panel.border = element_rect(linewidth=1),
          panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 18, face = "bold"),
          strip.background = element_rect(linewidth=1, fill = "gray95"),
          legend.position = c(0.1, 0.85), 
          legend.key.size = unit(0.6, 'cm'),
          legend.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          axis.text = element_text(size = 9),
          axis.text.y = element_text(size = 12),
          text = element_text(size = 16, family = "serif", face = "bold"))
}

# Create and save plots
figs4a <- create_plot(data5_ad_age %>% filter(Country == "China"), "China")
figs4b <- create_plot(data5_ad_age %>% filter(Country == "India"), "India")

ggsave("FigS4a.png", figs4a, width = 8, height = 4.5, dpi = 600)
ggsave("FigS4b.png", figs4b, width = 8, height = 4.5, dpi = 600)

################################################################################
################################# FIG S6a&d #####################################
################################################################################

process_data <- function(sheet_name) {
  read_excel("./I-JEDI Model/Employment_Results.xlsx", sheet = sheet_name) %>%
    select(Country, Year, Jobs, Earnings_USD_base2020_PPP) %>%
    group_by(Country, Year) %>%
    summarise(Jobs = sum(Jobs),
              Earnings_USD_base2020_PPP = sum(Earnings_USD_base2020_PPP),
              .groups = "drop") %>%
    mutate(Jobs_ths = Jobs / 1000,
           Earnings_billion = Earnings_USD_base2020_PPP / 10^9)
}

direct_data <- process_data("Results_Direct_Effect")
indirect_data <- process_data("Results_Indirect_Effect")
induced_data <- process_data("Results_Induced_Effect")
total_data <- process_data("Results_Total_Effect")

# Combine all data
all_data <- bind_rows(
  direct_data %>% mutate(Type = "Direct"),
  indirect_data %>% mutate(Type = "Indirect"),
  induced_data %>% mutate(Type = "Induced"),
  total_data %>% mutate(Type = "Total")
)

# Function to create plots
create_plot <- function(data, country, y_var, y_label) {
  plot_data <- data %>% filter(Country == country)
  
  ggplot(plot_data, aes(x = as.factor(Year), y = !!sym(y_var))) +
    geom_bar(data = plot_data %>% filter(Type != "Total"), 
             aes(fill = Type),
             stat = "identity", position = "stack", alpha = 1, width = 0.7) +
    geom_line(data = plot_data %>% filter(Type == "Total"), 
              aes(group = 1), linetype = "dashed", color = "gray", size = 0.8) +
    geom_point(data = plot_data %>% filter(Type == "Total"), 
               aes(group = 1), shape = 18, color = "gray40", size = 3) +
    labs(title = paste(country),
         x = "Year",
         y = y_label) +
    theme_bw() +
    theme(panel.border = element_rect(linewidth = 1.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.13, 0.85), 
          legend.key.size = unit(0.6, 'cm'),
          legend.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          axis.text = element_text(size = 18),
          text = element_text(size = 16, family = "serif", face = "bold")) +
    scale_fill_manual(values = c("Direct" = "#CEDFEF", 
                                 "Indirect" = "#78B7C9",
                                 "Induced" = "#46788E")) +
    scale_y_continuous(breaks = pretty_breaks())
}

# Create plots
plot1 <- create_plot(all_data, "China", "Jobs_ths", "Jobs (Thousands)")
plot2 <- create_plot(all_data, "India", "Jobs_ths", "Jobs (Thousands)")
plot3 <- create_plot(all_data, "China", "Earnings_billion", "Job Earnings (Billion USD)")
plot4 <- create_plot(all_data, "India", "Earnings_billion", "Job Earnings (Billion USD)")

ggsave("FigS6a.png", plot1, width = 6.1, height = 4, dpi = 600)
ggsave("FigS6b.png", plot2, width = 6.1, height = 4, dpi = 600)
ggsave("FigS6c.png", plot3, width = 6.1, height = 4, dpi = 600)
ggsave("FigS6d.png", plot4, width = 6.1, height = 4, dpi = 600)
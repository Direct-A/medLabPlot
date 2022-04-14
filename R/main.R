###
### ---------------
###
### Author: sYc
### Date: 2022-04-01 10:21:46
### Email: songyicheng0@gmail.com
### Blog:
### Update Log: 2022-04-01  First version
###
### ---------------

# 依赖载入
## 主要依赖
library(tidyverse)
library(readxl)
## 统计
library(Rmisc)
library(rstatix)
## 绘图-主题
library(ggthemes)
## 绘图-配色
library(RColorBrewer)
library(colorspace)


# 传入数据构造
## TODO: 重新思考变量命名，更统一，更直观

## 手动统计显著性标记
## 顺序标记
data_list.annotation <- data.frame(
  anno_x = c(),
  anno_y = c(),
  anno_label = c()
)

data_list.range_fa <- list(
  groups = c()
)

## 数据结构组装
data_list <- list(
  data = data_list.data,
  annotation = data_list.annotation,
  range_fa = data_list.range_fa,
  # 分组列-结尾位置  # 分组列-名称  # 分组列-亚组名
  group_end = 1, group_vars = c("groups"), subgroup_name = "groups",
  # 图片-座标轴取值列名
  data_x = "groups", data_y = "beta_gal",
  # 图片-色彩填充依据
  data_fill = "groups",
  # 图片-y轴名称  # 图片-文件名
  y_title = "β-半乳糖苷酶阳性率 (%)", filen_ame = "cell-beta-gal.png",
  x_title = "β-半乳糖苷酶阳性率 (%)",
  # 图片-y轴程度限制  # 图片-y标度
  y_limits = c(0, 100), y_breaks = seq(0, 100, 20),
  # 图片-文件尺寸  # 图片-字号
  g_width = 400, g_height = 500, font_size = 25,
  # 图片-配色方案
  g_color = colorspace::qualitative_hcl(
    n = length(table(data_c$groups)) + 2, palette = "Set2"
  )
)

# 数据预处理
## 宽变长
ifelse(
  ncol(data_list$data) > 2,
  data <- pivot_longer(data_list$data,
    col = (data_list$group_end + 1):ncol(data_list$data),
    names_to = data_list$subgroup_name,
    values_to = data_list$data_y
  ),
  data <- data_list$data
)
glimpse(data)

## 统计描述
data_c <- summarySE(
  data,
  measurevar = data_list$data_y,
  groupvars = data_list$group_vars,
)
## 顺序实现
for (col_name in names(data_list$range_fa)) {
  data_c <- mutate(
    data_c,
    !!(col_name) := factor(
      eval(sym(col_name), data_c),
      levels = data_list$range_fa[[col_name]]
    )
  )
}
glimpse(data_c)
## 统计分析
stat_test <- data %>%
  t_test(rate ~ groups, ref.group = "NCG") %>%
  add_xy_position() %>%
  dplyr::select(p.adj.signif, y.position, xmax)

# 绘图
## 统计显著性标识
p1 <- ggplot(
  data_c,
  aes(
    x = !!sym(data_list$data_x),
    y = !!sym(data_list$data_y),
    fill = !!sym(data_list$data_fill)
  )
) +
  geom_bar(
    stat = "identity",
    colour = "black", # Use black outlines,
    size = 1, # Thinner lines
    width = 0.8,
    position = position_dodge2(1)
  ) +
  geom_errorbar(
    aes(
      ymin = !!sym(data_list$data_y) - se,
      ymax = !!sym(data_list$data_y) + se
    ),
    size = 0.5, # Thinner lines
    width = .3,
    position = position_dodge(0.8)
  ) +
  if (data_list$x_title) {
    p1 + xlab("")
  }
p1 + ylab(data_list$title)

p1 + geom_text(
  data = data_list$annotation,
  aes(x = anno_x, y = anno_y, label = anno_label, fill = NULL),
  family = "serif",
  size = 6
) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = data_list$y_limits,
    breaks = data_list$y_breaks
  ) +
  # 固定色彩搭配
  scale_fill_manual(values = data_list$g_color) +
  theme_classic()

p1 + theme(
  # 分组标记
  legend.position = "none",
  text = element_text(
    family = "serif", size = data_list$font_size, face = "bold"
  ),
  # x 轴标记 45 度
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  axis.title.x = element_blank(),
  axis.line.x.bottom = element_line(size = 1),
  axis.line.y.left = element_line(size = 1),
  axis.ticks = element_line(size = 1),
  axis.ticks.length = unit(0.5, "lines"),
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

# ggbreak 实现 y 轴截断
p1 + scale_y_break(
  expand = c(0, 0),
  limits = data_list$y_limits,
  breaks = data_list$y_breaks
) +

  # ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  # scale_fill_hue(
  #   name = data_fill, # Legend label, use darker colors
  #   breaks = c("AG", "NCG"),
  #   labels = brewer.pal(5, "Set3")
  # ) +
  # scale_x_continuous(expand = c(0, 0)) +

  ggsave(
    p1,
    filename = data_list$filen_ame, device = png,
    width = data_list$g_width, height = data_list$g_height,
    units = "px", dpi = 72
  )

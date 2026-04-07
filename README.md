
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SpatialEpi

Package of data and methods for spatial epidemiology.

## Installation

Get the released version from CRAN:

``` r
devtools::install_github('https://github.com/xuefliang/SpatialEpi')
```


```
library(SpatialEpi)
library(sf)
library(tidyverse)
library(maps)
library(sp)

# Producing Maps
data(scotland)
polygon <- scotland$polygon$polygon
nrepeats <- scotland$polygon$nrepeats
names <- scotland$data$county.names

# 注意：函数名改为 polygon2sf，参数 coordinate.system 改为 crs
sf_polygon <- polygon2sf(polygon, crs = "+proj=utm", names, nrepeats)

par(mfrow = c(1, 2))

# 左图：原始多边形
plot(
  polygon,
  type = "n",
  xlab = "Eastings (km)",
  ylab = "Northings (km)",
  main = "Polygon File"
)
polygon(polygon)

# 右图：sf 对象
plot(sf::st_geometry(sf_polygon), axes = TRUE)
title(xlab = "Eastings (km)", ylab = "Northings (km)", main = "SF Polygon")
plot(sf::st_geometry(sf_polygon[23, ]), add = TRUE, col = "red")


# 获取县级地图并转换为 sf
county.map <- map(
  "county",
  c("pennsylvania", "vermont"),
  fill = TRUE,
  plot = FALSE
)
county_sf <- st_as_sf(county.map)
st_crs(county_sf) <- 4326 # WGS84 (等同于 +proj=longlat)

# 获取州级地图并转换为 sf
state.map <- map("state", fill = TRUE, plot = FALSE)
state_sf <- st_as_sf(state.map)
st_crs(state_sf) <- 4326

# 绑图
plot(st_geometry(county_sf), axes = TRUE, border = "red")
plot(st_geometry(state_sf), add = TRUE, lwd = 2)

# 转换坐标系统                                                                                                          county.grid <- latlong2grid(county_sf)
state.grid <- latlong2grid(state_sf)

plot(st_geometry(state.grid), add = TRUE, lwd = 2)

# 坐标转换
coord <- rbind(c(-73.75, 45.4667), c(-122.6042, 45.6605))
latlong2grid(coord)

# 疾病地图
# 苏格兰地图上绘制均匀分布 Uniform(0,1) 的随机变量
data(scotland_sf)
scotland_sf |>
  mutate(y = runif(nrow(scotland_sf))) |>
  mapvariable(y)

# 宾夕法尼亚州肺癌发病率
data(pennLC_sf)
pennLC_sf |>
  # 按县汇总数据
  group_by(county) |>
  summarise(
    total_population = sum(population),
    total_cases = sum(cases),
    geometry = first(geometry), # 保留第一个几何形状
    .groups = "drop"
  ) |>
  # 转换为 sf 对象并处理坐标
  sf::st_as_sf() |>
  latlong2grid() |>
  # 计算发病率
  mutate(incidence = (total_cases / total_population) * 1000) |>
  # 绘制地图
  mapvariable(incidence)

# Scotland癌症
data(scotland_sf)
scotland_sf |>
  mutate(SMR = cases / expected) |>
  mapvariable(SMR)

# 内部间接标准化控制已知风险因素（此处为分层），我们可以使用 expected()命令计算每个区域的（间接）预期疾病数
data(pennLC_sf)
n.strata <- 16
population <- tapply(pennLC_sf$population, pennLC_sf$county, sum)
cases <- tapply(pennLC_sf$cases, pennLC_sf$county, sum)
expected.cases <- expected(pennLC_sf$population, pennLC_sf$cases, n.strata)

library(dplyr)
data(pennLC_sf)
n.strata <- 16
pennLC_sf %>%
  group_by(county) %>%
  summarise(
    population = sum(population),
    cases = sum(cases),
    .groups = 'drop'
  ) -> county_summary
population <- county_summary$population
cases <- county_summary$cases
expected.cases <- expected(pennLC_sf$population, pennLC_sf$cases, n.strata)

# Empirical Bayes
# Clayton 和 Kaldor 提出了一种经验贝叶斯方法来估计疾病率。该估计值代表了地区标准化死亡比与整体平均相对风险之间的加权折衷。这些估计值比原始标准化死亡比
# 稳定得多
data(scotland_sf)

scotland_sf |>
  mutate(
    RR = {
      x <- AFF
      Xmat <- cbind(x, x^2)
      results <- eBayes(cases, expected, Xmat)
      results$RR
    }
  ) |>
  mapvariable(RR)

#  Cluster Detection
# 聚类检测是对大片小型行政区域进行常规监测，以发现疾病“热点”的证据，而无需对其位置有任何先入为主的假设
data(pennLC_sf)
sf::sf_use_s2(FALSE)
# 获取唯一县级几何
county_sf <- sf::st_as_sf(aggregate(
  pennLC_sf["geometry"],
  by = list(county = pennLC_sf$county),
  FUN = head,
  n = 1
))

# 获取质心并转换坐标
geo <- sf::st_coordinates(sf::st_centroid(county_sf))
geo <- latlong2grid(geo)

# 汇总人口和病例
population <- tapply(pennLC_sf$population, pennLC_sf$county, sum)
cases <- tapply(pennLC_sf$cases, pennLC_sf$county, sum)

# 计算期望病例数
expected.cases <- expected(pennLC_sf$population, pennLC_sf$cases, 16)

# 设置参数
pop.upper.bound <- 0.5
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

# 二项分布分析
#  Kulldorff 方法，用于寻找最可能的聚类
binomial <- kulldorff(
  geo,
  cases,
  population,
  NULL,
  pop.upper.bound,
  n.simulations,
  alpha.level,
  plot
)

# 绑制聚类
cluster <- binomial$most.likely.cluster$location.IDs.included
plot(sf::st_geometry(county_sf), axes = TRUE)
plot(sf::st_geometry(county_sf[cluster, ]), add = TRUE, col = "red")
title("Most Likely Cluster")

# 泊松分布分析
poisson <- kulldorff(
  geo,
  cases,
  population,
  expected.cases,
  pop.upper.bound,
  n.simulations,
  alpha.level,
  plot
)

cluster <- poisson$most.likely.cluster$location.IDs.included
plot(sf::st_geometry(county_sf), axes = TRUE)
plot(sf::st_geometry(county_sf[cluster, ]), add = TRUE, col = "red")
title("Most Likely Cluster Controlling for Strata")

k <- 1250
alpha.level <- 0.05

# 注意：函数名是 besag_newell（下划线），不是 besag.newell（点）
# 不控制分层
results <- besag_newell(
  geo,
  population,
  cases,
  expected.cases = NULL,
  k,
  alpha.level
)

# 控制分层
results <- besag_newell(geo, population, cases, expected.cases, k, alpha.level)


##############################
# 加载数据并关闭 s2
data(pennLC_sf)
sf_use_s2(FALSE)

# 使用 tidyverse 方式处理数据
county_data <- pennLC_sf %>%
  # 按县分组并汇总数据
  group_by(county) %>%
  summarise(
    population = sum(population, na.rm = TRUE),
    cases = sum(cases, na.rm = TRUE),
    geometry = st_union(geometry), # 合并几何体
    .groups = 'drop'
  ) %>%
  # 获取质心坐标
  mutate(
    centroid = st_centroid(geometry),
    coords = st_coordinates(centroid)
  ) %>%
  # 提取坐标
  mutate(
    lon = coords[, "X"],
    lat = coords[, "Y"]
  ) %>%
  select(-coords, -centroid)

# 转换坐标
geo_coords <- county_data %>%
  st_drop_geometry() %>%
  select(lon, lat) %>%
  as.matrix() %>%
  latlong2grid()

# 提取向量数据
population <- county_data$population
cases <- county_data$cases

# 计算期望病例数
expected.cases <- expected(pennLC_sf$population, pennLC_sf$cases, 16)

# 设置参数
pop.upper.bound <- 0.5
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

# 二项分布分析 - Kulldorff 方法
binomial <- kulldorff(
  geo_coords,
  cases,
  population,
  NULL,
  pop.upper.bound,
  n.simulations,
  alpha.level,
  plot
)

# 可视化最可能的聚类
cluster_indices <- binomial$most.likely.cluster$location.IDs.included

county_data %>%
  mutate(
    cluster = row_number() %in% cluster_indices,
    fill_color = if_else(cluster, "red", "white")
  ) %>%
  ggplot() +
  geom_sf(aes(fill = fill_color), color = "black") +
  scale_fill_identity() +
  labs(title = "Most Likely Cluster") +
  theme_minimal()

# 或者使用基础绘图
plot(st_geometry(county_data), axes = TRUE)
plot(st_geometry(county_data[cluster_indices, ]), add = TRUE, col = "red")
title("Most Likely Cluster")

# 泊松分布分析
poisson <- kulldorff(
  geo_coords,
  cases,
  population,
  expected.cases,
  pop.upper.bound,
  n.simulations,
  alpha.level,
  plot
)

# 可视化泊松分析结果
cluster_indices_poisson <- poisson$most.likely.cluster$location.IDs.included

county_data %>%
  mutate(
    cluster = row_number() %in% cluster_indices_poisson,
    fill_color = if_else(cluster, "red", "white")
  ) %>%
  ggplot() +
  geom_sf(aes(fill = fill_color), color = "black") +
  scale_fill_identity() +
  labs(title = "Most Likely Cluster Controlling for Strata") +
  theme_minimal()

# 或者使用基础绘图
plot(st_geometry(county_data), axes = TRUE)
plot(
  st_geometry(county_data[cluster_indices_poisson, ]),
  add = TRUE,
  col = "red"
)
title("Most Likely Cluster Controlling for Strata")

# Besag-Newell 分析
k <- 1250
alpha.level <- 0.05

# 不控制分层
results_no_strata <- besag_newell(
  geo_coords,
  population,
  cases,
  expected.cases = NULL,
  k,
  alpha.level
)

# 控制分层
results_with_strata <- besag_newell(
  geo_coords,
  population,
  cases,
  expected.cases,
  k,
  alpha.level
)


# 基于实际数据结构的可视化函数
visualize_besag_newell_clusters <- function(
  results,
  county_sf,
  alpha = 0.05,
  title = "Besag-Newell Clusters"
) {
  if (is.null(results$clusters) || length(results$clusters) == 0) {
    # 没有聚类时的可视化
    p <- county_sf %>%
      ggplot() +
      geom_sf(fill = "lightgray", color = "black") +
      labs(title = paste(title, "- No Clusters Found")) +
      theme_minimal()
    return(p)
  }

  # 提取显著聚类 (p < alpha)
  significant_clusters <- results$clusters[
    sapply(results$clusters, function(x) x$p.value < alpha)
  ]

  if (length(significant_clusters) == 0) {
    # 没有显著聚类
    p <- county_sf %>%
      ggplot() +
      geom_sf(fill = "lightgray", color = "black") +
      labs(title = paste(title, "- No Significant Clusters (α =", alpha, ")")) +
      theme_minimal()
    return(p)
  }

  # 创建聚类标识
  county_sf_with_clusters <- county_sf %>%
    mutate(
      location_id = row_number(),
      cluster_id = 0, # 0 表示不在任何聚类中
      p_value = NA_real_,
      SMR = NA_real_
    )

  # 为每个显著聚类分配ID
  for (i in seq_along(significant_clusters)) {
    cluster_locations <- significant_clusters[[i]]$location.IDs.included
    county_sf_with_clusters <- county_sf_with_clusters %>%
      mutate(
        cluster_id = ifelse(location_id %in% cluster_locations, i, cluster_id),
        p_value = ifelse(
          location_id %in% cluster_locations,
          significant_clusters[[i]]$p.value,
          p_value
        ),
        SMR = ifelse(
          location_id %in% cluster_locations,
          significant_clusters[[i]]$SMR,
          SMR
        )
      )
  }

  # 创建颜色映射
  n_clusters <- length(significant_clusters)
  cluster_colors <- c("lightgray", rainbow(n_clusters, alpha = 0.7))

  # 可视化
  p <- county_sf_with_clusters %>%
    mutate(
      cluster_factor = factor(
        cluster_id,
        levels = 0:n_clusters,
        labels = c("No cluster", paste("Cluster", 1:n_clusters))
      ),
      fill_color = cluster_colors[cluster_id + 1]
    ) %>%
    ggplot() +
    geom_sf(aes(fill = fill_color), color = "black", size = 0.3) +
    scale_fill_identity() +
    labs(
      title = title,
      subtitle = paste(
        "Significant clusters (p <",
        alpha,
        "):",
        length(significant_clusters)
      )
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 8),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10)
    )

  return(list(
    plot = p,
    data = county_sf_with_clusters,
    clusters = significant_clusters
  ))
}

# 创建详细的聚类信息表
create_cluster_summary <- function(results, alpha = 0.05) {
  if (is.null(results$clusters) || length(results$clusters) == 0) {
    return(tibble(message = "No clusters found"))
  }

  # 提取显著聚类信息
  significant_clusters <- results$clusters[
    sapply(results$clusters, function(x) x$p.value < alpha)
  ]

  if (length(significant_clusters) == 0) {
    return(tibble(message = paste("No significant clusters (α =", alpha, ")")))
  }

  # 创建摘要表
  cluster_summary <- map_dfr(seq_along(significant_clusters), function(i) {
    cluster <- significant_clusters[[i]]
    tibble(
      cluster_id = i,
      n_counties = length(cluster$location.IDs.included),
      counties = paste(cluster$location.IDs.included, collapse = ", "),
      population = cluster$population,
      observed_cases = cluster$number.of.cases,
      expected_cases = round(cluster$expected.cases, 1),
      SMR = round(cluster$SMR, 3),
      p_value = format(cluster$p.value, scientific = TRUE, digits = 3)
    )
  })

  return(cluster_summary)
}

# 应用到两个分析结果
result_no_strata <- visualize_besag_newell_clusters(
  results_no_strata,
  county_data,
  alpha = 0.05,
  title = "Besag-Newell Clusters (No Strata Control)"
)

result_with_strata <- visualize_besag_newell_clusters(
  results_with_strata,
  county_data,
  alpha = 0.05,
  title = "Besag-Newell Clusters (With Strata Control)"
)

# 显示图形
print(result_no_strata$plot)
print(result_with_strata$plot)

# 创建聚类摘要表
summary_no_strata <- create_cluster_summary(results_no_strata, alpha = 0.05)
summary_with_strata <- create_cluster_summary(results_with_strata, alpha = 0.05)

print("不控制分层的显著聚类:")
print(summary_no_strata)

print("控制分层的显著聚类:")
print(summary_with_strata)

# 并排比较图
library(patchwork)
if (require(patchwork, quietly = TRUE)) {
  combined_plot <- result_no_strata$plot + result_with_strata$plot
  print(combined_plot)
}


```
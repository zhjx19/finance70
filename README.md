# finance70

**《Tidyverse 进阶修炼：金融数据操作 70 题（2026 版）》**——任坤的 R 数据操作练习，张敬信。

用 `tidyverse` 与 `data.table` 两套方案解决同一批金融数据操作问题，覆盖筛选、分组汇总、窗口计算、连接与整形等常见场景。

## 目录结构

```
├── Renkun-R-Data-Practise.qmd        # 主文档（Quarto 源文件）
├── Renkun-R-Data-Practise.pdf        # 渲染好的 PDF
├── data.table-solutions.R            # data.table 版 70 题解答
├── data/stock-market-data.rds        # 股票市场日行情数据
├── images/                           # 文档插图
└── Renkun.Rproj                      # RStudio 项目文件
```

## 使用方法

```r
# 安装依赖
install.packages(c("tidyverse", "data.table", "slider", "stringr"))

# data.table 解法
source("data.table-solutions.R")

# 渲染主文档（需 Quarto + lualatex）
quarto render Renkun-R-Data-Practise.qmd
```

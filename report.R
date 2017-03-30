library(magrittr)
library(dplyr)
library(ggplot2)

# 讀取資料
power <- readLines(file("power.txt", encoding = "BIG-5"))
power.split <- strsplit(power, split = ";\t", fixed = TRUE)
power.mat <- do.call(rbind, power.split)
power.df <- data.frame(power.mat, stringsAsFactors = FALSE)
colnames(power.df) <- c("id", "name", "year", "power")

# 整理型態
power.df$name <- factor(power.df$name)
power.df$year <- as.integer(power.df$year)
power.df$power <- as.numeric(power.df$power)

gdp <- file("NA8103A1Ac.csv", encoding = "BIG-5") %>% readLines()
gdp.split <- strsplit(gdp, ",")
year.index <- (sapply(gdp.split, length) == 1) %>% which %>% head(7)
# 開始的index
year.index.start <- year.index
# 結束的index
## 每一個開始的index接續到下一個開始的index
## 也就是結尾的index就是下一個開始的index
year.index.end <- c(tail(year.index, -1), length(gdp.split))
# 開始撰寫for迴圈
## 我們先建立每一年份整理出來的gdp 資料
## 這些資料會被放到gdp.df.components之中
gdp.df.components <- list()
# 請填寫正確的for 迴圈的範圍
for(i in 1:7) {
  # 針對特定年份做處理的程式碼
  ## 開始的index
  start <- year.index.start[i]
  ## 結束的index
  end <- year.index.end[i]
  ## 年份的資料在第一筆
  year <- gdp.split[[start]] %>%
    ## 將 "2007" ==> 2007，將"用空白替換掉
    gsub(pattern = '"', replacement = '')
  ## 只抽出這次要處理的資料
  target <- gdp.split[start:end]
  ## 挑出長度是3 的，做rbind
  target.mat <- do.call(rbind, 
                        target[sapply(target, length) == 3])
  ## 原本的第一行是空白，我們改成放年份
  target.mat[,1] <- year
  ## 處理第二行中的"
  target.mat[,2] <- gsub('"', '', target.mat[,2])
  ## 將這輪處理的資料，放到gdp.df.components
  gdp.df.components[[i]] <- target.mat
}
gdp.df <- do.call(rbind, gdp.df.components) %>% 
  ## 我們先不把資料轉成factor
  data.frame(stringsAsFactors = FALSE)
colnames(gdp.df) <- c("year", "name", "gdp")
## 把year 轉成integer
gdp.df$year <- as.integer(gdp.df$year)
## 把name 轉成character
gdp.df$name <- as.character(gdp.df$name)
## 把gdp 轉成numeric
gdp.df$gdp <- as.numeric(gdp.df$gdp)
gdp.df <- filter(gdp.df, !is.na(gdp))

gdp.df %<>% mutate(id = strsplit(name, ".", fixed = TRUE) %>% sapply("[", 1))
gdp.df2 <- filter(gdp.df, nchar(id) == 1)
power.df2 <- 
  filter(power.df, grepl("^[A-Z]", id)) %>%
  mutate(year = year + 1911, id = gsub(".", "", id, fixed = TRUE))

translation.power <- local({
  retval <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10)
  names(retval) <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
  retval
})

translation.gdp <- local({
  translation.gdp <- c(1, 2, 3, 4, 4, 5, 6, 7, 6, 7, 8, 8, 9, 9, 10, 9, 9, 9, 9)
  names(translation.gdp) <- head(LETTERS, length(translation.gdp))
  translation.gdp
})

power.df3 <-
  power.df2 %>%
  mutate(id2 = translation.power[id]) %>%
  filter(!is.na(id2)) %>%
  group_by(year, id2) %>%
  summarise(power = sum(power), name = paste(name, collapse = ","))

gdp.df3 <- 
  gdp.df2 %>%
  mutate(id2 = translation.gdp[id]) %>%
  group_by(year, id2) %>%
  summarise(gdp = sum(gdp))

power.gdp <- inner_join(power.df3, gdp.df3, c("year", "id2")) %>%
  mutate(eff = gdp / power) %>%
  as.data.frame()

library(ggplot2)
ggplot(power.gdp, aes(x = name, y = eff)) +
  geom_bar(stat = "identity") +
  theme_grey(base_family="STKaiti")

sessionInfo()

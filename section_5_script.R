pacman::p_load(tidyverse, readr, readxl)
library(estimatr)
library(modelsummary)

# データの取得
data514 <- read_csv("youdou.csv")
glimpse(data514)

# 経済成長率及び対数GDPを作成
data514 <- data514 |> 
  mutate(
    ecodev = 100 * (log(data514$y99) - log(data514$y80)) / 19,
    lny80 = log(y80),
    lny90 = log(y90),
    lny99 = log(y99)
    )

# 表5-5
# datasummary内の変数は全てmodelsummaryの関数（N,Mean,SDなど）
vars <- data514 |> 
  select(ecodev, trust80, norm80, education80, lny80)
 
table5.5 <- modelsummary::datasummary(
  All(vars) ~ N + Mean + SD + Min + Max,
  data = data514,
  output = "data.frame",
  fmt = 3)

table5.5[, 1] <- c("経済成長率", "信頼", "規範", "教育水準",
                        "初期時点対数GDP")

colnames(table5.5) <- c("変数", "サンプルサイズ", "平均",
                        "標準誤差", "最小値", "最大値")

gt::gt(table5.5)



# 実証5-14
# 5-14-a
#各モデルのOLS推定（不均一分散に頑強な標準誤差を使用）
model1 <- lm_robust(ecodev ~ trust80, data = data514)
model2 <- lm_robust(ecodev ~ norm80, data = data514)
model3 <- lm_robust(ecodev ~ trust80 + norm80, data = data514)
model4 <- lm_robust(ecodev ~ trust80 + log(y80) + education80,
                    data = data514)
model5 <- lm_robust(ecodev ~ norm80 + log(y80) + education80,
                    data = data514)
model6 <- lm_robust(ecodev ~ trust80 + norm80 + log(y80) + education80,
                    data = data514)

models5_14 <- list(
  "(1)" = model1,
  "(2)" = model2,
  "(3)" = model3,
  "(4)" = model4,
  "(5)" = model5,
  "(6)" = model6
)

attr(models5_14$`(3)`, "FTEST") <- TRUE
attr(models5_14$`(6)`, "FTEST") <- TRUE

glance_custom.lm_robust <- function(x) {
  if(!isTRUE(attr(x, "FTEST"))) return(NULL)
  
  ftest <- car::linearHypothesis(x, test = "F", c("trust80", "norm80"))
  
  out <- tibble(
    F_value = ftest[["F"]][2],
    F_p = sprintf("(%.3f)", ftest[["Pr(>F)"]][2])
  )
  return(out)
}

cm <- c("trust80" = "信頼",
        "norm80" = "規範",
        "lny80" = "初期時点対数GDP",
        "education80" = "教育水準",
        "(Intercept)" = "定数項")

gm <- tribble(
  ~raw, ~clean, ~fmt,
  "F_value", "F検定量の値 $H_0:\\beta_{信頼}=0, \\beta_{規範}=0$", 3,
  "F_p", "   ", 3,
  "adj.r.squared", "$\\bar{R}^2$", 3,
  "nobs", "サンプルサイズ", 0
)

modelsummary::msummary(
  models5_14,
  coef_map = cm,
  gof_omit = "R2$|RMSE|AIC|BIC|Log.Lik.",
  gof_map = gm,
  output = "kableExtra",
  estimate = "{estimate}{stars}",
  notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001,"
) |> 
  row_spec(c(0, 10, 12), extra_css = "border-bottom: 1.5px solid")




# 5-14-c
# 1段階目は非ロバスト、2段階目はロバスト標準誤差を使用
modelc_11 <- lm(trust80 ~ norm80, data = data514)
modelc_12 <- lm_robust(data514$ecodev ~ modelc_11$residuals)
summary(modelc_1)

modelc_21 <- lm(ecodev ~ norm80, data = data514)
modelc_22 <- lm_robust(modelc_21$residuals ~ modelc_11$residuals)
summary(modelc_22)



# 5-15
data515 <- haven::read_dta("timss.dta")
glimpse(data515)

# 5-15-a
modela <- lm_robust(mathscore ~ agese_q2 + agese_q3 + agese_q4,
                    data = data515)
summary(modela)

# 5-15-b
# 4-6月生まれのダミー変数をagese_q1と設定し、OLS推定
data515 <- data515 |> 
  mutate(
    agese_q1 = if_else(agese_q2 + agese_q3 + agese_q4 == 1, 0, 1)) |> 
  relocate(agese_q1, .before = agese_q2)

modelb <- lm_robust(mathscore ~ agese_q1 + agese_q2 + agese_q3,
                    data = data515)
summary(modelb)


# 5-15-c
# 交差項の信頼区間がいずれも0を含んでおり、男女による差があるとは言えない
modelc <- lm_robust(mathscore ~ 
                      agese_q1 * gender + 
                      agese_q2 * gender + 
                      agese_q3 * gender,
                    data = data515)
summary(modelc)


# 5-15-d
# 共変量を加えたモデルでも、有意に低い
modeld <- lm_robust(mathscore ~ agese_q2 + agese_q3 + agese_q4 +
                      comu_1 + comu_2 + comu_3 + comu_4 + comu_5 +
                      computer + numpeople +
                      mothereduc_1 + mothereduc_2 + mothereduc_3 + 
                      mothereduc_4 + mothereduc_5 + 
                      fathereduc_1 + fathereduc_2 + fathereduc_3 + 
                      fathereduc_4 + fathereduc_5,
                    data = na.omit(data515))

summary(modeld)


# 5-15-e
# 理科の成績を用いた分析でも、結論は変わらず
modele_a <- lm_robust(sciencescore ~ agese_q2 + agese_q3 + agese_q4,
                    data = data515)
summary(modele_a)


modele_b <- lm_robust(sciencescore ~ agese_q1 + agese_q2 + agese_q3,
                    data = data515)
summary(modele_b)


modele_c <- lm_robust(mathscore ~ 
                      agese_q1 * gender + 
                      agese_q2 * gender + 
                      agese_q3 * gender,
                    data = data515)
summary(modele_c)


modele_d <- lm_robust(sciencescore ~ agese_q2 + agese_q3 + agese_q4 +
                      comu_1 + comu_2 + comu_3 + comu_4 + comu_5 +
                      computer + numpeople +
                      mothereduc_1 + mothereduc_2 + mothereduc_3 + 
                      mothereduc_4 + mothereduc_5 + 
                      fathereduc_1 + fathereduc_2 + fathereduc_3 + 
                      fathereduc_4 + fathereduc_5,
                    data = na.omit(data515))

summary(modele_d)



# 分析結果を表にまとめる（おまけ）
# 参照：https://sishii0418.github.io/nishiyama_econometrics/ch5.html#%E7%B7%B4%E7%BF%92%E5%95%8F%E9%A1%8C-5-15-%E5%AE%9F%E8%A8%BC

pacman::p_load(modelsummary, kableExtra)

models_5_15 <- list()
models_5_15[['a']] <- modela
models_5_15[['c']] <- modelc
models_5_15[['d']] <- modeld
models_5_15[['ea']] <- modele_a
models_5_15[['ec']] <- modele_c
models_5_15[['ed']] <- modele_d

cm <- c("agese_q2",
        "agese_q3",
        "agese_q4",
        "gender",
        "gender:agese_q2",
        "gender:agese_q3",
        "gender:agese_q4",
        "(Intercept)")
  
gm <- tribble(
  ~raw,            ~clean,           ~fmt,
  "adj.r.squared", "$\\bar{R}^2$",   2,
  "nobs",          "サンプルサイズ", 0)  

modelsummary::msummary(
         models_5_15,
         coef_map = cm,
         gof_map = gm,
         stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
         estimate = "{estimate}{stars}",
         output = "kableExtra",
         notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001") |> 
  kableExtra::row_spec(0, extra_css = "border-bottom: 1.5px solid") |>   kableExtra::row_spec(16, extra_css = "border-bottom: 1.5px solid")



  
  
  
  
  
  
  
  
  
  
  





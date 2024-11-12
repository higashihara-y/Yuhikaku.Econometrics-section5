pacman::p_load(tidyverse, readr, readxl)
library(estimatr)

# 5-14
data514 <- read_csv("youdou.csv")
glimpse(data514)

# 経済成長率を算出
data514 <- data514 |> 
  mutate(
    ecodev = 100 * (log(data514$y99) - log(data514$y80)) / 19)


# 5-14-a
#各モデルのOLS推定（不均一分散に頑強な標準誤差を使用）
model1 <- lm_robust(ecodev ~ trust80, data = data514)
summary(model1)

model2 <- lm_robust(ecodev ~ norm80, data = data514)
summary(model2)

model3 <- lm_robust(ecodev ~ trust80 + norm80, data = data514)
summary(model3)

model4 <- lm_robust(ecodev ~ trust80 + log(y80) + education80,
                    data = data514)
summary(model4)

model5 <- lm_robust(ecodev ~ norm80 + log(y80) + education80,
                    data = data514)
summary(model5)

model6 <- lm_robust(ecodev ~ trust80 + norm80 + log(y80) + education80,
                    data = data514)
summary(model6)


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



  
  
  
  
  
  
  
  
  
  
  





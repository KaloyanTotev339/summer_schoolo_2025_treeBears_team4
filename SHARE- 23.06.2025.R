library(readr)
library(readxl)
library(dplyr)

df1 <- read_delim("C:/Users/Sergey Filipov/Desktop/Лятно училище/Кейс SHARE/statisticsexp.csv",
                  delim = ";", 
                  locale = locale(encoding = "UTF-8"),
                  show_col_types = FALSE)

df2 <- read_excel("C:/Users/Sergey Filipov/Desktop/Лятно училище/Кейс SHARE/Coding tables.xlsx")

#  Създай копие за чистене
df1_clean <- df1

# Чистене (замени отрицателните стойности с NA)
df1_clean[df1_clean < 0] <- NA

# Списък с ключови колони
key_cols <- c("mergeid", "hhid8", "mergeidp8", "coupleid8", "country", "language")

# Брой колони преди чистене
before_ncol <- ncol(df1_clean)

# Открий колоните с >30% NA (без ключовите)
drop_cols <- setdiff(names(df1_clean), key_cols)[colMeans(is.na(df1_clean[setdiff(names(df1_clean), key_cols)])) > 0.3]

# Изтрий ги (ключовите не се пипат)
df1_clean <- df1_clean[, !(names(df1_clean) %in% drop_cols)]


# С КАКВО РАЗПОЛАГАМЕ СЛЕД ЧИСТЕНЕТО????
# Първи 6 реда
print(head(df1_clean))

# Структура (имена, типове, примерни стойности)
str(df1_clean)

# Имена на колоните
print(colnames(df1_clean))

# Размерност (редове, колони)
print(dim(df1_clean))      # (редове, колони)
print(nrow(df1_clean))     # брой редове
print(ncol(df1_clean))     # брой колони

# Обобщена статистика за числовите и факторни колони
print(summary(df1_clean))

# ------------------------------------------------------------------------------
# df1_clean съдържа данни за следните основни модули:
#
# Accommodation & household:     ac*
# Health:                        br*, adl, iadl, bmi, chronic*, euro*, phactiv, maxgrip, sphus, casp, loneliness
# Children:                      ch*
# Demographics:                  dn*, country, language, gender, age2020, yrbirth, mobirth
# Employment:                    ep*, te*
# Expectations:                  ex*
# Cognitive:                     cf*
# Social:                        sn*, famnet, childnet, siblingnet, friendnet, formalnet, othernet, womennet, mennet, prx*, contact*
# Health care:                   hc*
# Money/Financial:               mn*, fam_resp, fin_resp, hou_resp
# Education:                     isced*
# Administrative:                mergeid, hhid8, waveid, panel_status, и други
# ------------------------------------------------------------------------------

# save.image(file = "C:/Users/Sergey Filipov/Desktop/Лятно училище/Кейс SHARE/my_environment.RData")


# --------- 1. Определи целевите променливи -----------
target_health <- "sphus"
target_age <- "age2020"

# --------- 2. Избери "leaky" features за всеки модел ------------

# За модел за Self-Perceived Health:
leaky_health <- c(
  "adl", "adl2", "iadl", "iadl2", "chronic2w8", "chronicw8c", "gali",
  "bmi", "bmi2", "phactiv", "maxgrip", "mobility", "mobilit2", "mobilit3",
  grep("^euro", colnames(df1_clean), value = TRUE),  # всички euro*
  target_health # не ползваме target като предиктор!
)

# За модел за Age (longevity):
leaky_age <- c(
  "yrbirth", "mobirth", "age2020", "age_int", "year_mean", "year_1", 
  target_age # не ползваме target като предиктор!
)

# --------- 3. Премахни "leaky" feature-и за всеки модел ----------

# Всички налични колони
all_cols <- colnames(df1_clean)

# Колони за модел за здраве (без leaky за health)
keep_health <- setdiff(all_cols, leaky_health)
df_health <- df1_clean[, keep_health]

# Колони за модел за възраст (без leaky за age)
keep_age <- setdiff(all_cols, leaky_age)
df_age <- df1_clean[, keep_age]

# --------- 4. Проверка -----------
cat("Брой колони за health модел:", ncol(df_health), "\n")
cat("Брой колони за age модел:", ncol(df_age), "\n")




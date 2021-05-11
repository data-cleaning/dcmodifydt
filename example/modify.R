library(data.table)

m <- modifier( if (age > 130) age = 130
             , income[age < 12] <- 0
             )

dat <- fread(text =
"age, income
 140,  300
  11, 2000
  25, 3000"
)

# modify a copy of the data
dat_m <- modify(dat, m, copy = TRUE)
print(dat_m)

# the data it self
setmodify(dat, m)
print(dat)

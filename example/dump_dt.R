library(dcmodify)

m <- modifier( if (age > 130) age = 130
               , income[age < 12] <- 0
)

dump_dt(m, "my_data")

library(data.table)
library(dcmodify)

m <- modifier( if (age > 130) age = 130
             , income[age < 12] <- 0
             )

income <- fread( text =
"age, income
 140,  300
  11, 2000
  25, 3000"
)


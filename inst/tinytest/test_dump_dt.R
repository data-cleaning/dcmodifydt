library(data.table)
library(dcmodifydt)

m <- modifier( if (age < 12) income <- 0
               , age[age > 130] <- 130
)


f <- tempfile("dcmodify")
dump_dt(m, "mydt", f)
d <- readLines(f)
d
expect_equal_to_reference(d, "dump.rds")

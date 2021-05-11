library(data.table)
library(dcmodifydt)

d <- fread(text=
"age, income
  10,   2000
  -1,      1
  150,  3000"
)

m <- modifier( if (age < 12) income <- 0
             , age[age > 130] <- 130
             )

# works on a copy
d2 <- modify(d, m, copy=TRUE)
expect_equal(d2$age, c(10,-1,130))
expect_equal(d2$income, c(0,0,3000))


# works on a copy
expect_warning({
  d2 <- modify(d, m)
})

expect_equal(d2$age, c(10,-1,130))
expect_equal(d2$income, c(0,0,3000))


# works on the object itself
modify(d, m, copy = FALSE)

expect_equal(d$age, c(10,-1,130))
expect_equal(d$income, c(0,0,3000))

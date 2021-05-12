library(dcmodifydt)

dt_assign <- dcmodifydt:::dt_assign

m <- modifier( if (age < 12) income <- 0
             , age[age > 130] <- 130
             , is.na(age) <- age < 0
             )

asgns <- m$assignments()

dt_asgns <- lapply(asgns, dt_assign)

expect_equal( dt_asgns[[1]]
            , quote(dat[age < 12, income := 0])
            )

expect_equal( dt_asgns[[2]]
            , quote(dat[age > 130, age := 130])
            )

expect_equal( dt_asgns[[3]]
            , quote(dat[age < 0, age := NA])
            )

dt_asgns_char <- lapply(dt_asgns, dcmodifydt:::dt_assign_char)

expect_equal( dt_asgns_char[[1]]
            , "dat[age < 12, income := 0]"
            )

expect_equal( dt_asgns_char[[2]]
            , "dat[age > 130, age := 130]"
            )

expect_equal( dt_asgns_char[[3]]
            , "dat[age < 0, age := NA]"
            )

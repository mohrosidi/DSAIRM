context("test-generate_ggplot.R")

test_that("generate_ggplot returns a ggplot",
          {
            simresult=DSAIRM::simulate_basicvirus_ode()
            expect_is( DSAIRM::generate_ggplot(list(list(simres = simresult, xlab = "Time"))), "ggplot"  )
          })


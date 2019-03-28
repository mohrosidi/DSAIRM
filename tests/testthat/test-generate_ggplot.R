context("test-generate_ggplot.R")

test_that("generate_ggplot returns a ggplot",
          {
            #running simulation directly
            simresult=DSAIRM::simulate_basicvirus_ode()
            result1 = vector("list",1)
            result1[[1]]$simres = simresult
            result1[[1]]$plottype = "Lineplot"
            expect_is( DSAIRM::generate_ggplot(result1), "ggplot"  )


            #running simulation through run_model
            tfinal = 120
            modelsettings =  list(U = 1e5,I=0,V=10, n = 0, dU = 0,dI = 1, dV = 2, b = 2e-05, p = 5, g = 1, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_basicvirus_ode'
            result = run_model(modelsettings)
            expect_is( DSAIRM::generate_ggplot(result), "ggplot"  )



          })


context("test-generate_plotly.R")

test_that("generate_plotly returns a plotly plot",
          {

            #running simulation directly
            simresult=DSAIRM::simulate_basicvirus_stochastic()
            result1 = vector("list",1)
            result1[[1]]$simres = simresult
            result1[[1]]$plottype = "Lineplot"
            expect_is( DSAIRM::generate_plotly(result1), "plotly" )


            #running simulation through run_model
            tfinal = 120
            modelsettings =  list(U = 1e3, I=0, V=10, n = 0, dU = 0, b = 2e-03, dI = 1, p = 5, dV = 2, rngseed = 100,  tfinal = tfinal, modeltype = "_stochastic_", nplots = 1, nreps = 5)
            modelsettings$simfunction = 'simulate_basicvirus_stochastic'
            result = run_model(modelsettings)
            expect_is( DSAIRM::generate_plotly(result), "plotly" )

          })


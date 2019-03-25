context("test-fit-apps.R")


test_that("fit apps all run correctly",
{

            #test basic fit
            modelsettings =  list(U = 10000, I = 0, V = 10, X =1, n = 0, dU = 0, dI = 1, g = 1, p = 10, plow = 0.001, phigh = 1000, psim = 10, b = 1e-04, blow = 1e-6, bhigh = 1e-3, bsim = 1e-4, dV = 2, dVlow = 0.001, dVhigh = 1e3, dVsim = 10)
            modelsettings$usesimdata = FALSE
            modelsettings$noise = 0.01
            modelsettings$iter = 10
            modelsettings$solvertype = 1
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'none'
            modelsettings$simfunction = 'simulate_basicmodel_fit'
            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$simres$data$outcome,1)
            testthat::expect_equal(finaldatapoint, 0)

            test=c(result,result)
            expect_is( DSAIRM::generate_ggplot(test), "ggplot"  )


            #test confint fit
            modelsettings =  list(U = 10000, I = 0, V = 10, blow = 1e-6, bhigh = 1000)
            modelsettings$iter = 10
            modelsettings$nsample = 10
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_confint_fit'
            result = run_model(modelsettings)
            finalsimI = round(tail(result[[1]]$simres$ts$I,1))
            testthat::expect_equal(finalsimI, 160)

            #test model comparison fit
            modelsettings =  list(U = 10000, I = 0, V = 10, X = 1)
            modelsettings$iter = 10
            modelsettings$fitmodel = 2
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_modelcomparison_fit'
            result = run_model(modelsettings)
            finalssr = round(result[[1]]$simres$SSR)
            testthat::expect_equal(finalssr, 12)

})

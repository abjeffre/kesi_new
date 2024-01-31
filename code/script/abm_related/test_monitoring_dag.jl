###############
####### 


    nrounds = 3600
    climate_var = .1
    c1 = cos.(collect(1:nrounds)/(15*pi))   + rand(Normal(0,climate_var), nrounds)
    c2 = -cos.(collect(1:nrounds)/(14*pi))   + rand(Normal(0,climate_var), nrounds)
    c3 = sin.(collect(1:nrounds)/(16*pi))   + rand(Normal(0,climate_var), nrounds)
    c4 = -sin.(collect(1:nrounds)/(12*pi))   + rand(Normal(0,climate_var), nrounds)
    g = c1 .* .7 .+ c2 .* -.3 .+ c3 .* -.2 .+ c4 .* .1 
    g = g.+3


    no_adapation=cpr_abm(tech =.000002,
    leak = false,
    experiment_limit = .5,
    regrow = 0.007,
    n = 300,
    experiment_punish2 = .9,
    outgroup = 0.001,
    wage_data = g,
    price = 3.5, 
    nrounds = nrounds,
    max_forest = 20000,
    set_stock = .5,
    nsim = 1,
    nmodels=5,
    labor = 1,
    fidelity = .3,
    labor_market = false,
    genetic_evolution = false,
    #seizure_pun2_correlation = .1
    )


    adapation=cpr_abm(tech =.000002,
    leak = false,
    experiment_limit = .5,
    regrow = 0.007,
    n = 300,
    experiment_punish2 = .1,
    outgroup = 0.001,
    wage_data = g,
    price = 3.5, 
    nrounds = nrounds,
    max_forest = 20000,
    set_stock = .5,
    nsim = 1,
    nmodels=5,
    labor = 1,
    fidelity = .3,
    labor_market = false,
    genetic_evolution = false,
    seizure_pun2_correlation = .5)


    #################################
    ############# NO adapation ######

    dat1=DataFrame(
        g = g[100:end],
        caught = Int64.(round.(no_adapation[:caught2][100:end,1,1].*150)))
    formula=@formula(caught ~ g)
    glm(formula, dat1, Poisson())


    #################################
    ############# Adapation #########

    dat2=DataFrame(
        g = g[100:end],
        caught = Int64.(round.(adapation[:caught2][100:end,1,1].*150)),
        monitoring = adapation[:punish2][100:end,1,1].*150)
    formula=@formula(caught ~ g)
    formula2=@formula(caught ~ g + monitoring)
    glm(formula, dat2, Poisson())
    glm(formula2, dat2, Poisson())


plot(adapation[:caught2][:,1,1])
plot!(adapation[:punish2][:,1,1], )

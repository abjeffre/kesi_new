function standardize(x) 
        μ = mean(x)
        σ = std(x)
        return((x .- μ)/σ)
 end
nrounds = 360*12
capture_rate = .05
price = 3.2
mah = .5
welast = 0
labor = rand(Uniform(.99, 1), 300)
freq = 5
climate_var =1

inspect = collect(.01 : .05 : .65)
price = collect(2.5:.2:5)

S=expand_grid(price, inspect)
nsim = 25

for i in 1:size(S)[1]
        pri = fill(S[i, 1], nsim)
        ins = fill(S[i, 2], nsim)
        c1list = []
        c2list = []
        c3list = []
        c4list = []
        glist = []
        for j in 1:nsim
               c1 = cos.(collect(1:nrounds)/(15*pi))   + rand(Normal(0,climate_var), nrounds)
               c2 = -cos.(collect(1:nrounds)/(14*pi))   + rand(Normal(0,climate_var), nrounds)
               c3 = sin.(collect(1:nrounds)/(16*pi))   + rand(Normal(0,climate_var), nrounds)
               c4 = -sin.(collect(1:nrounds)/(12*pi))   + rand(Normal(0,climate_var), nrounds)
               g = c1 .* .7 .+ c2 .* -.3 .+ c3 .* -.2 .+ c4 .* .1 
               g = g.+3
               push!(glist, g)
               push!(c1list, c1)
               push!(c2list, c2)
               push!(c3list, c3)
               push!(c4list, c4)
        end
        
        @everywhere function multi_core(pr, in, glist)
                cpr_abm(tech =.000002,
                leak = false,
                experiment_limit = .5,
                regrow = 0.007,
                n = 300,
                experiment_punish2 = in,
                outgroup = 0.001,
                wage_data = glist,
                price = pr, 
                nrounds = 360*12,
                max_forest = 20000,
                set_stock = .5,
                nsim = 1,
                nmodels=5,
                labor = 1,
                fidelity = .3,
                labor_market = false,
                genetic_evolution = false)
        end
        out=pmap(multi_core, pri, ins, glist)
        
        for j in 1:nsim
                dat=DataFrame(
                  g = glist[j],
                  c1 = c1list[j],
                  c2 = c1list[j],
                  c3 = c1list[j],
                  c4 = c1list[j],
                  caught = Int64.(round.(out[j][:caught2][:,1,1].*150)))
                  pr = pri[1]
                  in = ins[1]
                  CSV.write(string("kesi/data/kesi_sweep/price$pr", "inspect$in","sim$j",".csv"), dat)
         end
 
end

CSV.write(string("kesi/data/kesi_sweep/kesi_sweep_list_base.csv"), DataFrame(Float64.(S[:,1:2]), :auto))

@everywhere using CSV
































# for i in 1:100


#         a = cpr_abm(tech =.000002,
#         leak = false,
#         experiment_limit = mah,
#         regrow = 0.007,
#         n = 300,
#         experiment_punish2 = capture_rate,
#         outgroup = 0.001,
#         wage_data = g,
#         price = price, 
#         nrounds = nrounds,
#         max_forest = 20000,
#         set_stock = .5,
#         nsim = 1,
#         nmodels=5,
#         labor = 1,
#         fidelity = .3,
#         labor_market = false,
#         genetic_evolution = false)

#       #   plot(a[:harvest][:,1,1])
#       caught = Int64.(round.(a[:caught2][50:end,1,1].*300, digits=0))
#       dat=DataFrame(caught = caught,
#               wage = g[50:end],
#               climate = c4[50:end])
#       plot(caught)

#       # Weather and Wages
#       fm = @formula(caught ~ wage)
#       lm_bau = glm(fm, dat, Poisson())
#       pvals =coeftable(lm_bau).cols[4][2]
#       push!(pbauw, pvals[1])
#       push!(coefbauw, coef(lm_bau)[2])


 
# end
        

# #         b = cpr_abm(tech =.000002,
# #         leak = false,
# #         experiment_limit = mah,
# #         regrow = 0.007,
# #         n = 150,
# #         experiment_punish2 = capture_rate,
# #         outgroup = 0.001,
# #         wage_data = g2,
# #         price = price, 
# #         nrounds = nrounds,
# #         max_forest = 20000,
# #         set_stock = .5,
# #         nsim = 1,
# #         nmodels=5,
# #         labor = 1,
# #         fidelity = .2,
# #         labor_market = false,
# #         genetic_evolution = false)

# #        #  plot(b[:harvest][:,1,1])
# #         # plot!(g2)



#         ##################################
#         ############ BAU #################

#         # ######################################
#         # ############ HIGH VAR #################

#         # caught = Int64.(round.(b[:caught2][50:end,1,1].*300, digits=0))
#         # dat=DataFrame(caught = caught,
#         #         wage = g2[50:end],
#         #         climate = c2[50:end]) 
#         # # Weather and Wages
#         # fm = @formula(caught ~ wage + climate)
#         # lm_high = glm(fm, dat, Poisson())
#         # pvals =coeftable(lm_high).cols[4][2:3]
#         # push!(phighw, pvals[1])
#         # push!(phighc, pvals[2])
#         # push!(coefhighw, coef(lm_high)[2])

# end

# scatter(coefbauw, label = "")

# bau_p_values = scatter(pbauw, label = "wages")
# hline!([.05])
# hline!([.01])
# sum(pbauw .< 0.01)



# ####################################
# ############ RECOVER WAGE ##########

# dat=DataFrame(g = g,
# c1 = c1,
# c2 = c2,
# c3 = c3,
# c4 = c4)

# fm = @formula(g ~ c1 + c2 + c3 + c4)
# lm_bau = glm(fm, dat, Normal())

# ########################################################
# ########### SEE IF WE CAN GET IT FOR ONE YEAR ##########

# dat=DataFrame(g = g[1:26],
# c1 = c1[1:26],
# c2 = c2[1:26],
# c3 = c3[1:26],
# c4 = c4[1:26])


# fm = @formula(g ~ c1 + c2 + c3 + c4)
# lm_bau = glm(fm, dat, Normal())
# β=coef(lm_bau)
# g_est = β[1].+ β[2] .*c1 .+ β[3].*c2 .+ β[4].*c3 .+ β[5] .*c4  


# dat=DataFrame(g_est = g_est[1:end],
# c1 = c1[1:end],
# c2 = c2[1:end],
# c3 = c3[1:end],
# c4 = c4[1:end],
# caught = Int64.(round.(a[:caught2][1:end,1,1].*300)))

# fm = @formula(caught ~ g_est )
# lm_bau = glm(fm, dat, Poisson())

# fm = @formula(caught ~ g_est + c1 + c2 + c3 + c4)
# lm_bau = glm(fm, dat, Poisson())



# dat=DataFrame(g = g,
# c1 = c1[1:end],
# c2 = c2[1:end],
# c3 = c3[1:end],
# c4 = c4[1:end],
# caught = Int64.(round.(a[:caught2][1:end,1,1].*300)))



# CSV.write("cpr\\data\\test.csv", dat)



# ##################################################################
# ############## INTRODUCE MONITORING COVARYING WITH WAGES #########

# # Notes: In the Pemba 



# # plot!(high, label = "#high", c = "black", ylim = (0, .5), alpha = .5)
# # plot!(bau, label = "bau", c = "#15bfca", ylim = (0, .5), alpha = .5)


# # # It appears that the marginal increase in the rate of illegal activity is sensitve to 
# # # The 

# # c = .1

# # #################
# # function overharvest(; w1=.25, b= .25, c  = .1, h1 = .1)
# #         ifelse.(c .< b.*h1 .- w1.*h1, 1 ,0)
# # end

# # plot(overharvest(w1= bau, b= 1.1))

# # plot(b[:effort][:,1,:])

# # plot(baup)
# # plot!(highp) 



# #  plot!(mean(a[:stock][:,1,:], dims = 2), label = "")
# #  plot!(mean(a[:payoffR][:,1,:], dims = 2), label = "", ylim = (0, 1))
# #  plot!(mean(a[:effort][:,1,:], dims = 2), label = "", ylim = (0, 1))
# #  plot!(mean(a[:harvest][:,1,:], dims = 2), label = "", ylim = (0, 1))



# # b = cpr_abm(tech =.000002,
# #  leak = false,
# #  experiment_limit = .2,
# #  regrow = 0.007,
# #  experiment_punish2 = .2,
# #  outgroup = 0.001,
# #  wages = .25,
# #  price = .25, 
# #  nrounds = nrounds,
# #  max_forest = 20000,
# #  #reset_stock = reset_stock,
# #  nsim = 30)

# # low=plot(mean(b[:caught2][:,1,:], dims = 2), label = "", xlab = "Time", c = "#CA2015")
# #  hline!((.25, .25), label = "", c = "black")
# #  plot!(mean(b[:stock][:,1,:], dims = 2), label = "")
 

# # plot(low, high, size = (900, 400), bottom_margin = 20px, grid = false, lw =2)
# # savefig("simulation_predictions.pdf")

# # ############################
# # #### CHECK RELATIONSHIP ####

# # # Check Coefs
# # acoefs=[]
# # for i in 1:30
# #     caught = Float64.(a[:caught2][100:end,1,i])
# #     dat=DataFrame(caught = caught,
# #             wage = wage[100:end])
# #     fm = @formula(caught ~ wage)
# #     lm1 = glm(fm, dat, Normal())
# #     push!(acoefs, coef(lm1)[2])
# #    # push!(bstd, stder(lm1)[2])
# # end
# # scatter(acoefs)



# # # Check Coefs
# # bcoefs=[]
# # for i in 1:30
# #     caught = Float64.(b[:caught2][100:end,1,i])
# #     dat=DataFrame(caught = caught,
# #             wage = wage[100:end])
# #     fm = @formula(caught ~ wage)
# #     lm1 = glm(fm, dat, Normal())
# #     push!(bcoefs, coef(lm1)[2])
# #    # push!(bstd, stder(lm1)[2])
# # end
# # scatter(bcoefs)



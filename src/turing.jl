using Turing

@model two_model() = begin
    
    # Hyper-parameters
    μ0 = [0.0, 0.0]
    σ0 = [1.0 0.0; 0.0 1.0]
   
    ii = copy(σ0)
 
    # Draw weights.
    π1 ~ Beta(1,1)
    π2 = 1-π1
    
    # Draw locations of the components.
    μ1 ~ MvNormal(μ0, σ0)
    μ2 ~ MvNormal(μ0, σ0)
    
    # Draw latent assignment.
    z ~ Categorical([π1, π2])
    
    # Draw observation from selected component.
    if z == 1
        x ~ MvNormal(μ1, ii)
    else
        x ~ MvNormal(μ2, ii)
    end
    return x 
end




iterations = 50
ϵ = 0.05
τ = 10

# Start sampling.
chain = sample(two_model(), HMC(ϵ, τ), iterations, progress=false);

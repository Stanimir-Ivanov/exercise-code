function pop = mutate(pop, bounds, factor)
% Mutates parameters in pop by a random displacement of a uniform 
% distributed values within the bounds. Note that the displacement 
% is multiplied by factor.

n_par = size(pop, 2);
n     = size(pop, 1);
delta = factor * (2*rand(size(pop)) - 1);  % Create displacement
delta = delta .* (ones(n,1) * (bounds(2,:) - bounds(1,:))); % Enlarge over range parameter
pop   = pop + delta;                       % Add displacement
pop   = min(pop, ones(n,1) * bounds(2,:)); % No displacement larger than max
pop   = max(pop, ones(n,1) * bounds(1,:)); % No displacement smaller than max

end    
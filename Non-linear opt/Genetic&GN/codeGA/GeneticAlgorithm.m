function [x_min, f_min, pop, pop_all] = GeneticAlgorithm( ...
    func, f_param, pop, n_gen, prop_comb, prop_mut, factor, bounds)
% Main function for the Genetic Algorithm
% func      : function to minimize.
% f_param   : parameters to be passed on to func.
% pop       : n by n_par matrix with population.
% n         : Number of individuals in a population.
% n_gen     : Number of generations.
% prop_comb : Proportion to combine.
% prop_mut  : Proportion to mutate.
% factor    : Multiplication factor for mutation.
% bounds    : 2 by n_par matrix with minimum and maximum values of each of 
%             the n_par parameters

n       = size(pop, 1);
n_par   = size(pop, 2);

f_x     = func(pop, f_param);             % Compute function values for all population members 
[f_x, f_ind] = sort(f_x, 'descend');      % Sort the x from worst to best.
f_min   = f_x(end);
pop     = pop(f_ind, :);                  % pop(n, :) has the x of with the lowest fx.
                                          % Reserve the last position for
                                          % the best value so far.
pop_all = zeros(n, n_par, n_gen);
pop_all(:, :, 1) = pop;

n_comb     = round(prop_comb * (n - 1) );
n_not_comb = (n - 1) - n_comb;
child      = zeros(n_comb, n_par);
pop_new    = zeros(n, n_par);
for i=1:n_gen
    n_current  = size(pop, 1);
    prob = exp(f_min - f_x);
    prob = prob/sum(prob);
    pop_new(n, :) = pop(n, :);   % Ensure that the old best is in pop_new
    
    % Select 1 - prob_comb individuals into pop_new. The probability of
    % being chosen is higher if the function value is lower.
    index_selected = randsample(1:n_current, n_not_comb, 'true', prob);
    pop_new(1:n_not_comb, :) = pop(index_selected, :);
     
    % Combine individuals in pop and add to pop_new
    ind_par1 = randsample(n_current, n_comb, 'true', prob);
    ind_par2 = randsample(n_current, n_comb, 'true', prob);
    for j=1:n_comb
       child(j, :) = combine(pop(ind_par1(j), :), pop(ind_par2(j), :));
    end
    pop_new(n_not_comb + (1:n_comb), :) = child;
     
    % Mutate selected individuals into pop_new and replace pop
    pop = pop_new;
    index_selected = randsample(1:n, floor(n * prop_mut), 'false');
    pop(index_selected, :) = mutate(pop_new(index_selected, :), bounds, factor);
    
    % Compute f_x
    f_x = func(pop, f_param);
    
    % Find the minimum fx
    [f_min, f_min_ind] = min(f_x); 
    pop(n, :) = pop(f_min_ind, :);   % Best x so far in n-th row of pop
    pop_all(:, :, i)   = pop;
end
x_min  = pop(n, :);

end




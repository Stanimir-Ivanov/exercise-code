function child = combine(parent1, parent2)
% Combines parameters in parent1 en parent2 into a new child.
% The function creates a child parameter via a convex combination of the
% parent parameters. The weight of the convex combination is drawn
% randomly from a uniform distribution on [0,1]
n_par        = size(parent1, 2);
n_parents    = size(parent1, 1);
T            = rand(n_parents, n_par);
child        = T .* parent1 + (1 - T).* parent2;
end    
function [ alpha ] = alpha_backtracking(func, grad, x, p, steplengthParam)
%ALPHA_BACKTRACKING Alpha backtracking algorithm
% func              - Function.
% grad              - Gradient.
% x                 - Current iterate (not used).
% p                 - Search direction (not used).
% steplengthParam   - Additional parameters for the algorithm, consists of alpha_bar, c and rho in that order.
alpha_bar = steplengthParam(1);
c = steplengthParam(2);
rho = steplengthParam(3);
alpha = alpha_bar;
while(func(x + alpha*p) > func(x) + c*alpha*dot(grad(x),p))
    alpha = rho*alpha;
end
end
function [ alpha ] = alpha_backtracking( func, grad, x, p, steplengthParam )
%ALPHA_BACKTRACKING Backtracking Line Search (Algorithm 3.1)
% func              - Function.
% grad              - Gradient.
% x                 - Current iterate.
% p                 - Search direction.
% steplengthParam   - Additional parameters for the algorithm, steplengthParam = [alphamax, c, rho].

alphamax = steplengthParam(1);
c        = steplengthParam(2);
rho      = steplengthParam(3);
alpha    = alphamax;
funcX    = func(x);
gradX    = grad(x);
while func(x + alpha * p) > funcX + c * alpha * gradX' * p;
   alpha = rho*alpha; 
end

end


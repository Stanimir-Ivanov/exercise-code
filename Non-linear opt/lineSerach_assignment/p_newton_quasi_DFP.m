function [ p, H ] = p_newton_quasi_DFP( func, grad, hes, x, prevX, prevH, directionParam )
%P_NEWTON_QUASI_DFP
% func              - Function (not used).
% grad              - Gradient.
% hes               - Hessian function (not used).
% x                 - Current iterate.
% prevX             - Previous iterate.
% prevH             - Previous approximated INVERSE hessian.
% directionParam    - Additional parameters for the algorithm (not used).

if isempty(prevX) %first iteration, see lineSearch
    H = prevH; %use supplied initial H
else
    s = x - prevX;
    y = grad(x) - grad(prevX);
    H = prevH - (prevH * (y * y') * prevH)/(y' * prevH * y) + (s * s')/(y' * s); %use (6.15) to update inverse Hessian directly
end    
p = -H*grad(x);

end


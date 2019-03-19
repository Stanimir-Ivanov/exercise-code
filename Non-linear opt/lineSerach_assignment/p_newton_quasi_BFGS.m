function [ p, H ] = p_newton_quasi_BFGS( func, grad, hes, x, prevX, prevH, directionParam )
%P_NEWTON_QUASI_BFGS
% func              - Function (not used).
% grad              - Gradient.
% hes               - Hessian function (not used).
% x                 - Current iterate.
% prevX             - Previous iterate.
% prevH             - Previous approximated INVERSE hessian.
% directionParam    - Additional parameters for the algorithm (not used).

if isempty(prevX) %first iteration, see lineSearch
    H   = prevH; %use supplied initial H
else
    I   = eye(length(x));
    s   = x - prevX;
    y   = grad(x) - grad(prevX);
    rho = 1/(y'*s);
    H   = (I - rho * (s * y')) * prevH * (I - rho * (y * s')) + rho * (s * s');
end
    
p = -H*grad(x);

end


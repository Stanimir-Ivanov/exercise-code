function [ p,H ] = p_newton(func, grad, hes, x, prevX, prevH, directionParam)
%P_NEWTON 
% func              - Function (not used).
% grad              - Gradient.
% hes               - Hessian function.
% x                 - Current iterate.
% prevX             - Previous iterate (not used).
% prevH             - Previous approximated INVERSE hessian (not used).
% directionParam    - Additional parameters for the algorithm (not used).

p = -hes(x)\grad(x); %efficient MATLAB equivalent to p=-inv(hes(x))*grad(x)
H = [];

end


function [ p, H ] = p_newton(func, grad, hes, x, prevX, prevH, directionParam)
%P_NEWTON Newton's method direction
% func              - Function (not used).
% grad              - Gradient.
% hes               - Hessian function.
% x                 - Current iterate (not used).
% prevX             - Previous iterate (not used).
% prevH             - Previous approximated INVERSE hessian (not used).
% directionParam    - Additional parameters for the algorithm (not used).
%
% OUTPUT
% p                 - Steepest descent direction.
% H                 - New approximation of the INVERSE hessian (not used).

p = -inv(hes(x))*grad(x); %steepest descent direction.
H = []; %no use of approximated hessians in this method.

end


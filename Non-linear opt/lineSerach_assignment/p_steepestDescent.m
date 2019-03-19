function [ p, H ] = p_steepestDescent(func, grad, hes, x, prevX, prevH, directionParam)
%P_STEEPESTDESCENT Steepest descent direction
% func              - Function (not used).
% grad              - Gradient.
% hes               - Hessian function (not used).
% x                 - Current iterate (not used).
% prevX             - Previous iterate (not used).
% prevH             - Previous approximated INVERSE hessian (not used).
% directionParam    - Additional parameters for the algorithm (not used).
%
% OUTPUT
% p                 - Steepest descent direction.
% H                 - New approximation of the INVERSE hessian (not used).

p = -grad(x); %steepest descent direction.
H = []; %no use of approximated hessians in this method.

end


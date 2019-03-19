function [ alpha ] = alpha_constant(func, grad, x,p,steplengthParam)
%ALPHA_CONSTANT Constant step length.
% func              - Function (not used).
% grad              - Gradient (not used).
% x                 - Current iterate (not used).
% p                 - Search direction (not used).
% steplengthParam   - Additional parameters for the algorithm, steplengthParam(1) used as constant step length.

alpha = steplengthParam(1);

end


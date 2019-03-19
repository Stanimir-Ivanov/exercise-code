function [ result ] = lineSearch( func, grad, hes, x0, B0, iterationLimit, epsilon, directionMethod, directionParam, steplengthMethod, steplengthParam )
%LINESEARCH Perform Line Search Method to minimize a given function.
%
%   INPUT:
%   func                - Function to be minimized:
%                           should take vector x as input, and return a value.
%   grad                - Gradient of the function to be minimized:
%                           should take vector x as input, and return the search direction and a new hessian approximation (if applicable).
%   hes*                - Hessian of the function to be optimized.
%   x0                  - Initial iterate.
%   B0*                 - Initial approximated hessian (positive definite matrix).
%   iterationLimit      - Maximum number of line search iterations.
%   epsilon             - Stop if ||grad(f(x_k))|| <= epsilon.
%   directionMethod     - Algorithm to calculate p. Should take parameters (func,grad,hes,x,prevX,prevH,directionParam) and output the search direction p and approximated INVERSE hessian H (as second output).
%                           % func              - See above.
%                           % grad              - See above.
%                           % hes*              - See above.
%                           % x                 - Current iterate.
%                           % prevX*            - Previous iterate.
%                           % prevH*            - Previous approximated INVERSE hessian.
%                           % directionParam*   - Additional parameters for the algorithm.
%   directionParam*     - Parameters to be passed on to directionMathod.
%   steplengthMethod    - Algorithm to calculate alpha. Should take parameters (func,grad,x,p,steplengthParam) and output the step length alpha.
%                           % func              - See above.
%                           % grad              - See above.
%                           % x                 - Current iterate.
%                           % p                 - Search direction.
%                           % steplengthParam*  - Additional parameters for the algorithm.       
%   stepLengthParam*    - Parameters to be passed on to steplengthMethod.
% Asterisk * indicates that the parameter is optional, depending on which method is being used.
%
%   OUTPUT:
%   ***** TO BE DEFINED! *****
k = 1;
result = [];
x = x0;
prevX = [];
H = inv(B0);
while(norm(grad(x)) > epsilon && k <= iterationLimit)    
    [p, H] = directionMethod(func, grad, hes, x, prevX, H, directionParam);
    alpha = steplengthMethod(func, grad, x, p, steplengthParam);
    prevX = x;
    x = x + alpha*p;    
    result = [result, x];
    k = k + 1;
end
end

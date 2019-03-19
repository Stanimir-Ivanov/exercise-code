function [ result ] = lineSearch( func, grad, hes, x0, B0, iterationLimit, epsilon, directionMethod, directionParam, steplengthMethod, steplengthParam )
%LINESEARCH Perform Line Search Method to minimize a given function.
%
%   INPUT:
%   func                - Function to be minimized.
%   grad                - Gradient of the function to be minimized.
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
%   result              - Matrix of length(x0) by number of iterates, result(:,k) represents x_k.

result   = zeros(length(x0),iterationLimit); % Initialize result matrix
H        = inv(B0);
prevX    = x0;
x        = x0;
gradNorm = epsilon + 1;      % Set initial gradNorm to epsilon + 1 to pass
                             % first while test.
k        = 1;                % Set iteration counter to 1
while k <= iterationLimit && gradNorm > epsilon
    if k == 1
        % First iteration, pass an empty matrix for prevX
        [p, H] = directionMethod(func, grad, hes, x0, [], H, directionParam); 
    else
        [p, H] = directionMethod(func, grad, hes, x, prevX, H, directionParam);
    end
    alpha       = steplengthMethod(func, grad, x, p, steplengthParam);
    prevX       = x;
    x           = x + alpha * p;
    gradNorm    = sqrt(sum(grad(x).^2));
    result(:,k) = x;
    k           = k + 1;
end

result = result(:,1:(k-1)); % k is increases at the end of the loop, so k-1 is last column

end

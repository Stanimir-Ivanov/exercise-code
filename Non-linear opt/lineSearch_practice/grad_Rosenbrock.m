function [ value ] = grad_Rosenbrock( x )
%GRAD_ROSENBROCK Implementation of the gradient of the Rosenbrock function  f(x) = 100*(x_2 - x_1^2)^2 + (1 - x_1)^2
%   x(1)    - x_1
%   x(2)    - x_2

value = [-400*x(1)*(x(2) - x(1)^2) - 2*(1 - x(1)); 200*(x(2) - x(1)^2)];

end
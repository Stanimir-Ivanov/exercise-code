function [ value ] = func_Rosenbrock( x )
%FUNC_ROSENBROCK Implementation of the Rosenbrock function f(x) = 100*(x_2 - x_1^2)^2 + (1 - x_1)^2
%   x(1)    - x_1
%   x(2)    - x_2

value = 100*(x(2) - x(1)^2)^2 + (1 - x(1))^2;
end
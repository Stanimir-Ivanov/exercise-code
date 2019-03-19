function [ value ] = hes_Rosenbrock( x )
%HES_ROSENBROCK Implementation of the hessian of the Rosenbrock function  f(x) = 100*(x_2 - x_1^2)^2 + (1 - x_1)^2
%   x(1)    - x_1
%   x(2)    - x_2

value = [2 - 400*(x(2) - x(1)^2) + 800*x(1)^2, -400*x(1); -400*x(1), 200];
end
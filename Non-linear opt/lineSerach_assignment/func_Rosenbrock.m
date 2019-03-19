function [ result ] = func_Rosenbrock( x )
%FUNC_ROSENBROCK Rosenbrock function

result = 100*(x(2)-x(1)^2)^2 + (1-x(1))^2;

end


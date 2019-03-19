function [ result ] = grad_Rosenbrock( x )
%GRAD_ROSENBROCK gradient of Rosenbrock function

result    = zeros(2,1);
result(1) =   2*x(1) - 400*x(1)*(x(2) - x(1)^2) - 2;
result(2) = 200*x(2) - 200*x(1)^2;

end


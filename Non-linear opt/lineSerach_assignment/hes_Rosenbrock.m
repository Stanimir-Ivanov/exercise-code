function [ result ] = hes_Rosenbrock( x )
%HES_ROSENBROCK Hessian of Rosenbrock function

result = zeros(2,2);
result(1,1) = 1200*x(1)^2 - 400*x(2) + 2;
result(1,2) = -400*x(1);
result(2,1) = result(1,2);
result(2,2) = 200;

end


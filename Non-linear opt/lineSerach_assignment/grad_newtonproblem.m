function [ value ] = grad_newtonproblem( x )
%GRAD_NEWTONPROBLEM gradient belonging to question 4 in Practice set 2

value = [x(1)^3 - 2*x(1) + 2; 2*x(2)];

end


function [ value ] = hes_newtonproblem( x )
%HES_NEWTONPROBLEM Hessian belonging to question 4 in Practice set 2

 
 value = [3*x(1)^2 - 2, 0; 0, 2];
end


function [ value ] = grad_six( x )
%GRAD_SIX gradient belonging to question 6 of Practice set 2

value = [2*x(1)/sqrt(x(1)^2 + x(2)^2 + x(3)^2) + 1 + exp(x(1) + x(2)); ...
         2*x(2)/sqrt(x(1)^2 + x(2)^2 + x(3)^2) + 2 * x(2) + exp(x(1) + x(2)); ...
         2*x(3)/sqrt(x(1)^2 + x(2)^2 + x(3)^2) + exp(x(3)) + 1];

end


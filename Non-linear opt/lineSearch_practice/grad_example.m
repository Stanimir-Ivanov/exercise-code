function [ value ] = grad_example( x )
%GRAD_EXAMPLE Example of the gradient of the function f(x,y) = x_1^2 + x_2^2
%   x(1)    - x_1
%   x(2)    - x_2

value = [2*x(1); 2*x(2)];

end


function [ value ] = func_example( x )
%FUNC_EXAMPLE Example of the function f(x,y) = x_1^2 + x_2^2
%   x(1)    - x_1
%   x(2)    - x_2

value = x(1).^2 + x(2).^2;

end


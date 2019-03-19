function [ result ] = func_part1( x )
%FUNC_PART1 function of part 1 
result = ((3/2) - x(1) + x(1)*x(2))^2 + ((9/4) - x(1) + x(1)*x(2)^2)^2;
end
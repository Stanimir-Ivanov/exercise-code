function [ result ] = grad_part1( x )
%GRAD_PART1 function of part 1 
 result = [2*(((3/2) - x(1) + x(1)*x(2))*(x(2)-1) + ((9/4) - x(1) + x(1)*x(2)^2)*(x(2)^2 - 1));2*(x(1)*((3/2) - x(1) + x(1)*x(2)) + 2*x(1)*x(2)*((9/4) - x(1) + x(1)*x(2)^2))];
end
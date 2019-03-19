function [ value ] = func_six( x )
%FUNC_SIX function belonging to question 6 of Practice set 2

value = x(1) + x(2)^2 + x(3) + exp(x(1)+x(2)) + exp(x(3)) ...
        + 2 * sqrt(x(1)^2 + x(2)^2 + x(3)^2);

end


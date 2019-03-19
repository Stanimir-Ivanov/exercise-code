function [ value ] = func_seven( x )
%FUNC_SEVEN function belonging to question 7 of Practice set 2

value = 2 * sqrt(( 0 - x(1))^2 + ( 0 - x(2))^2) ...
    + 2.5 * sqrt(( 0 - x(1))^2 + (10 - x(2))^2) ...
    +       sqrt((12 - x(1))^2 + ( 2 - x(2))^2);

end


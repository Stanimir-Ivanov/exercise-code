function [ value ] = grad_seven( x )
%GRAD_SEVEN gradient belonging to question 7 of Practice set 2

value = [2 * (x(1)-0)/sqrt((0 - x(1))^2 + (0 - x(2))^2) + 2.5 * (x(1)-0)/sqrt((0 - x(1))^2 + (10 - x(2))^2) +  (x(1)-12)/sqrt((12 - x(1))^2 + (2 - x(2))^2),
    2 * (x(2)-0)/sqrt((0 - x(1))^2 + (0 - x(2))^2) + 2.5 * (x(2)-10)/sqrt((0 - x(1))^2 + (10 - x(2))^2) +  (x(2)-2)/sqrt((12 - x(1))^2 + (2 - x(2))^2)];

end


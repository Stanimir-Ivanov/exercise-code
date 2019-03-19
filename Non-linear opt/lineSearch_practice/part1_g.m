function [value] = part1_g( x, x0 )
%part_1g function part1_g calculates the majorizing quadratic function of the Hinge function: max(x,0)
    value = (x.^2)/(4*x0) + (x/2) + (x0/4);
end
function [ result ] = func_part2( x )
%FUNC_PART2 function of part 2 
if (x < 3)
    result = 3*sqrt(3 - x) + 1;
elseif (x >= 3 && x <= 5)
    result = (x - 3)^2 + 1;
else 
    result = (3/2)*log(x - 4) + 5;
end
end
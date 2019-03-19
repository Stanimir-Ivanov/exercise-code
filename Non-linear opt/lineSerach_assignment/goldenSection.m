function [result] = goldenSection(func, x1, x2, epsilon, iterationLimit)
%GOLDENSECTION implements the golden section algorithm
% define golden ratio constant
phi = (1 + sqrt(5))/2;
% define initial x3 and x4 with corresponding f(x)
x3 = x2 - (1/phi)*(x2 - x1);
f3 = func(x3);
x4 = x1 + (1/phi)*(x2 - x1);
f4 = func(x4);
iter = 1;
result = [x1, x2, x3, x4]';
while(abs(x3 - x4) >= epsilon && iter < iterationLimit)
    % if the left function value is smaller, move the right boundry to the left    
    if (f3 < f4)
        x2 = x4;
        x4 = x3;
        f4 = f3;
        x3 = x2 - (1/phi)*(x2 - x1);
        f3 = func(x3);
    % conversly if the oposite is true
    else
        x1 = x3;
        x3 = x4;
        f3 = f4;
        x4 = x1 + (1/phi)*(x2 - x1);
        f4 = func(x4);
    end
    %update iteration count
    iter = iter + 1;
    %update result
    result = [result, [x1, x2, x3, x4]'];
end
end
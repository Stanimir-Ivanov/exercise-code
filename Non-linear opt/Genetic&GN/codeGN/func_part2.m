function [r, J] = func_part2(x)
%define the elements of r(x)
r1 = @(x) x(1)*(x(2)^2)*(x(3)^2) + exp(x(1) + x(2)) + x(1);
r2 = @(x) (x(1)^2)*(x(2)^2)*x(3) + exp(x(3)) + x(3);
%define the elmeents of J(x), the Jacobian of r(x)
J11 = @(x) (x(2)^2)*(x(3)^2) + exp(x(1) + x(2)) + 1;
J12 = @(x) 2*x(1)*x(2)*(x(3)^2) + exp(x(1) + x(2));
J13 = @(x) 2*x(1)*(x(2)^2)*x(3);
J21 = @(x) 2*x(1)*(x(2)^2)*x(3);
J22 = @(x) 2*(x(1)^2)*x(2)*x(3);
J23 = @(x) (x(1)^2)*(x(2)^2) + exp(x(3)) + 1;
%define r(x) and J(x) through their elements
r = @(x) [r1(x); r2(x)];
J = @(x) [J11(x), J12(x), J13(x); J21(x), J22(x), J23(x)];
%evaluate r and J at x
r = r(x);
J = J(x);
end
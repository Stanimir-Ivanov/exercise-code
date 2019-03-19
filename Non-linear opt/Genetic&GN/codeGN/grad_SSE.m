function [ result ] = grad_SSE( x )
%GRAD_SSE Function that gives the gradient of func_SSE given parameters x.

% Make matrix t and vector y available in this function
global t
global y

%%% Calculate gradient of func_SSE here
J = @(x) [(1/2)*x(1) + t(:,1), (1/2)*x(2) + t(:,2)];
r = @(x) ((1/4)*(x(1)^2 + x(2)^2) + t*x - y);

result = J(x)'*r(x);

end


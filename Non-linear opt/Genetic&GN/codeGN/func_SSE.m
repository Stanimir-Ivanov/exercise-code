function [ result ] = func_SSE( x )
%FUNC_SSE Function that gives the sum squared errors/2 for given parameters x.

% Make matrix t and vector y available in this function
global t
global y

%%% Calculate SSE/2 here
result = (1/2)*sum(((1/4)*(x(1)^2 + x(2)^2) + t*x - y).^2);

end
function [ result ] = hes_SSE( x )
%HES_SSE Function that gives the APPROXIMATED HESSIAN of func_SSE for given parameters x.

% Make matrix t and vector y available in this function
global t
global y

%%% Calculate APPROXIMATED HESSIAN of func_SSE here
J = @(x) [(1/2)*x(1) + t(:,1), (1/2)*x(2) + t(:,2)];
result = J(x)'*J(x);
end


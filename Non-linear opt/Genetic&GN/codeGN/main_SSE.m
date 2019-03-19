% Make matrix t and vector y available for all functions
global t
global y
load Student_Performance_Data_Set
t = [G1 G2];
y = G3;
iterationLimit = 100;
epsilon = 1e-4;
x0 = [0;0];
B0 = inv(hes_SSE(x0));
directionParam = [];
steplengthParam = [1];
%%% BELOW THIS LINE: perform Gauss-Newton by doing a linesearch with Newton
%%% directions and constant step length 1, using func_SSE, grad_SSE and hes_SSE.
%%% Store the result in a matrix "result", i.e. result = lineSearch(...)
result = lineSearch( @func_SSE, @grad_SSE, @hes_SSE, x0, B0, iterationLimit, epsilon, @p_newton, directionParam, @alpha_constant, steplengthParam );

% main function, solve r(x) = 0 through algorithm 11.1
% start at x = 0
x = [0; 0; 0];
for k = 1:100
%evaluate r and J at x
[r, J] = func_part2(x);
%SVD for J
[U, D, V] = svd(J, 'econ');
%calculate p via Newton's method for non-linear equations
p = -V*inv(D)*U'*r;
% new x
x = x + p;
end
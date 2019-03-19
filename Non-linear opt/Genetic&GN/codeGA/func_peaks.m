function z = func_peaks(X, f_param)
% This is the standard function of the MatLab logo (see peaks documention)
x = X(:, 1);
y = X(:, 2);
z =  3*(1 - x).^2.*exp(-(x.^2) - (y+1).^2) ... 
   - 10*(x/5 - x.^3 - y.^5).*exp(-x.^2 - y.^2) ... 
   - 1/3*exp(-(x + 1).^2 - y.^2); 
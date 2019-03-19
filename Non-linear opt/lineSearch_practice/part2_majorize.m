function [ result ] = part2_majorize( x, epsilon, iterationLimit )
%part2_majorize
    N = @(x) -x'*x -1; 
    D = @(x) 1 - sum(x) + sum(abs(x)).^2;
    f = @(x) N(x)/D(x);
    Binv = @(x) diag(abs(x)./sum(abs(x)));
    Gfosc = @(x) Binv(x)*((1/2)*ones([length(x),1]) - (1/f(x))*x);    
    k = 1;    
    while k == 1 || (k <= iterationLimit && abs(f(x) - f(x0)) > epsilon)                               
        x0 = x;
        x = Gfosc(x0);
        result(:,k) = x;        
        k = k + 1;
    end
end
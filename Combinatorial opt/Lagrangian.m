function [obj_lagrange, x_lagrange] = Lagrangian(c, A, lambda)
    % use n as shorthand for problem dim
    [~, n] = size(A);
    % set all xs to zeroes for simplicity
    x_lagrange = zeros([n, 1]);
    % only set an x as 1 if it meets the criteria unambigiously
    x_lagrange(c - A'*lambda < 0) = 1;
    % evaluate objective
    obj_lagrange = sum(lambda) + sum(min([(c - A'*lambda) (zeros([n, 1]))], [], 2));    
end
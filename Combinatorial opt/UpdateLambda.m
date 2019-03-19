function lambda_next = UpdateLambda(A, lambda, LB, UB, x_lagrange, rho)
    % get the size of the problem
    [m,~] = size(A);
    % evaluate mu
    mu = rho * (UB - LB)/sum((ones([m,1]) - A*x_lagrange).^2);
    % evaluate next lambda
    lambda_next = lambda - mu*(A*x_lagrange - ones([m,1]));
end
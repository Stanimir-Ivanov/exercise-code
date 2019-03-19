function [LB_best, UB_best, x_best, LB_list, UB_list] = SubgradientOpt(c, A, lambda_init, rho_init, k)
    % variable initialization 
    LB_best = -Inf;
    UB_best = +Inf;
    LB_list = zeros([k,1]);
    UB_list = zeros([k,1]);
    lambda = lambda_init;
    % begin iterations   
    for iter = 1:k
    	% ensure every element of lambda is non-negative by setting negative elements to 0
    	lambda = max([lambda zeros([length(lambda_init),1])], [], 2);
    	% obtain objective value and solution from a Lagrangian relaxation with current lambda
        [obj_lagrange, x_lagrange] = Lagrangian(c, A, lambda);
        % obtain feasible solution from Lagrangian solution by using greedy heuristic
        [obj_feas, x_feas] = InfeasToFeas(c, A, x_lagrange);     
        % check whether current feasable solution is better than the best found so far and update if so 
        if obj_feas < UB_best
            x_best = x_feas;
        end
        % check whether a new best LB or UB has been obtained
        LB_best = max([obj_lagrange LB_best]);
        UB_best = min([obj_feas UB_best]);
        % update LB and UB list for current iteration
        LB_list(iter) = obj_lagrange;
        UB_list(iter) = obj_feas;
        % calculate the lambda used for the next iteration
        lambda = UpdateLambda(A, lambda, obj_lagrange, UB_best, x_lagrange, rho_init/iter);        
    end
end

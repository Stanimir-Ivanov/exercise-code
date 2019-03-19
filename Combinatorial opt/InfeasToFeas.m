function [obj_feas, x_feas] = InfeasToFeas(c, A, x_infeas)
    % get problem size
    [m, ~] = size(A);
    % set of covered customers initialized to none
    covered = false([m,1]);
    % a customer is covered if at least one route visits him
    covered(A*x_infeas >= 1) = true;
    % feas solution contains at least as many routes as infeas
    x_feas = x_infeas;
    % remove selected routes from further consideration
    A(covered,:) = 0;
    while(sum(covered) < m)
        % find the next most economically efficient route
        [~,index] = max(sum(A)./c');
        % add it to the feas solution
        x_feas(index) = 1;
        covered(A*x_feas >= 1) = true;
        % remove selected routes from further consideration
        A(covered,:) = 0;
    end
    % evaluate feasible objective
    obj_feas = x_feas'*c;
end
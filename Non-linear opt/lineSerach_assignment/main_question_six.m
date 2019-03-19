x0             = [0.01; 0; 0];
iterationLimit = 1000;
epsilon        = 1E-4;

alphamax = 1;
c        = 1E-4;
rho      = 0.9;
backtrackingParam = [alphamax, c, rho];

B0 = eye(3);

result = lineSearch(    @func_six,...
                        @grad_six,...
                        [],...                   % Hessian not needed for BFGS.
                        x0,...
                        B0,...                   % Initial hessian  needed.
                        iterationLimit,...
                        epsilon,...
                        @p_newton_quasi_BFGS,... % Directions by BFGS.
                        [],...                   % No parameters necessary (see p_BFGS).
                        @alpha_backtracking,...  % Backtracking steplength.
                        backtrackingParam);      % See alpha_backtracking.

display(result);

result(:,end)
funcValues(end)
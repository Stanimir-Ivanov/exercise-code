x0             = [0; 1];
iterationLimit = 1000;
epsilon        = 1E-4;

alphamax = 1;
c        = 1E-4;
rho      = 0.9;
backtrackingParam = [alphamax, c, rho];
B0       = eye(2);

result_BFGS = lineSearch(@func_Rosenbrock,...
                        @grad_Rosenbrock,...
                        [],...                  %No Hessian is needed for BFGS.
                        x0,...
                        B0,...                  %Initial approximated hessian not needed.
                        iterationLimit,...
                        epsilon,...
                        @p_newton_quasi_BFGS,...%Directions by BFGS.
                        [],...                  %No parameters necessary.
                        @alpha_backtracking,... %Back tracking steplength.
                        backtrackingParam);     %See alpha_constant.


result_DFP = lineSearch(@func_Rosenbrock,...
                        @grad_Rosenbrock,...
                        [],...                  %No Hessian is needed for DFP.
                        x0,...
                        B0,...                  %Initial approximated hessian not needed.
                        iterationLimit,...
                        epsilon,...
                        @p_newton_quasi_DFP,... %Directions by DFP.
                        [],...                  %No parameters necessary.
                        @alpha_backtracking,... %Back tracking steplength.
                        backtrackingParam);     %See alpha_constant.


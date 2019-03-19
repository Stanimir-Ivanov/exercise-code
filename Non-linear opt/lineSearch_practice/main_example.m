%Line Search setup
x0 = [1; 1];            %starting point
iterationLimit = 1000;  %maximum number of iterations
epsilon = 1E-4;         %epsilon for stopping condition

%Prepare parameters for steepest descent
directionParam = []; %currently zero parameters, but can be a vector

%Prepare parameters for constant step length
constantAlpha = .1;    
steplengthParam = [constantAlpha]; %currently one parameter, but can be a vector

%Perform a steepest descent line search with constant step lengths
%See lineSearch.m for the definition of all the parameters of the lineSearch function!
result = lineSearch(    @func_example,...       %Minimize func_example, defined in func_example.m (@ refers to a function).
                        @grad_example,...       %Corresponding gradient function, defined in grad_example.m.
                        [],...                  %Hessian not needed for steepest descent.
                        x0,...
                        [],...                  %Initial approximated hessian not needed.
                        iterationLimit,...
                        epsilon,...
                        @p_steepestDescent,...  %Directions by steepest descent method (note again the @, so we pass a function).
                        directionParam,...      %Parameters for steepest descent, prepared above. See also p_steepestDescent.m.
                        @alpha_constant,...     %Constant steplength method.
                        steplengthParam);       %Parameters for constant step length method, prepared above. See also alpha_constant.m.

%Result displays all the iterations of x
display(result);
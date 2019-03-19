%Line Search setup
x0 = [1; 1];            %starting point
iterationLimit = 1000;  %maximum number of iterations
epsilon = 1E-4;         %epsilon for stopping condition

%Prepare parameters for steepest descent
directionParam = []; %currently zero parameters, but can be a vector

%Prepare parameters for constant step length
constantAlpha  = 0.1;
% constantAlpha = 1; %for 1b
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

% CODE FOR PLOTTING %

% Define domain
x1 = linspace(-5, 5, 30);
x2 = linspace(-5, 5, 30);

% Convert to grid matrices
[X1, X2] = meshgrid(x1,x2);

% Define Z such that Z(i,j) = f(X(i,j),Y(i,j))
Z = zeros(size(X1));
for i = 1:size(X1,1)
    for j = 1:size(X1,2)
        Z(i,j) = func_example([X1(i,j);X2(i,j)]);
    end
end

% Create surface plot
surf(X1,X2,Z);
xlabel('x1');
ylabel('x2');
zlabel('f(x2,x2)');

% Function values of iterates
funcValues = zeros(1,size(result,2));
for i = 1:size(result,2)
    funcValues(i) = func_example(result(:,i));
end

% Add iterations
hold on %to work in same figure
plot3(result(1,:), result(2,:), funcValues,'-vr','MarkerSize',10,'MarkerFaceColor','r');

% I create a separate contour plot, so I can change for which z-value the
% contours appear. Note that I could have used surfc instead of surf
% earlier, which gives similar results.

% Create contour plot
[~,h] = contour(X1,X2,Z);

% Change elevation of contour plot
elevation = -5;
hh = get(h,'Children');
for i=1:numel(hh)
    set(hh(i),'ZData',elevation*ones(size(get(hh(i),'XData'))));
end
hold off

% End of contour plot.

% Make current figure the active window
figure(gcf)

result(:,end) %display last iterate
funcValues(end) %display last function value